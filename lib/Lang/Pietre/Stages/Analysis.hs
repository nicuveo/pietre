{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens                           hiding (op)
import Control.Monad.RWS.Strict
import Data.HashMap.Strict                    qualified as M
import Data.HashSet                           qualified as S
import Data.List                              qualified as L
import Data.List.NonEmpty                     qualified as NE

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Monad


--------------------------------------------------------------------------------
-- Analysis

type Symbols = HashMap Identifier (WithLocation (Declaration Resolved))

analyzeModule
  :: HashMap ModuleName Symbols
  -> ModuleName
  -> Module
  -> ([Diagnostic], Maybe Symbols)
analyzeModule dependencies thisName this@Module {..} = runAnalysis thisName $ runMaybeT do
  foreignNames <- initImportedContext dependencies this
  localNames   <- initLocalNames _modDeclarations
  local (infoTopLevelNames %~ (combineMaps foreignNames . combineMaps localNames)) $ MaybeT do
    symbols <- for _modDeclarations \declaration -> runMaybeT do
      let identifier = declarationIdentifier $ _located declaration
          declName   = TopLevelDeclaration thisName identifier
      -- TODO: also export Enum values
      resolvedDeclaration <- analyzeDeclaration declName declaration
      pure (identifier, WithLocation (_location declaration) resolvedDeclaration)
    pure $ M.fromList <$> sequence symbols
  where
    combineMaps = M.unionWith (<>)


--------------------------------------------------------------------------------
-- Internals

initImportedContext
  :: HashMap ModuleName Symbols
  -> Module
  -> MaybeT AnalysisM (HashMap Path (NonEmpty Name))
initImportedContext dependencies Module {..} = do
  -- for each imported module, we create a hashmap
  -- from module name to hashmap of path to non-empty list:
  -- the hashmap of imported paths, grouped by module
  knownSymbols :: [HashMap ModuleName (HashMap Path (NonEmpty (Name, WithLocation (Declaration Resolved))))] <-
    for _modImports \Import {..} -> do
      symbols <- handleMaybe (ErrorImportPath _importPath) $
        M.lookup _importPath dependencies
      let resolved = flip M.mapWithKey symbols \name symbol ->
            pure (TopLevelDeclaration _importPath name, symbol)
      M.singleton _importPath . M.fromListWith (<>) <$> case _importType of
        Qualified qualifiedName -> pure do
          (name, symbol) <- M.toList resolved
          (_importPath <> pure name, symbol) : do
            qualifier <- maybeToList qualifiedName
            pure (qualifier :| [name], symbol)
        Specific names -> concat <$> for names \name -> do
          symbol <- handleMaybe (ErrorImportSymbol _importPath name) $
            M.lookup name resolved
          pure
            [ (pure name, symbol)
            , (_importPath <> pure name, symbol)
            ]
        Exhaustive -> pure $ M.toList resolved >>= \(name, symbol) ->
          [ (pure name, symbol)
          , (_importPath <> pure name, symbol)
          ]

  -- we group the declarations per module, using (<>) on the hashmap:
  -- this discards duplicates within the same module, as the same
  -- module might appear more than once in the list of imports.
  -- we then concatenate the non-empty lists, which confusingly is also
  -- a union with (<>), but on the non-empty lists.
  -- the result is a hashmap from path to grouped non-empty list of
  -- possible matches across modules
  let importedScope :: HashMap Path (NonEmpty (Name, WithLocation (Declaration Resolved))) =
        foldl' (M.unionWith (<>)) M.empty $ M.elems $
        foldl' (M.unionWith (<>)) M.empty $ knownSymbols

  contextDeclarations <>= M.fromList (concatMap toList $ M.elems importedScope)
  pure (M.map (fmap fst) importedScope)

initLocalNames
  :: [Annotated Declaration Parsed]
  -> MaybeT AnalysisM (HashMap Path (NonEmpty Name))
initLocalNames declarations = do
  moduleName <- view infoModuleName

  -- gather all top level names
  -- group them by identifier
  let topLevelNames = M.fromListWith combineLocations do
        decl <- declarations
        let identifier = declarationIdentifier $ _located decl
            declName = TopLevelDeclaration moduleName identifier
        case _located decl of
              EnumDecl enumInfo ->
                (identifier, (declName, pure decl)) : do
                  enumValue <- _enumValues enumInfo
                  pure (enumValue, (declName, pure decl))
              _ -> pure (identifier, (declName, pure decl))

  -- report an error if any identifier appears more than once
  failed <- or <$> for (M.toList topLevelNames) \(identifier, (_, annotatedDeclarations)) -> do
    let hasDuplicates = NE.length annotatedDeclarations > 1
    when hasDuplicates $
      tell [ErrorMultipleDeclaration identifier $ fmap _location annotatedDeclarations]
    pure hasDuplicates
  when failed mzero

  -- add top-level names to the context
  contextLocals .= M.fromList do
    (_identifier, (name, declaration :| _)) <- M.toList topLevelNames
    pure (name, declaration)
  pure $ M.foldlWithKey' (makePaths moduleName) M.empty topLevelNames

  where
    combineLocations (name1, locations1) (_name2, locations2) =
      (name1, locations1 <> locations2)

    makePaths moduleName accum identifier (name, _) =
        accum
          & M.insert (pure identifier) (pure name)
          & M.insert (moduleName <> pure identifier) (pure name)


reportFatal
  :: MonadWriter [Diagnostic] m
  => Diagnostic
  -> MaybeT m a
reportFatal = (>> mzero) . tell . pure


handleMaybe
  :: MonadWriter [Diagnostic] m
  => Diagnostic
  -> Maybe a
  -> MaybeT m a
handleMaybe diagnostic = \case
  Nothing -> reportFatal diagnostic
  Just x  -> pure x


--------------------------------------------------------------------------------
-- Type resolving

type ResolveCallback r
  =  forall (p :: ASTPhase)
  .  Analyzable p
  => PathInfo Resolved
  -> Maybe (WithLocation (Declaration p))
  -> MaybeT AnalysisM r

resolvePath
  :: ResolveCallback r
  -> PathInfo Parsed
  -> MaybeT AnalysisM r
resolvePath processDeclaration PathInfo {..} = do
  names@(name :| others) <- handleMaybe (ErrorNameNotFound _pathName) =<<
    views infoNames (M.lookup _pathName)
  when (not $ null others) $
    reportFatal $ ErrorAmbiguousPath _pathName names
  params <- traverse resolveType _pathParams
  resolveName processDeclaration $ PathInfo name params

resolveName
  :: ResolveCallback r
  -> PathInfo Resolved
  -> MaybeT AnalysisM r
resolveName processDeclaration resolvedPathInfo =
  uses contextDeclarations (M.lookup name) >>= \case
    Just decl -> processDeclaration resolvedPathInfo (Just decl)
    Nothing   -> uses contextLocals (M.lookup name) >>= processDeclaration resolvedPathInfo
  where
    name = _pathName resolvedPathInfo

resolveConstValue
  :: PathInfo Parsed
  -> MaybeT AnalysisM TypedExpression
resolveConstValue pathInfo = resolvePath go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} = \case
      Nothing   -> reportFatal undefined
      Just decl -> case _located decl of
        TypeAliasDecl _ -> reportFatal $ ErrorNotAConst originalPath _pathName
        FunctionDecl  _ -> reportFatal $ ErrorNotAConst originalPath _pathName
        StructDecl    _ -> reportFatal $ ErrorNotAConst originalPath _pathName
        EnumDecl      enumInfo -> do
          let identifier = NE.last originalPath
          if identifier == _enumName enumInfo
            then reportFatal $ ErrorNotAConst originalPath _pathName
            else
              case L.elemIndex identifier (_enumValues enumInfo) of
                Nothing -> error "ICE"
                Just i  -> pure $ TypedExpression resolvedPath $ IntLiteralExpr i
        ConstDecl     _ ->
          resolveDeclaration _pathName decl <&> \case
            ConstDecl cInfo -> _constExpr cInfo
            _ -> error "ICE"

resolveType
  :: PathInfo Parsed
  -> MaybeT AnalysisM (PathInfo Resolved)
resolveType pathInfo = resolvePath go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback (PathInfo Resolved)
    go path@PathInfo {..} = \case
      Nothing   -> pure path
      Just decl -> case _located decl of
        TypeAliasDecl info -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            TypeAliasDecl taInfo -> taInfo
            _ -> error "ICE"
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportFatal $ ErrorIncorrectTypeParameterCount _pathName expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          pure resultPathInfo
        ConstDecl     _ -> reportFatal (ErrorNotAType originalPath _pathName)
        FunctionDecl  _ -> reportFatal (ErrorNotAType originalPath _pathName)
        EnumDecl      _ -> do
          when (not $ null _pathParams) $
            reportFatal $ ErrorIncorrectTypeParameterCount _pathName 0 (length _pathParams)
          pure path
        StructDecl info -> do
          let expected = length (_structParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportFatal $ ErrorIncorrectTypeParameterCount _pathName expected actual
          pure path

resolveStruct
  :: PathInfo Parsed
  -> MaybeT AnalysisM (PathInfo Resolved, StructInfo Resolved)
resolveStruct pathInfo = resolvePath go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback (PathInfo Resolved, StructInfo Resolved)
    go path@PathInfo {..} = \case
      Nothing   -> reportFatal $ ErrorNotAStruct originalPath _pathName
      Just decl -> case _located decl of
        ConstDecl     _ -> reportFatal $ ErrorNotAStruct originalPath _pathName
        FunctionDecl  _ -> reportFatal $ ErrorNotAStruct originalPath _pathName
        EnumDecl      _ -> reportFatal $ ErrorNotAStruct originalPath _pathName
        TypeAliasDecl info -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            TypeAliasDecl taInfo -> taInfo
            _ -> error "ICE"
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportFatal $ ErrorIncorrectTypeParameterCount _pathName expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          resolveName go resultPathInfo
        StructDecl _ -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            StructDecl sInfo -> sInfo
            _ -> error "ICE"
          let expected = length (_structParams resolvedInfo)
              actual   = length _pathParams
          when (expected /= actual) $
            reportFatal $ ErrorIncorrectTypeParameterCount _pathName expected actual
          pure (path, resolvedInfo)

tryResolveEnumFromName
  :: PathInfo Resolved
  -> MaybeT AnalysisM (Maybe EnumInfo)
tryResolveEnumFromName = resolveName go
  where
    go :: ResolveCallback (Maybe EnumInfo)
    go _ decl = pure do
      EnumDecl enumInfo <- fmap _located decl
      pure enumInfo

substituteTypes
  :: HashMap Identifier (PathInfo Resolved)
  -> PathInfo Resolved
  -> MaybeT AnalysisM (PathInfo Resolved)
substituteTypes mappings info@PathInfo {..} = case _pathName of
  TypeParameter name -> M.lookup name mappings `onNothing` error "ICE"
  _                  -> pathParams (traverse $ substituteTypes mappings) info


--------------------------------------------------------------------------------
-- Analysis

class Analyzable p where
  resolveDeclaration :: Name -> WithLocation (Declaration p) -> MaybeT AnalysisM (Declaration Resolved)

instance Analyzable Parsed where
  resolveDeclaration = analyzeDeclaration

instance Analyzable Resolved where
  resolveDeclaration _ = pure . _located


analyzeDeclaration
  :: Name
  -> WithLocation (Declaration Parsed)
  -> MaybeT AnalysisM (Declaration Resolved)
analyzeDeclaration thisName declaration = do
  uses contextDeclarations (M.lookup thisName) >>= \case
    Just decl -> pure $ _located decl
    Nothing -> local setScope do
      declCycle <- views infoStack (S.member thisName)
      if declCycle
      then reportFatal (ErrorCyclicDefinition thisName)
      else local (infoStack %~ S.insert thisName) do
        resolvedDeclaration <- case _located declaration of
          TypeAliasDecl info -> TypeAliasDecl <$> analyzeTypeAlias thisName info
          StructDecl    info -> StructDecl    <$> analyzeStruct    thisName info
          EnumDecl      info -> EnumDecl      <$> analyzeEnum      thisName info
          ConstDecl     info -> ConstDecl     <$> analyzeConst     thisName info
          FunctionDecl  info -> FunctionDecl  <$> analyzeFunction  thisName info
        contextDeclarations %= M.insert thisName (WithLocation declLocation resolvedDeclaration)
        pure resolvedDeclaration
  where
    declLocation = _location declaration
    setScope info@AnalysisInfo{..} = info
      { _infoLocation = declLocation
      , _infoNames    = _infoTopLevelNames
      }

analyzeTypeAlias
  :: Name
  -> TypeAliasInfo Parsed
  -> MaybeT AnalysisM (TypeAliasInfo Resolved)
analyzeTypeAlias _thisName TypeAliasInfo {..} = do
  let typeNames = M.fromList do
        identifier <- _aliasParams
        pure (pure identifier, pure (TypeParameter identifier))
  -- TODO: emit warning if type name shadows existing type
  local (infoNames %~ M.union typeNames) do
    resolved <- resolveType _aliasValue
    pure $ TypeAliasInfo _aliasName _aliasParams resolved

analyzeStruct
  :: Name
  -> StructInfo Parsed
  -> MaybeT AnalysisM (StructInfo Resolved)
analyzeStruct _thisName info = do
  let typeNames = M.fromList do
        identifier <- _structParams info
        pure (pure identifier, pure (TypeParameter identifier))
  -- TODO: emit warning if type name shadows existing type
  local (infoNames %~ M.union typeNames) $
    structValues (traverse (traverse resolveType)) info

analyzeEnum
  :: Name
  -> EnumInfo
  -> MaybeT AnalysisM EnumInfo
analyzeEnum thisName info = do
  let names = group $ sort $ _enumValues info
  failed <- or <$> for names \case
    []       -> error "ICE"
    [_]      -> pure False
    (name:_) -> do
      tell [ErrorEnumDuplicateEntries thisName name]
      pure True
  when failed mzero
  pure info

typeMatches
  :: PathInfo Resolved
  -> PathInfo Resolved
  -> Bool
typeMatches p1 p2 = case (_pathName p1, _pathName p2) of
  (TypeParameter _, _) -> True
  (_, TypeParameter _) -> True
  (TopLevelDeclaration m1 i1, TopLevelDeclaration m2 i2) ->
    m1 == m2 && i1 == i2 && argumentsMatch
  (BuiltinType t1, BuiltinType t2) ->
    t1 == t2 && argumentsMatch
  _ -> False
  where
    argumentsMatch = and $ zipWith typeMatches (_pathParams p1) (_pathParams p2)


analyzeConst
  :: Name
  -> ConstInfo Parsed
  -> MaybeT AnalysisM (ConstInfo Resolved)
analyzeConst _thisName ConstInfo {..} = do
  resolvedType <- resolveType _constType
  resolvedExpr <- go _constExpr
  unless (resolvedType `typeMatches` _exprType resolvedExpr) $
    reportFatal $ ErrorWrongType [resolvedType] (_exprType resolvedExpr)
  pure $ ConstInfo _constName resolvedType resolvedExpr
  where
    binaryIntExpression
      :: (Int -> Int -> Int)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryIntExpression f e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      case r1 of
        IntLiteralExpr x -> case r2 of
          IntLiteralExpr y -> pure $ IntExpression (f x y)
          _                -> reportFatal $ ErrorWrongType [IntType] t2
        _ -> reportFatal $ ErrorWrongType [IntType] t1

    binaryBoolExpression
      :: (Bool -> Bool -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryBoolExpression f e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      case r1 of
        BoolLiteralExpr x -> case r2 of
          BoolLiteralExpr y -> pure $ BoolExpression (f x y)
          _                 -> reportFatal $ ErrorWrongType [BoolType] t2
        _ -> reportFatal $ ErrorWrongType [BoolType] t1

    comparisonExpression
      :: (Expression Resolved -> Expression Resolved -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    comparisonExpression op e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      when (t1 /= t2) $
        reportFatal $ ErrorWrongType [t1] t2
      pure $ BoolExpression $ compareExpressions op r1 r2

    compareExpressions
      :: (Expression Resolved -> Expression Resolved -> Bool)
      -> Expression Resolved
      -> Expression Resolved
      -> Bool
    compareExpressions op e1 e2 = do
      case (e1, e2) of
        (StructExpr _ f1, StructExpr _ f2) ->
          let sortedF1 = map (_exprValue . snd) $ L.sortOn fst $ NE.toList f1
              sortedF2 = map (_exprValue . snd) $ L.sortOn fst $ NE.toList f2
          in all (uncurry $ compareExpressions op) $ zip sortedF1 sortedF2
        _ -> op e1 e2

    go
      :: WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    go WithLocation {..} =
      local (infoLocation .~ _location) $
        case _located of
          PathExpr p ->
            resolveConstValue p
          CastExpr e t  -> do
            resolvedExpr <- go e
            resolvedTargetType <- resolveType t
            let resolvedSourceType = _exprType resolvedExpr
                reportCastError :: forall a. MaybeT AnalysisM a
                reportCastError = reportFatal $ ErrorWrongCast resolvedSourceType resolvedTargetType
            targetEnum <- tryResolveEnumFromName resolvedTargetType
            sourceEnum <- tryResolveEnumFromName resolvedSourceType
            case (sourceEnum, targetEnum) of
              (Just _, Just destEnum) -> do
                intValue <- case _exprValue resolvedExpr of
                  IntLiteralExpr  i -> pure i
                  _                 -> error "ICE"
                when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                  reportFatal $ ErrorEnumOutOfBounds destEnum intValue
                pure $ TypedExpression resolvedTargetType $ IntLiteralExpr intValue
              (Nothing, Just destEnum) -> do
                intValue <- case _exprValue resolvedExpr of
                  IntLiteralExpr  i -> pure i
                  CharLiteralExpr c -> pure $ ord c
                  BoolLiteralExpr b -> pure $ fromEnum b
                  _                 -> reportCastError
                when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                  reportFatal $ ErrorEnumOutOfBounds destEnum intValue
                pure $ TypedExpression resolvedTargetType $ IntLiteralExpr intValue
              (Just _, Nothing) -> do
                intValue <- case _exprValue resolvedExpr of
                  IntLiteralExpr  i -> pure i
                  _                 -> error "ICE"
                case resolvedTargetType of
                  IntType  -> pure $ IntExpression intValue
                  CharType -> pure $ CharExpression $ chr intValue
                  BoolType -> pure $ BoolExpression $ toEnum intValue
                  _        -> reportCastError
              (Nothing, Nothing) -> do
                intValue <- case _exprValue resolvedExpr of
                  IntLiteralExpr  i -> pure i
                  CharLiteralExpr c -> pure $ ord c
                  BoolLiteralExpr b -> pure $ fromEnum b
                  _                 -> reportCastError
                case resolvedTargetType of
                  IntType  -> pure $ IntExpression intValue
                  CharType -> pure $ CharExpression $ chr intValue
                  BoolType -> pure $ BoolExpression $ toEnum intValue
                  _        -> reportCastError
          FieldAccessExpr expr field -> do
            TypedExpression structType structValue <- go expr
            case structValue of
              StructExpr _ fields -> do
                fmap snd $
                  find ((field ==) . fst) fields `onNothing`
                    reportFatal (ErrorFieldAccessFieldNotFound structType field)
              _                   -> reportFatal $ ErrorFieldAccessNotAStruct structType
          CallExpr _ _  -> reportFatal undefined
          ArrayExpr _ -> undefined
          IndexExpr _ _ -> undefined
          StructExpr path fields -> do
            (resolvedType, structInfo) <- resolveStruct path
            resolvedFields <- (traverse . traverse) go fields
            analyzeStructFields resolvedType (_structValues structInfo) resolvedFields
            pure $ TypedExpression resolvedType $ StructExpr resolvedType resolvedFields
          IntLiteralExpr    i -> pure $ IntExpression  i
          BoolLiteralExpr   b -> pure $ BoolExpression b
          CharLiteralExpr   c -> pure $ CharExpression c
          StringLiteralExpr s -> pure $ TypedExpression undefined $ StringLiteralExpr s
          ReferenceExpr _ -> reportFatal undefined
          BoolNegationExpr e -> go e >>= \case
            BoolExpression x -> pure $ BoolExpression (not x)
            TypedExpression t _ -> reportFatal $ ErrorWrongType [BoolType] t
          IntNegationExpr e -> go e >>= \case
            IntExpression x -> pure $ IntExpression (-x)
            TypedExpression t _ -> reportFatal $ ErrorWrongType [IntType] t
          AdditionExpr e1 e2 -> do
            TypedExpression t1 r1 <- go e1
            TypedExpression t2 r2 <- go e2
            case r1 of
              IntLiteralExpr x -> case r2 of
                IntLiteralExpr y -> pure $ IntExpression (x + y)
                _                -> reportFatal $ ErrorWrongType [IntType] t2
              _                -> reportFatal $ ErrorWrongType [IntType] t1
          SubtractionExpr    e1 e2 -> binaryIntExpression subtract e1 e2
          MultiplicationExpr e1 e2 -> binaryIntExpression (*) e1 e2
          DivisionExpr       e1 e2 -> binaryIntExpression div e1 e2
          ModuloExpr         e1 e2 -> binaryIntExpression mod e1 e2
          ExponentiationExpr e1 e2 -> binaryIntExpression (^) e1 e2
          EqualityExpr       e1 e2 -> comparisonExpression (==) e1 e2
          DifferenceExpr     e1 e2 -> comparisonExpression (/=) e1 e2
          GreaterExpr        e1 e2 -> comparisonExpression (>)  e1 e2
          LesserExpr         e1 e2 -> comparisonExpression (<)  e1 e2
          GreaterEqExpr      e1 e2 -> comparisonExpression (>=) e1 e2
          LesserEqExpr       e1 e2 -> comparisonExpression (<=) e1 e2
          BoolAndExpr        e1 e2 -> binaryBoolExpression (&&) e1 e2
          BoolOrExpr         e1 e2 -> binaryBoolExpression (||) e1 e2
          RangeInclusiveExpr           _ _ -> undefined
          RangeExclusiveExpr           _ _ -> undefined
          AssignmentExpr               _ _ -> reportFatal undefined
          AdditionAssignmentExpr       _ _ -> reportFatal undefined
          SubtractionAssignmentExpr    _ _ -> reportFatal undefined
          MultiplicationAssignmentExpr _ _ -> reportFatal undefined
          DivisionAssignmentExpr       _ _ -> reportFatal undefined
          ModuloAssignmentExpr         _ _ -> reportFatal undefined
          ExponentiationAssignmentExpr _ _ -> reportFatal undefined

analyzeStructFields
  :: PathInfo Resolved
  -> NonEmpty (Identifier, PathInfo Resolved)
  -> NonEmpty (Identifier, TypedExpression)
  -> MaybeT AnalysisM ()
analyzeStructFields typeName reference values = do
  let referenceMap = M.fromList $ NE.toList reference
      valuesMap    = M.fromListWith (<>) $ NE.toList $ (fmap . fmap) pure values
  for_ reference \(identifier, typePath) -> do
    case fold $ M.lookup identifier valuesMap of
      []  -> reportFatal $ ErrorStructMissingField typeName identifier
      [x] -> when (_exprType x /= typePath) $ reportFatal $ ErrorWrongType [typePath] (_exprType x)
      _   -> reportFatal $ ErrorStructDuplicatedField typeName identifier
  for_ values \(identifier, _) -> do
    when (not $ M.member identifier referenceMap) $
      reportFatal $ ErrorStructUnknownField typeName identifier

analyzeFunction
  :: Name
  -> FunctionInfo Parsed
  -> MaybeT AnalysisM (FunctionInfo Resolved)
analyzeFunction _thisName FunctionInfo {..} = do
  resolvedType <- traverse resolveType _funType
  pure $ FunctionInfo _funName _funParams [] resolvedType []


--------------------------------------------------------------------------------
-- Local helpers

pattern IntExpression :: Int -> TypedExpression
pattern IntExpression x = TypedExpression IntType (IntLiteralExpr x)

pattern CharExpression :: Char -> TypedExpression
pattern CharExpression x = TypedExpression CharType (CharLiteralExpr x)

pattern BoolExpression :: Bool -> TypedExpression
pattern BoolExpression x = TypedExpression BoolType (BoolLiteralExpr x)
