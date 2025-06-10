{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens                           hiding (mapping, op)
import Control.Monad.Extra                    (unlessM, whenJustM)
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
  (topLevelNames, locals) <- initLocalNames _modDeclarations
  let initReaderContext =
        (infoLocals .~ locals) .
        (infoTopLevelNames %~ (combineMaps foreignNames . combineMaps topLevelNames))
  -- TODO: do a runReaderT here instead
  local initReaderContext $ MaybeT do
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
  -> MaybeT AnalysisM
     ( HashMap Path (NonEmpty Name)
     , HashMap Name (WithLocation (Declaration Parsed))
     )
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

  let locals = M.fromList do
        (_identifier, (name, declaration :| _)) <- M.toList topLevelNames
        pure (name, declaration)
  let localNames = M.foldlWithKey' (makePaths moduleName) M.empty topLevelNames
  pure (localNames, locals)

  where
    combineLocations (name1, locations1) (_name2, locations2) =
      (name1, locations1 <> locations2)

    makePaths moduleName accum identifier (name, _) =
        accum
          & M.insert (pure identifier) (pure name)
          & M.insert (moduleName <> pure identifier) (pure name)


reportWarning
  :: MonadWriter [Diagnostic] m
  => Diagnostic
  -> m ()
reportWarning = tell . pure

reportError
  :: MonadWriter [Diagnostic] m
  => Diagnostic
  -> MaybeT m a
reportError = (>> mzero) . tell . pure


handleMaybe
  :: MonadWriter [Diagnostic] m
  => Diagnostic
  -> Maybe a
  -> MaybeT m a
handleMaybe diagnostic = \case
  Nothing -> reportError diagnostic
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
  :: TypeResolutionMode
  -> ResolveCallback r
  -> PathInfo Parsed
  -> MaybeT AnalysisM r
resolvePath mode processDeclaration PathInfo {..} = do
  names@(name :| others) <- handleMaybe (ErrorNameNotFound _pathName) =<<
    uses contextNames (M.lookup _pathName)
  when (not $ null others) $
    reportError $ ErrorAmbiguousPath _pathName names
  params <- traverse (resolveType mode) _pathParams
  resolveName processDeclaration $ PathInfo name params

lookupIdentifier
  :: Identifier
  -> MaybeT AnalysisM (Maybe (NonEmpty Name))
lookupIdentifier identifier =
  uses contextNames (M.lookup $ pure identifier)

resolveName
  :: ResolveCallback r
  -> PathInfo Resolved
  -> MaybeT AnalysisM r
resolveName processDeclaration resolvedPathInfo =
  uses contextDeclarations (M.lookup name) >>= \case
    Just decl -> processDeclaration resolvedPathInfo (Just decl)
    Nothing   -> views infoLocals (M.lookup name) >>= processDeclaration resolvedPathInfo
  where
    name = _pathName resolvedPathInfo

resolveConstValue
  :: PathInfo Parsed
  -> MaybeT AnalysisM TypedExpression
resolveConstValue pathInfo = resolvePath AllowPlaceholder go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} = \case
      Nothing   -> reportError undefined
      Just decl -> case _located decl of
        TypeAliasDecl _ -> reportError $ ErrorNotAConst originalPath _pathName
        FunctionDecl  _ -> reportError $ ErrorNotAConst originalPath _pathName
        StructDecl    _ -> reportError $ ErrorNotAConst originalPath _pathName
        EnumDecl      enumInfo -> do
          let identifier = NE.last originalPath
          if identifier == _enumName enumInfo
            then reportError $ ErrorNotAConst originalPath _pathName
            else
              case L.elemIndex identifier (_enumValues enumInfo) of
                Nothing -> error "ICE"
                Just i  -> pure $ TypedExpression resolvedPath $ IntLiteralExpr i
        ConstDecl     _ ->
          resolveDeclaration _pathName decl <&> \case
            ConstDecl cInfo -> _constExpr cInfo
            _ -> error "ICE"

{-
resolveTypeMaybe
  :: Path
  -> MaybeT AnalysisM [Name]
resolveTypeMaybe path = do
  names <- maybe [] NE.toList <$> uses contextNames (M.lookup path)
  catMaybes <$> for names \name ->
    resolveName go $ PathInfo name []
  where
    go :: ResolveCallback (Maybe Name)
    go PathInfo {..} = \case
      Nothing   -> pure $ Just _pathName
      Just decl -> case _located decl of
        TypeAliasDecl _ -> pure $ Just _pathName
        EnumDecl      _ -> pure $ Just _pathName
        StructDecl    _ -> pure $ Just _pathName
        ConstDecl     _ -> pure Nothing
        FunctionDecl  _ -> pure Nothing
-}

resolveType
  :: TypeResolutionMode
  -> PathInfo Parsed
  -> MaybeT AnalysisM (PathInfo Resolved)
resolveType mode pathInfo = resolvePath mode go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback (PathInfo Resolved)
    go path@PathInfo {..} = \case
      Nothing   -> case (_pathName, mode) of
        (Placeholder, ForbidPlaceholder context) ->
          reportError $ ErrorPlaceholder context
        _ -> pure path
      Just decl -> case _located decl of
        TypeAliasDecl info -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            TypeAliasDecl taInfo -> taInfo
            _ -> error "ICE"
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount _pathName expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          pure resultPathInfo
        ConstDecl     _ -> reportError (ErrorNotAType originalPath _pathName)
        FunctionDecl  _ -> reportError (ErrorNotAType originalPath _pathName)
        EnumDecl      _ -> do
          when (not $ null _pathParams) $
            reportError $ ErrorIncorrectTypeParameterCount _pathName 0 (length _pathParams)
          pure path
        StructDecl info -> do
          let expected = length (_structParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount _pathName expected actual
          pure path

resolveStruct
  :: PathInfo Parsed
  -> MaybeT AnalysisM (PathInfo Resolved, StructInfo Resolved, HashMap Identifier (PathInfo Resolved))
resolveStruct pathInfo = resolvePath AllowPlaceholder go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback (PathInfo Resolved, StructInfo Resolved, HashMap Identifier (PathInfo Resolved))
    go path@PathInfo {..} = \case
      Nothing -> do
        when (_pathName == Placeholder) $
          reportError $ ErrorPlaceholder "struct name in struct expression"
        reportError $ ErrorNotAStruct originalPath _pathName
      Just decl -> case _located decl of
        ConstDecl     _ -> reportError $ ErrorNotAStruct originalPath _pathName
        FunctionDecl  _ -> reportError $ ErrorNotAStruct originalPath _pathName
        EnumDecl      _ -> reportError $ ErrorNotAStruct originalPath _pathName
        TypeAliasDecl info -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            TypeAliasDecl taInfo -> taInfo
            _ -> error "ICE"
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount _pathName expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          resolveName go resultPathInfo
        StructDecl _ -> do
          resolvedInfo <- resolveDeclaration _pathName decl <&> \case
            StructDecl sInfo -> sInfo
            _ -> error "ICE"
          let expectedParams = _structParams resolvedInfo
              expectedCount  = length expectedParams
              givenCount     = length _pathParams
          when (expectedCount /= givenCount && givenCount > 0) $
            reportError $ ErrorIncorrectTypeParameterCount _pathName expectedCount givenCount
          let mapping = M.fromList $
                if null _pathParams
                then [(paramName, PlaceholderType) | paramName <- expectedParams]
                else zip expectedParams _pathParams
          pure (path, resolvedInfo, mapping)

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

setTypeParameters
  :: Name
  -> [Identifier]
  -> MaybeT AnalysisM ()
setTypeParameters typeName parameters = do
  typeNames <-
    fmap M.fromList $
      for (group $ sort parameters) \case
        []               -> error "ICE"
        (identifier:_:_) -> reportError $ ErrorDuplicateTypeParameter typeName identifier
        [identifier]     -> do
          when (isReserved identifier) $
            reportError $ ErrorReservedIdentifier typeName identifier
          let parameterName = TypeParameter identifier
          whenJustM (lookupIdentifier identifier) \names ->
            reportWarning $ WarningNameShadow names parameterName
          pure (pure identifier, pure parameterName)
  contextNames %= M.union typeNames

typeDiff
  :: PathInfo Resolved
  -> PathInfo Resolved
  -> [(PathInfo Resolved, PathInfo Resolved)]
typeDiff p1 p2
  | _pathName p1 /= _pathName p2 = [(p1, p2)]
  | otherwise = concatMap (uncurry typeDiff) $ zip (_pathParams p1) (_pathParams p2)

typeMatches
  :: PathInfo Resolved
  -> PathInfo Resolved
  -> Bool
typeMatches p1 p2 = case (_pathName p1, _pathName p2) of
  (BuiltinType "!void", _) -> True
  (_, BuiltinType "!void") -> True
  (TypeParameter _, _) -> True
  (_, TypeParameter _) -> True
  (Placeholder, _) -> True
  (_, Placeholder) -> True
  (name1, name2) -> name1 == name2 && and (zipWith typeMatches (_pathParams p1) (_pathParams p2))

typesAllMatch
  :: [PathInfo Resolved]
  -> Bool
typesAllMatch types = and do
  (headType : remainingTypes) <- L.tails types
  otherType <- remainingTypes
  pure $ headType `typeMatches` otherType

mostSpecificType
  :: NonEmpty (PathInfo Resolved)
  -> PathInfo Resolved
mostSpecificType = NE.head . NE.sortWith numberOfParameters
  where
    numberOfParameters PathInfo {..} = case _pathName of
      TypeParameter _ -> 1 :: Int
      _               -> sum $ map numberOfParameters _pathParams


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
    Nothing -> do
      resetState
      let declLocation = _location declaration
      contextLocation .= declLocation
      declCycle <- views infoStack (S.member thisName)
      if declCycle
      then reportError (ErrorCyclicDefinition thisName)
      else local (infoStack %~ S.insert thisName) do
        let declIdentifier = declarationIdentifier $ _located declaration
        when (isReserved declIdentifier) $
          reportError $ ErrorReservedIdentifier thisName declIdentifier
        resolvedDeclaration <- case _located declaration of
          TypeAliasDecl info -> TypeAliasDecl <$> analyzeTypeAlias thisName info
          StructDecl    info -> StructDecl    <$> analyzeStruct    thisName info
          EnumDecl      info -> EnumDecl      <$> analyzeEnum      thisName info
          ConstDecl     info -> ConstDecl     <$> analyzeConst     thisName info
          FunctionDecl  info -> FunctionDecl  <$> analyzeFunction  thisName info
        contextDeclarations %= M.insert thisName (WithLocation declLocation resolvedDeclaration)
        pure resolvedDeclaration

analyzeTypeAlias
  :: Name
  -> TypeAliasInfo Parsed
  -> MaybeT AnalysisM (TypeAliasInfo Resolved)
analyzeTypeAlias thisName TypeAliasInfo {..} = do
  setTypeParameters thisName _aliasParams
  resolved <- resolveType (ForbidPlaceholder "type alias declaration") _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolved

analyzeStruct
  :: Name
  -> StructInfo Parsed
  -> MaybeT AnalysisM (StructInfo Resolved)
analyzeStruct thisName info = do
  setTypeParameters thisName (_structParams info)
  structValues ((traverse . traverse) (resolveType $ ForbidPlaceholder "struct fields declaration")) info

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

analyzeConst
  :: Name
  -> ConstInfo Parsed
  -> MaybeT AnalysisM (ConstInfo Resolved)
analyzeConst _thisName ConstInfo {..} = do
  resolvedType <- resolveType (ForbidPlaceholder "const declaration") _constType
  resolvedExpr <- go _constExpr
  when (resolvedType /= _exprType resolvedExpr) $
    reportError $ ErrorWrongType [resolvedType] (_exprType resolvedExpr)
  pure $ ConstInfo _constName resolvedType resolvedExpr
  where
    binaryIntExpression
      :: (Int -> Int -> Int)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryIntExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression IntType (IntLiteralExpr x) -> pure x
        TypedExpression e1t _ ->
          reportError $ ErrorWrongType [IntType] e1t
      rhs <- go e2 >>= \case
        TypedExpression IntType (IntLiteralExpr x) -> pure x
        TypedExpression e2t _ ->
          reportError $ ErrorWrongType [IntType] e2t
      pure $ IntExpression (f lhs rhs)

    binaryBoolExpression
      :: (Bool -> Bool -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryBoolExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression e1t _ ->
          reportError $ ErrorWrongType [BoolType] e1t
      rhs <- go e2 >>= \case
        TypedExpression BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression e2t _ ->
          reportError $ ErrorWrongType [BoolType] e2t
      pure $ BoolExpression (f lhs rhs)

    comparisonExpression
      :: (Expression Resolved -> Expression Resolved -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    comparisonExpression op e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      when (t1 /= t2) $
        reportError $ ErrorWrongType [t1] t2
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
    go WithLocation {..} = do
      contextLocation .= _location
      case _located of
        PathExpr p ->
          resolveConstValue p
        CastExpr e t  -> do
          resolvedExpr <- go e
          resolvedTargetType <- resolveType (ForbidPlaceholder "cast expression") t
          let resolvedSourceType = _exprType resolvedExpr
              reportCastError :: forall a. MaybeT AnalysisM a
              reportCastError = reportError $ ErrorWrongCast resolvedSourceType resolvedTargetType
          targetEnum <- tryResolveEnumFromName resolvedTargetType
          sourceEnum <- tryResolveEnumFromName resolvedSourceType
          case (sourceEnum, targetEnum) of
            (Just _, Just destEnum) -> do
              intValue <- case _exprValue resolvedExpr of
                IntLiteralExpr  i -> pure i
                _                 -> error "ICE"
              when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                reportError $ ErrorEnumOutOfBounds destEnum intValue
              pure $ TypedExpression resolvedTargetType $ IntLiteralExpr intValue
            (Nothing, Just destEnum) -> do
              intValue <- case _exprValue resolvedExpr of
                IntLiteralExpr  i -> pure i
                CharLiteralExpr c -> pure $ ord c
                BoolLiteralExpr b -> pure $ fromEnum b
                _                 -> reportCastError
              when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                reportError $ ErrorEnumOutOfBounds destEnum intValue
              pure $ TypedExpression resolvedTargetType $ IntLiteralExpr intValue
            (Just _, Nothing) -> do
              intValue <- case _exprValue resolvedExpr of
                IntLiteralExpr  i -> pure i
                _                 -> error "ICE"
              case resolvedTargetType of
                IntType  -> pure $ IntExpression intValue
                CharType -> pure $ CharExpression $ chr intValue
                BoolType -> pure $ BoolExpression $ intValue /= 0
                _        -> reportCastError
            (Nothing, Nothing) -> do
              case (resolvedTargetType, _exprValue resolvedExpr) of
                (IntType,  IntLiteralExpr  i) -> pure $ IntExpression i
                (IntType,  CharLiteralExpr c) -> pure $ IntExpression $ ord c
                (IntType,  BoolLiteralExpr b) -> pure $ IntExpression $ fromEnum b
                (CharType, IntLiteralExpr  i) -> pure $ CharExpression $ chr i
                (CharType, CharLiteralExpr c) -> pure $ CharExpression c
                (BoolType, IntLiteralExpr  i) -> pure $ BoolExpression $ i /= 0
                (BoolType, CharLiteralExpr c) -> pure $ BoolExpression $ ord c > 0
                (BoolType, BoolLiteralExpr b) -> pure $ BoolExpression b
                _ -> reportCastError
        FieldAccessExpr expr field -> do
          TypedExpression structType structValue <- go expr
          case structValue of
            StructExpr _ fields -> do
              fmap snd $
                find ((field ==) . fst) fields `onNothing`
                  reportError (ErrorFieldAccessFieldNotFound structType field)
            _                   -> reportError $ ErrorFieldAccessNotAStruct structType
        CallExpr _ _  -> reportError undefined
        ArrayExpr _ -> undefined
        IndexExpr _ _ -> undefined
        StructExpr path fields -> do
          (resolvedType, structInfo, paramMapping) <- resolveStruct path
          resolvedFields <- (traverse . traverse) go fields
          fullyResolvedType <- analyzeStructFields resolvedType structInfo paramMapping resolvedFields
          pure $ TypedExpression fullyResolvedType $ StructExpr fullyResolvedType resolvedFields
        IntLiteralExpr    i -> pure $ IntExpression  i
        BoolLiteralExpr   b -> pure $ BoolExpression b
        CharLiteralExpr   c -> pure $ CharExpression c
        StringLiteralExpr s -> pure $ TypedExpression undefined $ StringLiteralExpr s
        ReferenceExpr _ -> reportError undefined
        BoolNegationExpr e -> go e >>= \case
          BoolExpression x -> pure $ BoolExpression (not x)
          TypedExpression t _ -> reportError $ ErrorWrongType [BoolType] t
        IntNegationExpr e -> go e >>= \case
          IntExpression x -> pure $ IntExpression (-x)
          TypedExpression t _ -> reportError $ ErrorWrongType [IntType] t
        AdditionExpr e1 e2 -> do
          lhs <- go e1
          case lhs of
            TypedExpression IntType (IntLiteralExpr _) -> pure ()
            TypedExpression e1t _ -> reportError $ ErrorWrongType [IntType] e1t
          rhs <- go e2
          case rhs of
            TypedExpression IntType (IntLiteralExpr _) -> pure ()
            TypedExpression e2t _ -> reportError $ ErrorWrongType [IntType] e2t
          when (_exprType lhs /= _exprType rhs) $
            reportError $ ErrorWrongType [_exprType lhs] (_exprType rhs)
          case (_exprValue lhs, _exprValue rhs) of
            (IntLiteralExpr x, IntLiteralExpr y) -> pure $ IntExpression (x + y)
            _                                    -> error "ICE"
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
        AssignmentExpr               _ _ -> reportError undefined
        AdditionAssignmentExpr       _ _ -> reportError undefined
        SubtractionAssignmentExpr    _ _ -> reportError undefined
        MultiplicationAssignmentExpr _ _ -> reportError undefined
        DivisionAssignmentExpr       _ _ -> reportError undefined
        ModuloAssignmentExpr         _ _ -> reportError undefined
        ExponentiationAssignmentExpr _ _ -> reportError undefined

analyzeStructFields
  :: PathInfo Resolved
  -> StructInfo Resolved
  -> HashMap Identifier (PathInfo Resolved)
  -> NonEmpty (Identifier, TypedExpression)
  -> MaybeT AnalysisM (PathInfo Resolved)
analyzeStructFields typeName StructInfo {..} paramMapping values = do
  let referenceMap = M.fromList $ NE.toList _structValues
      valuesMap    = M.fromListWith (<>) $ NE.toList $ (fmap . fmap) pure values
  for_ values \(identifier, _) -> do
    when (not $ M.member identifier referenceMap) $
      reportError $ ErrorStructUnknownField typeName identifier
  allDiffs <- for _structValues \(fieldName, fieldType) -> do
    case fold $ M.lookup fieldName valuesMap of
      []      -> reportError $ ErrorStructMissingField typeName fieldName
      (_:_:_) -> reportError $ ErrorStructDuplicatedField typeName fieldName
      [expr]  -> do
        catMaybes <$>
          for (typeDiff fieldType (_exprType expr)) \(lhs, rhs) -> do
            case (_pathName lhs, _pathName rhs) of
              (TypeParameter paramName, _) -> do
                typePattern <- M.lookup paramName paramMapping `onNothing` error "ICE"
                unless (typePattern `typeMatches` rhs) $
                  reportError $ ErrorWrongType [typePattern] rhs
                pure $ Just (paramName, pure rhs)
              (_, TypeParameter _) -> pure Nothing
              _ -> reportError $ ErrorWrongType [fieldType] (_exprType expr)
  let tempMapping :: HashMap Identifier (NonEmpty (PathInfo Resolved)) = M.fromListWith (<>) $ concat $ NE.toList allDiffs
  resolvedTypeParams <- for _structParams \paramName -> do
    possibleTypes <- M.lookup paramName tempMapping `onNothing`
      reportError (ErrorStructAmbiguousType typeName paramName)
    unless (typesAllMatch $ NE.toList possibleTypes) $
      reportError $ ErrorStructIncompatibleTypes typeName paramName possibleTypes
    pure $ mostSpecificType possibleTypes
  pure (typeName & pathParams .~ resolvedTypeParams)

analyzeFunction
  :: Name
  -> FunctionInfo Parsed
  -> MaybeT AnalysisM (FunctionInfo Resolved)
analyzeFunction thisName FunctionInfo {..} = do
  setTypeParameters thisName _funParams
  resolvedType <- traverse (resolveType $ ForbidPlaceholder "function declaration") _funType
  let argNames = M.fromListWith (+) do
        (argName, _) <- _funArgs
        pure (argName, 1 :: Int)
  allValid <- and <$> for (M.toList argNames) \(argName, argCount) -> do
    when (argCount > 1) $
      tell [ErrorFunctionDuplicatedArg argName]
    pure $ argCount == 1
  unless allValid mzero -- TODO: introduce better error handling
  resolvedArgs <- traverse analyzeArg _funArgs
  contextFunType .= fromMaybe UnitType resolvedType
  resolvedStatements <- traverse analyzeStatement _funBody
  pure $ FunctionInfo _funName _funParams resolvedArgs resolvedType resolvedStatements
  where
    analyzeArg (argName, argType) = do
      resolvedType <- case argType of
        ByValue t -> ByValue <$>
          resolveType (ForbidPlaceholder "function declaration") t
        ByReference t -> ByReference <$>
          resolveType (ForbidPlaceholder "function declaration") t
      when (isReserved argName) $
        reportError $ ErrorReservedIdentifier thisName argName
      let resolvedName = FunctionArgument argName
      whenJustM (lookupIdentifier argName) \names ->
        reportWarning $ WarningNameShadow names resolvedName
      contextFunArgs %= M.insert argName resolvedType
      contextNames %= M.insert (pure argName) (pure resolvedName)
      pure (argName, resolvedType)

analyzeStatement
  :: WithLocation (Statement Parsed)
  -> MaybeT AnalysisM (Statement Resolved)
analyzeStatement statement = do
  contextLocation .= _location statement
  case _located statement of
    ContinueStmt -> do
      unlessM (use contextWithinLoop) $
        reportError ErrorContinueNotInLoop
      pure ContinueStmt
    BreakStmt -> do
      unlessM (use contextWithinLoop) $
        reportError ErrorBreakNotInLoop
      pure BreakStmt
    ReturnStmt returnExpr -> do
      resolvedReturnExpr <- traverse analyzeFunctionExpression returnExpr
      let returnType = maybe UnitType _exprType resolvedReturnExpr
      funReturnType <- use contextFunType
      unless (funReturnType `typeMatches` returnType) $
        reportError $ ErrorWrongType [funReturnType] returnType
      pure $ ReturnStmt resolvedReturnExpr
    _ -> undefined

analyzeFunctionExpression
  :: WithLocation (Expression Parsed)
  -> MaybeT AnalysisM TypedExpression
analyzeFunctionExpression expr = do
  contextLocation .= _location expr
  case _located expr of
    CastExpr e t -> do
      resolvedExpr <- analyzeFunctionExpression e
      resolvedTargetType <- resolveType (ForbidPlaceholder "cast expression") t
      let resolvedSourceType = _exprType resolvedExpr
          reportCastError :: forall a. MaybeT AnalysisM a
          reportCastError = reportError $ ErrorWrongCast resolvedSourceType resolvedTargetType
      targetEnum <- tryResolveEnumFromName resolvedTargetType
      sourceEnum <- tryResolveEnumFromName resolvedSourceType
      case (sourceEnum, targetEnum) of
        (Just _, Just destEnum) -> do
          case _exprValue resolvedExpr of
            IntLiteralExpr i ->
              compileTimeEnumCast resolvedTargetType destEnum i
            _ -> do
              -- TODO: insert runtime cast
              undefined
        (Nothing, Just destEnum) -> do
          case ( _exprType resolvedExpr
               , _exprValue resolvedExpr
               ) of
            (_, IntLiteralExpr  i) ->
              compileTimeEnumCast resolvedTargetType destEnum i
            (_, CharLiteralExpr c) ->
              compileTimeEnumCast resolvedTargetType destEnum $ ord c
            (_, BoolLiteralExpr b) ->
              compileTimeEnumCast resolvedTargetType destEnum $ fromEnum b
            (IntType, _) -> do
              -- TODO: insert runtime cast
              undefined
            (CharType, _) -> do
              -- TODO: insert runtime cast
              undefined
            (BoolType, _) -> do
              -- TODO: insert runtime cast
              undefined
            (VoidType, value) -> do
              pure $ TypedExpression VoidType value
            _ -> case _pathName $ _exprType resolvedExpr of
              TypeParameter _ ->
                pure resolvedExpr
              _ -> do
                reportCastError
        (Just _, Nothing) -> do
          case (resolvedTargetType, _exprValue resolvedExpr) of
            (IntType, IntLiteralExpr i) ->
              pure $ IntExpression i
            (IntType, value) ->
              pure $ TypedExpression IntType value
            (CharType, IntLiteralExpr i) ->
              pure $ IntExpression i
            (CharType, value) ->
              pure $ TypedExpression IntType value
            (BoolType, IntLiteralExpr i) ->
              pure $ IntExpression i
            (BoolType, value) ->
              pure $ TypedExpression IntType value
            _ -> reportCastError
        (Nothing, Nothing) -> do
          case (resolvedTargetType, resolvedSourceType, _exprValue resolvedExpr) of
            (IntType,  _, IntLiteralExpr  i) -> pure $ IntExpression i
            (IntType,  _, CharLiteralExpr c) -> pure $ IntExpression $ ord c
            (IntType,  _, BoolLiteralExpr b) -> pure $ IntExpression $ fromEnum b
            (IntType,  IntType,  value)      -> pure $ TypedExpression IntType value
            (IntType,  CharType, value)      -> pure $ TypedExpression IntType value
            (IntType,  BoolType, value)      -> pure $ TypedExpression IntType value
            (CharType, _, IntLiteralExpr  i) -> pure $ CharExpression $ chr i
            (CharType, _, CharLiteralExpr c) -> pure $ CharExpression c
            (CharType, IntType,  value)      -> pure $ TypedExpression CharType value
            (CharType, CharType, value)      -> pure $ TypedExpression CharType value
            (BoolType, _, IntLiteralExpr  i) -> pure $ BoolExpression $ i /= 0
            (BoolType, _, CharLiteralExpr c) -> pure $ BoolExpression $ ord c > 0
            (BoolType, _, BoolLiteralExpr b) -> pure $ BoolExpression b
            (BoolType, IntType,  value)      -> pure $ TypedExpression BoolType value
            (BoolType, CharType, value)      -> pure $ TypedExpression BoolType value
            (BoolType, BoolType, value)      -> pure $ TypedExpression BoolType value
            _ -> reportCastError

    {-
    PathExpr p ->
      resolveValue p -}
    _ -> undefined
  where
    compileTimeEnumCast enumType enumInfo intValue = do
      when (intValue < 0 || intValue >= length (_enumValues enumInfo)) $
        reportError $ ErrorEnumOutOfBounds enumInfo intValue
      pure $ TypedExpression enumType $ IntLiteralExpr intValue

{-
    FieldAccessExpr expr field -> do
      TypedExpression structType structValue <- go expr
      case structValue of
        StructExpr _ fields -> do
          fmap snd $
            find ((field ==) . fst) fields `onNothing`
              reportError (ErrorFieldAccessFieldNotFound structType field)
        _                   -> reportError $ ErrorFieldAccessNotAStruct structType
    CallExpr _ _  -> reportError undefined
    ArrayExpr _ -> undefined
    IndexExpr _ _ -> undefined
    StructExpr path fields -> do
      (resolvedType, structInfo, paramMapping) <- resolveStruct path
      resolvedFields <- (traverse . traverse) go fields
      fullyResolvedType <- analyzeStructFields resolvedType structInfo paramMapping resolvedFields
      pure $ TypedExpression fullyResolvedType $ StructExpr fullyResolvedType resolvedFields
    IntLiteralExpr    i -> pure $ IntExpression  i
    BoolLiteralExpr   b -> pure $ BoolExpression b
    CharLiteralExpr   c -> pure $ CharExpression c
    StringLiteralExpr s -> pure $ TypedExpression undefined $ StringLiteralExpr s
    ReferenceExpr _ -> reportError undefined
    BoolNegationExpr e -> go e >>= \case
      BoolExpression x -> pure $ BoolExpression (not x)
      TypedExpression t _ -> reportError $ ErrorWrongType [BoolType] t
    IntNegationExpr e -> go e >>= \case
      IntExpression x -> pure $ IntExpression (-x)
      TypedExpression t _ -> reportError $ ErrorWrongType [IntType] t
    AdditionExpr e1 e2 -> do
      lhs <- go e1
      case lhs of
        TypedExpression IntType (IntLiteralExpr _) -> pure ()
        TypedExpression e1t _ -> reportError $ ErrorWrongType [IntType] e1t
      rhs <- go e2
      case rhs of
        TypedExpression IntType (IntLiteralExpr _) -> pure ()
        TypedExpression e2t _ -> reportError $ ErrorWrongType [IntType] e2t
      when (_exprType lhs /= _exprType rhs) $
        reportError $ ErrorWrongType [_exprType lhs] (_exprType rhs)
      case (_exprValue lhs, _exprValue rhs) of
        (IntLiteralExpr x, IntLiteralExpr y) -> pure $ IntExpression (x + y)
        _                                    -> error "ICE"
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
    AssignmentExpr               _ _ -> reportError undefined
    AdditionAssignmentExpr       _ _ -> reportError undefined
    SubtractionAssignmentExpr    _ _ -> reportError undefined
    MultiplicationAssignmentExpr _ _ -> reportError undefined
    DivisionAssignmentExpr       _ _ -> reportError undefined
    ModuloAssignmentExpr         _ _ -> reportError undefined
    ExponentiationAssignmentExpr _ _ -> reportError undefined
  where
    binaryIntExpression
      :: (Int -> Int -> Int)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryIntExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression IntType (IntLiteralExpr x) -> pure x
        TypedExpression e1t _ ->
          reportError $ ErrorWrongType [IntType] e1t
      rhs <- go e2 >>= \case
        TypedExpression IntType (IntLiteralExpr x) -> pure x
        TypedExpression e2t _ ->
          reportError $ ErrorWrongType [IntType] e2t
      pure $ IntExpression (f lhs rhs)

    binaryBoolExpression
      :: (Bool -> Bool -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    binaryBoolExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression e1t _ ->
          reportError $ ErrorWrongType [BoolType] e1t
      rhs <- go e2 >>= \case
        TypedExpression BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression e2t _ ->
          reportError $ ErrorWrongType [BoolType] e2t
      pure $ BoolExpression (f lhs rhs)

    comparisonExpression
      :: (Expression Resolved -> Expression Resolved -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    comparisonExpression op e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      when (t1 /= t2) $
        reportError $ ErrorWrongType [t1] t2
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
-}


--------------------------------------------------------------------------------
-- Local helpers

pattern IntExpression :: Int -> TypedExpression
pattern IntExpression x = TypedExpression IntType (IntLiteralExpr x)

pattern CharExpression :: Char -> TypedExpression
pattern CharExpression x = TypedExpression CharType (CharLiteralExpr x)

pattern BoolExpression :: Bool -> TypedExpression
pattern BoolExpression x = TypedExpression BoolType (BoolLiteralExpr x)
