{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Stages.Analysis where

import "this" Prelude

import Control.Lens                           hiding (mapping, op)
import Control.Monad.Extra                    (unlessM, whenJustM)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe              (hoistMaybe)
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

type DefinitionCache = HashMap Name (WithLocation (Definition Resolved))

data ResolvedModule = ResolvedModule
  { _resmodExported        :: HashSet Identifier
  , _resmodDefinitionCache :: DefinitionCache
  }

analyzeModule
  :: DefinitionCache
  -> HashMap ModuleName (HashSet Identifier)
  -> ModuleName
  -> Module
  -> ([Diagnostic], Maybe ResolvedModule)
analyzeModule foreignDefinitions moduleExports moduleName Module {..} = runMaybeT do
  (exported, localDefinitions, localScope) <- createLocalScope moduleName _modDefinitions
  foreignScope <- createForeignScope moduleExports _modImports
  let builtinScope = M.fromList $ map (fmap pure) builtins
  let topLevelScope = builtinScope `combineMaps` localScope `combineMaps` foreignScope
  let analysisInfo = AnalysisInfo moduleName S.empty localDefinitions foreignDefinitions topLevelScope
  let (diagnostics, resolvedModule) =
        runAnalysis analysisInfo do
          result <- sequence <$>
            traverse (runMaybeT . analyzeDefinition) _modDefinitions
          cachedDefinitions <- use contextCache
          pure $ ResolvedModule exported cachedDefinitions <$ result
  tell diagnostics
  hoistMaybe resolvedModule
  where
    combineMaps = M.unionWith (<>)


--------------------------------------------------------------------------------
-- Internals

definitionIdentifiers :: Definition Parsed -> NonEmpty Identifier
definitionIdentifiers = \case
  ConstDef     ConstInfo     {..} -> pure _constName
  TypeAliasDef TypeAliasInfo {..} -> pure _aliasName
  StructDef    StructInfo    {..} -> pure _structName
  FunctionDef  FunctionInfo  {..} -> pure _funName
  EnumDef      EnumInfo      {..} -> _enumName :| _enumValues

createForeignScope
  :: MonadWriter [Diagnostic] m
  => HashMap ModuleName (HashSet Identifier)
  -> [Import]
  -> MaybeT m (HashMap Path (NonEmpty Role))
createForeignScope moduleExports imports = do
  -- for each imported module, we create a hashmap
  -- from module name to hashmap of path to non-empty list:
  -- the hashmap of imported paths, grouped by module
  knownSymbols :: [HashMap ModuleName (HashMap Path (NonEmpty Role))] <-
    for imports \Import {..} -> do
      exportedIdentifiers <- handleMaybe (ErrorImportPath _importPath) $
        M.lookup _importPath moduleExports
      let mkRole identifier = pure $ TopLevelDeclaration $ Name (_importPath <> pure identifier) []
      M.singleton _importPath . M.fromListWith (<>) <$> case _importType of
        Qualified Nothing ->
          pure $ S.toList exportedIdentifiers <&> \identifier ->
            (_importPath <> pure identifier, mkRole identifier)
        Qualified (Just qualifier) ->
          pure $ S.toList exportedIdentifiers >>= \identifier ->
            [ (pure qualifier <> pure identifier, mkRole identifier)
            , (_importPath <> pure identifier, mkRole identifier)
            ]
        Exhaustive ->
          pure $ S.toList exportedIdentifiers >>= \identifier ->
            [ (pure identifier, mkRole identifier)
            , (_importPath <> pure identifier, mkRole identifier)
            ]
        Specific identifiers ->
          concat <$> for identifiers \identifier -> do
            unless (identifier `S.member` exportedIdentifiers) $
              reportError $ ErrorImportSymbol _importPath identifier
            pure
              [ (pure identifier, mkRole identifier)
              , (_importPath <> pure identifier, mkRole identifier)
              ]

  -- we group the declarations per module, using (<>) on the hashmap:
  -- this discards duplicates within the same module, as the same
  -- module might appear more than once in the list of imports.
  -- we then concatenate the non-empty lists, which confusingly is also
  -- a union with (<>), but on the non-empty lists.
  -- the result is a hashmap from path to grouped non-empty list of
  -- possible matches across modules
  pure $
    foldl' (M.unionWith (<>)) M.empty $ M.elems $
    foldl' (M.unionWith (<>)) M.empty $ knownSymbols

createLocalScope
  :: MonadWriter [Diagnostic] m
  => ModuleName
  -> [Annotated Definition Parsed]
  -> MaybeT m
     ( HashSet Identifier
     , HashMap Name (WithLocation (Definition Parsed))
     , HashMap Path (NonEmpty Role)
     )
createLocalScope moduleName definitions = do
  -- gather all top level names
  -- group them by identifier
  let topLevelNames = M.fromListWith (<>) do
        definition <- definitions
        identifier <- NE.toList $ definitionIdentifiers $ _located definition
        let name  = Name (moduleName <> pure identifier) []
            role  = TopLevelDeclaration name
            paths = [pure identifier, moduleName <> pure identifier]
        pure (identifier, pure ((name, definition), map (, pure role) paths))

  -- report an error if any identifier appears more than once
  failed <- or <$> for (M.toList topLevelNames) \(identifier, entries) -> do
    let defs = entries <&> \((_, definition), _) -> definition
    let hasDuplicates = NE.length defs > 1
    when hasDuplicates $
      tell [ErrorMultipleDeclaration identifier $ fmap _location defs]
    pure hasDuplicates
  when failed mzero

  -- create all local maps
  let exports = M.keys topLevelNames
  let (localDefinitions, localScope) = unzip $ map NE.head $ M.elems topLevelNames
  pure ( S.fromList exports
       , M.fromList localDefinitions
       , M.fromList (concat localScope)
       )

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
  -> Maybe (Name, WithLocation (Definition p))
  -> MaybeT AnalysisM r

resolvePath
  :: TypeResolutionMode
  -> ResolveCallback r
  -> PathInfo Parsed
  -> MaybeT AnalysisM r
resolvePath mode callback PathInfo {..} = do
  roles@(role :| others) <- handleMaybe (ErrorRoleNotFound _pathName) =<<
    uses contextScope (M.lookup _pathName)
  when (not $ null others) $
    reportError $ ErrorAmbiguousPath _pathName roles
  params <- traverse (resolveType mode) _pathParams
  resolveRole callback $ PathInfo role params

lookupIdentifier
  :: Identifier
  -> MaybeT AnalysisM (Maybe (NonEmpty Role))
lookupIdentifier identifier =
  uses contextScope (M.lookup $ pure identifier)

resolveRole
  :: forall r
   . ResolveCallback r
  -> PathInfo Resolved
  -> MaybeT AnalysisM r
resolveRole callback resolvedPathInfo = do
  case getName (_pathName resolvedPathInfo) of
    Nothing   -> callback @Resolved resolvedPathInfo Nothing
    Just name -> do
      let
        call :: forall (p :: ASTPhase)
             .  Analyzable p
             => WithLocation (Definition p)
             -> MaybeT AnalysisM r
        call = callback resolvedPathInfo . Just . (name,)
      foreignDefinition <- views infoForeignDefinitions (M.lookup name)
      cachedDefinition  <- uses contextCache (M.lookup name)
      localDefinition   <- views infoLocalDefinitions (M.lookup name)
      let action = asum [ call <$> foreignDefinition
                        , call <$> cachedDefinition
                        , call <$> localDefinition
                        ]
      case action of
        Nothing -> error "ICE"
        Just a  -> a
  where
    getName = \case
      TopLevelDeclaration name   -> Just name
      BuiltinType _              -> Nothing
      BuiltinFunction name       -> Just name
      TypeParameter _            -> Nothing
      Placeholder                -> Nothing
      FunctionArgument _ argType -> getName $ _pathName $ functionArgType argType
      LetVariable _ varType      -> getName $ _pathName varType

resolveConstValue
  :: PathInfo Parsed
  -> MaybeT AnalysisM TypedExpression
resolveConstValue pathInfo = resolvePath AllowPlaceholder go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} = \case
      Nothing   -> reportError $ ErrorNotAConst originalPath _pathName
      Just (name, def) -> case _located def of
        TypeAliasDef _ -> reportError $ ErrorNotAConst originalPath _pathName
        FunctionDef  _ -> reportError $ ErrorNotAConst originalPath _pathName
        StructDef    _ -> reportError $ ErrorNotAConst originalPath _pathName
        EnumDef enumInfo -> do
          -- TODO: reject type parameters
          let identifier = NE.last $ _nameFullPath name
          if identifier == _enumName enumInfo
            then reportError $ ErrorNotAConst originalPath _pathName
            else
              case L.elemIndex identifier (_enumValues enumInfo) of
                Nothing -> error "ICE"
                Just i  -> pure $ TypedExpression resolvedPath $ IntLiteralExpr i
        ConstDef info -> do
          -- TODO: reject type parameters
          _constExpr <$> resolveDefinition (_location def) info

resolveValue
  :: PathInfo Parsed
  -> MaybeT AnalysisM TypedExpression
resolveValue pathInfo = resolvePath AllowPlaceholder go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback TypedExpression
    go resolvedPath@PathInfo {..} = \case
      Nothing -> case _pathName of
        FunctionArgument _ (ByValue argType) ->
          pure $ TypedExpression argType $ PathExpr $ resolvedPath
        FunctionArgument _ (ByReference argType) ->
          pure $ TypedExpression argType $ PathExpr $ resolvedPath
        LetVariable _ varType ->
          pure $ TypedExpression varType $ PathExpr $ resolvedPath
        TopLevelDeclaration _ -> error "ICE"
        Placeholder -> reportError $ ErrorPlaceholder "function expression path"
        _ -> reportError $ ErrorNotAValue originalPath _pathName
      Just (name, def) -> case _located def of
        TypeAliasDef _ -> reportError $ ErrorNotAValue originalPath _pathName
        FunctionDef  _ -> reportError $ ErrorNotAValue originalPath _pathName
        StructDef    _ -> reportError $ ErrorNotAValue originalPath _pathName
        EnumDef enumInfo -> do
          -- TODO: reject type parameters
          let identifier = NE.last $ _nameFullPath name
          if identifier == _enumName enumInfo
            then reportError $ ErrorNotAValue originalPath _pathName
            else
              case L.elemIndex identifier (_enumValues enumInfo) of
                Nothing -> error "ICE"
                Just i  -> pure $ TypedExpression resolvedPath $ IntLiteralExpr i
        ConstDef info ->
          -- TODO: reject type parameters
          _constExpr <$> resolveDefinition (_location def) info


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
      Nothing -> case (_pathName, mode) of
        (Placeholder, ForbidPlaceholder context) ->
          reportError $ ErrorPlaceholder context
        (FunctionArgument _ _, _) ->
          reportError $ ErrorNotAType originalPath _pathName
        (LetVariable _ _, _) ->
          reportError $ ErrorNotAType originalPath _pathName
        _ -> pure path
      Just (name, def) -> case _located def of
        ConstDef     _ -> reportError (ErrorNotAType originalPath _pathName)
        FunctionDef  _ -> reportError (ErrorNotAType originalPath _pathName)
        TypeAliasDef info -> do
          resolvedInfo <- resolveDefinition (_location def) info
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount name expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          pure resultPathInfo
        EnumDef enumInfo -> do
          let identifier = NE.last $ _nameFullPath name
          when (identifier /= _enumName enumInfo) $
            reportError $ ErrorNotAType originalPath _pathName
          pure path
        StructDef info -> do
          let expected = length (_structParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount name expected actual
          pure path

resolveStruct
  :: PathInfo Parsed
  -> MaybeT AnalysisM (Maybe (PathInfo Resolved, StructInfo Resolved, HashMap Identifier (PathInfo Resolved)))
resolveStruct pathInfo = resolvePath AllowPlaceholder go pathInfo
  where
    originalPath = _pathName pathInfo
    go :: ResolveCallback (Maybe (PathInfo Resolved, StructInfo Resolved, HashMap Identifier (PathInfo Resolved)))
    go path@PathInfo {..} = \case
      Nothing ->
        case _pathName of
          TypeParameter _ ->
            pure Nothing
          Placeholder ->
            reportError $ ErrorPlaceholder "struct name in struct expression"
          _ ->
            reportError $ ErrorNotAStruct originalPath _pathName
      Just (name, def) -> case _located def of
        ConstDef     _ -> reportError $ ErrorNotAStruct originalPath _pathName
        FunctionDef  _ -> reportError $ ErrorNotAStruct originalPath _pathName
        EnumDef      _ -> reportError $ ErrorNotAStruct originalPath _pathName
        TypeAliasDef info -> do
          resolvedInfo <- resolveDefinition (_location def) info
          let expected = length (_aliasParams info)
              actual   = length _pathParams
          when (expected /= actual) $
            reportError $ ErrorIncorrectTypeParameterCount name expected actual
          let typeArguments = M.fromList $ zip (_aliasParams resolvedInfo) _pathParams
          resultPathInfo <- substituteTypes typeArguments $ _aliasValue resolvedInfo
          resolveRole go resultPathInfo
        StructDef info -> do
          resolvedInfo <- resolveDefinition (_location def) info
          let expectedParams = _structParams resolvedInfo
              expectedCount  = length expectedParams
              givenCount     = length _pathParams
          when (expectedCount /= givenCount && givenCount > 0) $
            reportError $ ErrorIncorrectTypeParameterCount name expectedCount givenCount
          let mapping = M.fromList $
                if null _pathParams
                then [(paramName, PlaceholderType) | paramName <- expectedParams]
                else zip expectedParams _pathParams
          pure $ Just (path, resolvedInfo, mapping)

tryResolveEnumFromRole
  :: PathInfo Resolved
  -> MaybeT AnalysisM (Maybe (EnumInfo Resolved))
tryResolveEnumFromRole = resolveRole go
  where
    go :: ResolveCallback (Maybe (EnumInfo Resolved))
    go _ info = sequence do
      WithLocation loc (EnumDef enumInfo) <- fmap snd info
      pure $ resolveDefinition loc enumInfo

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
          let parameterRole = TypeParameter identifier
          whenJustM (lookupIdentifier identifier) \names ->
            reportWarning $ WarningNameShadow names parameterRole
          pure (pure identifier, pure parameterRole)
  contextScope %= M.union typeNames

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
-- Analyzable

class Analyzable p where
  resolveDefinition
    :: IsDefinition i
    => Location
    -> i p
    -> MaybeT AnalysisM (i Resolved)

instance Analyzable Parsed where
  resolveDefinition loc info = toDefinition info
    & WithLocation loc
    & analyzeDefinition
    & fmap (fromMaybe (error "ICE") . fromDefinition)

instance Analyzable Resolved where
  resolveDefinition _ = pure

class IsDefinition i where
  toDefinition :: i p -> Definition p
  fromDefinition :: Definition p -> Maybe (i p)

instance IsDefinition TypeAliasInfo where
  toDefinition = TypeAliasDef
  fromDefinition = \case
    TypeAliasDef i -> Just i
    _ -> Nothing

instance IsDefinition EnumInfo where
  toDefinition = EnumDef
  fromDefinition = \case
    EnumDef i -> Just i
    _ -> Nothing

instance IsDefinition StructInfo where
  toDefinition = StructDef
  fromDefinition = \case
    StructDef i -> Just i
    _ -> Nothing

instance IsDefinition ConstInfo where
  toDefinition = ConstDef
  fromDefinition = \case
    ConstDef i -> Just i
    _ -> Nothing

instance IsDefinition FunctionInfo where
  toDefinition = FunctionDef
  fromDefinition = \case
    FunctionDef i -> Just i
    _ -> Nothing


--------------------------------------------------------------------------------
-- Analysis

analyzeDefinition
  :: WithLocation (Definition Parsed)
  -> MaybeT AnalysisM (Definition Resolved)
analyzeDefinition definition = do
  moduleName <- view infoModuleName
  let exportedIdentifiers = definitionIdentifiers $ _located definition
      rootName = Name (moduleName <> pure (NE.head exportedIdentifiers)) []
  exportedNames <- for exportedIdentifiers \identifier -> do
    when (isReserved identifier) $
      reportError $ ErrorReservedIdentifier rootName identifier
    pure $ Name (moduleName <> pure identifier) []
  uses contextCache (M.lookup rootName) >>= \case
    Just def -> pure $ _located def
    Nothing -> do
      resetState
      let defLocation = _location definition
      contextLocation .= defLocation
      defCycle <- views infoStack (S.member rootName)
      if defCycle
      then reportError (ErrorCyclicDefinition rootName)
      else local (infoStack %~ S.insert rootName) do
        resolvedDefinition <- case _located definition of
          TypeAliasDef info -> TypeAliasDef <$> analyzeTypeAlias rootName info
          StructDef    info -> StructDef    <$> analyzeStruct    rootName info
          EnumDef      info -> EnumDef      <$> analyzeEnum      rootName info
          ConstDef     info -> ConstDef     <$> analyzeConst     rootName info
          FunctionDef  info -> FunctionDef  <$> analyzeFunction  rootName info
        contextCache <>= M.fromList do
          name <- NE.toList exportedNames
          pure (name, WithLocation defLocation resolvedDefinition)
        pure resolvedDefinition

analyzeTypeAlias
  :: Name
  -> TypeAliasInfo Parsed
  -> MaybeT AnalysisM (TypeAliasInfo Resolved)
analyzeTypeAlias thisName TypeAliasInfo {..} = do
  setTypeParameters thisName _aliasParams
  resolved <- resolveType (ForbidPlaceholder "type alias definition") _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolved

analyzeStruct
  :: Name
  -> StructInfo Parsed
  -> MaybeT AnalysisM (StructInfo Resolved)
analyzeStruct thisName info = do
  setTypeParameters thisName (_structParams info)
  structValues ((traverse . traverse) (resolveType $ ForbidPlaceholder "struct fields definition")) info

analyzeEnum
  :: Name
  -> EnumInfo Parsed
  -> MaybeT AnalysisM (EnumInfo Resolved)
analyzeEnum thisName EnumInfo {..} = do
  let names = group $ sort _enumValues
  failed <- or <$> for names \case
    []       -> error "ICE"
    [_]      -> pure False
    (name:_) -> do
      tell [ErrorEnumDuplicateEntries thisName name]
      pure True
  when failed mzero
  pure $ EnumInfo _enumName _enumValues

analyzeConst
  :: Name
  -> ConstInfo Parsed
  -> MaybeT AnalysisM (ConstInfo Resolved)
analyzeConst _thisName ConstInfo {..} = do
  resolvedType <- resolveType (ForbidPlaceholder "const definition") _constType
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
          targetEnum <- tryResolveEnumFromRole resolvedTargetType
          sourceEnum <- tryResolveEnumFromRole resolvedSourceType
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
          (resolvedType, structInfo, paramMapping) <- fromMaybe (error "ICE") <$> resolveStruct path
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
  resolvedType <- traverse (resolveType $ ForbidPlaceholder "function definition") _funType
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
  resolvedStatements <- analyzeBlock _funBody
  pure $ FunctionInfo _funName _funParams resolvedArgs resolvedType resolvedStatements
  where
    analyzeArg (argName, argType) = do
      resolvedType <- case argType of
        ByValue t -> ByValue <$>
          resolveType (ForbidPlaceholder "function definition") t
        ByReference t -> ByReference <$>
          resolveType (ForbidPlaceholder "function definition") t
      when (isReserved argName) $
        reportError $ ErrorReservedIdentifier thisName argName
      let resolvedRole = FunctionArgument argName resolvedType
      whenJustM (lookupIdentifier argName) \names ->
        reportWarning $ WarningNameShadow names resolvedRole
      contextScope %= M.insert (pure argName) (pure resolvedRole)
      pure (argName, resolvedType)

analyzeBlock
  :: [WithLocation (Statement Parsed)]
  -> MaybeT AnalysisM [Statement Resolved]
analyzeBlock statements = do
  previousScope <- use contextScope
  result <- traverse analyzeStatement statements
  contextScope .= previousScope
  pure result

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
          resultCast =
            TypedExpression resolvedTargetType $
            CastExpr resolvedExpr resolvedTargetType
      targetEnum <- tryResolveEnumFromRole resolvedTargetType
      sourceEnum <- tryResolveEnumFromRole resolvedSourceType
      case (sourceEnum, targetEnum) of
        (Just _, Just destEnum) -> do
          case _exprValue resolvedExpr of
            IntLiteralExpr i ->
              compileTimeEnumCast resolvedTargetType destEnum i
            _ ->
              pure resultCast
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
            (IntType, _) ->
              pure resultCast
            (CharType, _) ->
              pure resultCast
            (BoolType, _) ->
              pure resultCast
            (VoidType, value) -> do
              pure $ TypedExpression VoidType value
            _ -> case _pathName $ _exprType resolvedExpr of
              TypeParameter _ ->
                pure resultCast
              _ -> do
                reportCastError
        (Just _, Nothing) -> do
          case (resolvedTargetType, _exprValue resolvedExpr) of
            (IntType, IntLiteralExpr i) ->
              pure $ IntExpression i
            (IntType, value) ->
              pure $ TypedExpression IntType value
            (CharType, IntLiteralExpr i) ->
              pure $ CharExpression $ chr i
            (CharType, value) ->
              pure $ TypedExpression CharType value
            (BoolType, IntLiteralExpr i) ->
              pure $ BoolExpression $ i /= 0
            (BoolType, value) ->
              pure $ TypedExpression BoolType value
            _ -> case _pathName resolvedTargetType of
                TypeParameter _ ->
                  pure resultCast
                _ -> do
                  reportCastError
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
            _ -> case ( _pathName (_exprType resolvedExpr)
                      , _pathName resolvedTargetType
                      ) of
              (TypeParameter _, _) -> pure resultCast
              (_, TypeParameter _) -> pure resultCast
              _                    -> reportCastError
    PathExpr p ->
      resolveValue p
    FieldAccessExpr subExpr field -> do
      TypedExpression structType structValue <- analyzeFunctionExpression subExpr
      case structValue of
        StructExpr _ fields -> do
          fmap snd $
            find ((field ==) . fst) fields `onNothing`
              reportError (ErrorFieldAccessFieldNotFound structType field)
        _ -> undefined -- do
          -- reportError $ ErrorFieldAccessNotAStruct structType
    _ -> undefined
  where
    compileTimeEnumCast enumType enumInfo intValue = do
      when (intValue < 0 || intValue >= length (_enumValues enumInfo)) $
        reportError $ ErrorEnumOutOfBounds enumInfo intValue
      pure $ TypedExpression enumType $ IntLiteralExpr intValue

{-
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
