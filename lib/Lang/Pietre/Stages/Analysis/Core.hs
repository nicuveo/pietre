{-# LANGUAGE PatternSynonyms #-}

module Lang.Pietre.Stages.Analysis.Core where

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
import Lang.Pietre.Stages.Analysis.Context
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Monad


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
          whenJustM (lookupRole identifier) \names ->
            reportWarning $ WarningNameShadow names parameterRole
          pure (pure identifier, pure parameterRole)
  contextScope %= M.union typeNames

typeDiff
  :: PathInfo Resolved
  -> PathInfo Resolved
  -> [(PathInfo Resolved, PathInfo Resolved)]
typeDiff p1 p2 = case (_pathName p1, _pathName p2) of
  (TopLevelDeclaration t1, TopLevelDeclaration t2) ->
    if t1 == t2 then similar else different
  (BuiltinType t1, BuiltinType t2) ->
    if t1 == t2 then similar else different
  (TypeParameter t1, TypeParameter t2) ->
    if t1 == t2 then similar else different
  (Placeholder, Placeholder) ->
    similar
  (FunctionPointer t1, FunctionPointer t2) ->
    let args1 = map functionArgType $ _funtypeArgs t1
        args2 = map functionArgType $ _funtypeArgs t2
        return1 = fromMaybe UnitType $ _funtypeReturn t1
        return2 = fromMaybe UnitType $ _funtypeReturn t2
    in concat (zipWith typeDiff args1 args2) ++ typeDiff return1 return2
  _ ->
    different
  where
    different = [(p1, p2)]
    similar   = concatMap (uncurry typeDiff) $ zip (_pathParams p1) (_pathParams p2)

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
  (TopLevelDeclaration t1, TopLevelDeclaration t2) ->
    t1 == t2 && argMatches
  (BuiltinType t1, BuiltinType t2) ->
    t1 == t2 && argMatches
  (FunctionPointer t1, FunctionPointer t2) ->
    let args1 = _funtypeArgs t1
        args2 = _funtypeArgs t2
        return1 = fromMaybe UnitType $ _funtypeReturn t1
        return2 = fromMaybe UnitType $ _funtypeReturn t2
    in and (zipWith checkArgs args1 args2) && typeMatches return1 return2
  _ -> False
  where
    argMatches = and $ zipWith typeMatches (_pathParams p1) (_pathParams p2)
    checkArgs = curry \case
      (ByValue     t1, ByValue     t2) -> t1 `typeMatches` t2
      (ByReference t1, ByReference t2) -> t1 `typeMatches` t2
      _ -> False

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
  unless (resolvedType `typeMatches` _exprType resolvedExpr) $
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
      :: (forall a. Ord a => a -> a -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> MaybeT AnalysisM TypedExpression
    comparisonExpression op e1 e2 = do
      TypedExpression t1 r1 <- go e1
      TypedExpression t2 r2 <- go e2
      unless (t1 `typeMatches` t2) $
        reportError $ ErrorWrongType [t1] t2
      pure $ BoolExpression $ compareExpressions op r1 r2

    compareExpressions
      :: (forall a. Ord a => a -> a -> Bool)
      -> Expression Resolved
      -> Expression Resolved
      -> Bool
    compareExpressions op e1 e2 = do
      case (e1, e2) of
        (StructExpr _ f1, StructExpr _ f2) ->
          let sortedF1 = map (_exprValue . snd) $ L.sortOn fst $ NE.toList f1
              sortedF2 = map (_exprValue . snd) $ L.sortOn fst $ NE.toList f2
          in all (uncurry $ compareExpressions op) $ zip sortedF1 sortedF2
        (IntLiteralExpr  i1, IntLiteralExpr  i2) -> op i1 i2
        (CharLiteralExpr c1, CharLiteralExpr c2) -> op c1 c2
        (BoolLiteralExpr b1, BoolLiteralExpr b2) -> op b1 b2
        _ -> error "ICE"

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
          targetEnum <- lookupEnumType resolvedTargetType
          sourceEnum <- lookupEnumType resolvedSourceType
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
          (resolvedType, structInfo, paramMapping) <- fromMaybe (error "ICE") <$> resolveStructType path
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
          unless (_exprType lhs `typeMatches` _exprType rhs) $
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
  resolvedType <- traverse (resolveType $ ForbidPlaceholder "function definition") _funReturn
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
      whenJustM (lookupRole argName) \names ->
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
      targetEnum <- lookupEnumType resolvedTargetType
      sourceEnum <- lookupEnumType resolvedSourceType
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
      resolveExprValue p
    FieldAccessExpr subExpr field -> do
      _lhs@(TypedExpression structType structValue) <- analyzeFunctionExpression subExpr
      case structValue of
        StructExpr _ fields -> do
          fmap snd $
            find ((field ==) . fst) fields `onNothing`
              reportError (ErrorFieldAccessFieldNotFound structType field)
        -- _ -> resolveStruct structType >>= \case
        --   Nothing -> TypedExpression structType $ FieldAccessExpr lhs field
        --   Just (resolvedTypeName, structInfo, paramMapping) -> do
        --     fieldType <- find ((field ==) . fst) (_structValues structInfo) `onNothing`
        --       reportError (ErrorFieldAccessFieldNotFound structType field)
        --     resolvedFieldType <- substituteTypes paramMapping fieldType
        --     pure $ TypedExpression fieldType $ FieldAccessExpr lhs field
        _ -> undefined
    CallExpr funPath arguments  -> do
      (FunctionType {..}, mapping) <- resolveFunctionCallValue funPath
      resolvedArgs <- traverse analyzeFunctionExpression arguments
      let resolvedType = fromMaybe UnitType _funtypeReturn
      -- TODO: perform type parameter matching
      -- TODO: validate arg numbers and type
      undefined resolvedArgs resolvedType mapping
    _ -> undefined
  where
    compileTimeEnumCast enumType enumInfo intValue = do
      when (intValue < 0 || intValue >= length (_enumValues enumInfo)) $
        reportError $ ErrorEnumOutOfBounds enumInfo intValue
      pure $ TypedExpression enumType $ IntLiteralExpr intValue

{-
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
