module Lang.Pietre.Stages.Analysis.Core where

import "this" Prelude

import Control.Lens                              hiding (mapping, op, (...))
import Control.Monad.Extra                       (unlessM, whenJustM)
import Data.HashMap.Strict                       qualified as M
import Data.HashSet                              qualified as S
import Data.List                                 qualified as L
import Data.List.Extra                           (zipWithLongest)
import Data.List.NonEmpty                        qualified as NE

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Representations.Tokens
import Lang.Pietre.Stages.Analysis.Context
import Lang.Pietre.Stages.Analysis.Diagnostic
import Lang.Pietre.Stages.Analysis.Instantiation
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
  :: MonadDiagnostic m
  => HashMap ModuleName (HashSet Identifier)
  -> [Import]
  -> m (HashMap Path (NonEmpty Role))
createForeignScope moduleExports imports = do
  -- for each imported module, we create a hashmap
  -- from module name to hashmap of path to non-empty list:
  -- the hashmap of imported paths, grouped by module
  knownSymbols :: [HashMap ModuleName (HashMap Path (NonEmpty Role))] <-
    for imports \Import {..} -> do
      exportedIdentifiers <-
        M.lookup _importPath moduleExports `onNothing`
          fatal (ErrorImportPath _importPath)
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
              fatal $ ErrorImportSymbol _importPath identifier
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
    foldl' (M.unionWith (<>)) M.empty knownSymbols

createLocalScope
  :: MonadDiagnostic m
  => ModuleName
  -> [Annotated Definition Parsed]
  -> m
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
  let diagnostics = do
        (identifier, entries) <- M.toList topLevelNames
        let defs = entries <&> \((_, definition), _) -> definition
        guard $ NE.length defs > 1
        pure $ ErrorMultipleDeclaration identifier $ fmap _location defs
  unless (null diagnostics) do
    traverse_ report diagnostics
    abort

  -- create all local maps
  let exports = M.keys topLevelNames
  let (localDefinitions, localScope) = unzip $ map NE.head $ M.elems topLevelNames
  pure ( S.fromList exports
       , M.fromList localDefinitions
       , M.fromList (concat localScope)
       )

setTypeParameters
  :: [Identifier]
  -> AnalysisM ()
setTypeParameters parameters = do
  typeNames <-
    fmap M.fromList $
      for (group $ sort parameters) \case
        []               -> error "ICE"
        (identifier:_:_) -> fatal $ ErrorDuplicateTypeParameter identifier
        [identifier]     -> do
          when (isReserved identifier) $
            report $ ErrorReservedIdentifier identifier
          declName <- use contextCurrent
          let parameterRole = TypeParameter declName identifier
          whenJustM (lookupRole identifier) \names ->
            report $ WarningNameShadow names parameterRole
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
  (TypeParameter n1 t1, TypeParameter n2 t2) ->
    if t1 == t2 && n1 == n2 then similar else different
  (Placeholder, Placeholder) ->
    similar
  (FunctionPointer t1, FunctionPointer t2) ->
    let args1 = map (functionArgType . snd) $ _funtypeArgs t1
        args2 = map (functionArgType . snd) $ _funtypeArgs t2
        return1 = fromMaybe UnitType $ _funtypeReturn t1
        return2 = fromMaybe UnitType $ _funtypeReturn t2
    in concat (zipWith typeDiff args1 args2) ++ typeDiff return1 return2
  _ ->
    different
  where
    different = [(p1, p2)]
    similar =
      concatMap (uncurry typeDiff) $
      zipWithLongest defaultToPlaceholder (_pathParams p1) (_pathParams p2)
    defaultToPlaceholder l r =
      ( fromMaybe PlaceholderType l
      , fromMaybe PlaceholderType r
      )

typeMatches
  :: PathInfo Resolved
  -> PathInfo Resolved
  -> Bool
typeMatches p1 p2 = case (_pathName p1, _pathName p2) of
  (BuiltinType "!void", _) -> True
  (_, BuiltinType "!void") -> True
  (TypeParameter _ _, _) -> True
  (_, TypeParameter _ _) -> True
  (Placeholder, _) -> True
  (_, Placeholder) -> True
  (TopLevelDeclaration t1, TopLevelDeclaration t2) ->
    t1 == t2 && argMatches
  (BuiltinType t1, BuiltinType t2) ->
    t1 == t2 && argMatches
  (FunctionPointer t1, FunctionPointer t2) ->
    let args1 = map snd $ _funtypeArgs t1
        args2 = map snd $ _funtypeArgs t2
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
      TypeParameter _ _ -> 1 :: Int
      _                 -> sum $ map numberOfParameters _pathParams


--------------------------------------------------------------------------------
-- Analysis

analyzeDefinition
  :: WithLocation (Definition Parsed)
  -> AnalysisM (Maybe (Definition Resolved))
analyzeDefinition definition = do
  moduleName <- view infoModuleName
  let exportedIdentifiers = definitionIdentifiers $ _located definition
      rootName = Name (moduleName <> pure (NE.head exportedIdentifiers)) []
  exportedNames <- for exportedIdentifiers \identifier -> do
    when (isReserved identifier) $
      report $ ErrorReservedIdentifier identifier
    pure $ Name (moduleName <> pure identifier) []
  uses contextCache (M.lookup rootName) >>= \case
    Just def -> pure $ _located <$> def
    Nothing -> do
      let defLocation = _location definition
      contextLocation .= defLocation
      topLevelScope <- view infoTopLevelScope
      resolvedDefinition <-
        withLocalState topLevelScope do
          defCycle <- views infoStack (S.member rootName)
          if defCycle
          then fatal (ErrorCyclicDefinition rootName)
          else local (infoStack %~ S.insert rootName) do
            contextCurrent .= rootName
            case _located definition of
              TypeAliasDef info -> TypeAliasDef <$> analyzeTypeAlias info
              StructDef    info -> StructDef    <$> analyzeStruct    info
              EnumDef      info -> EnumDef      <$> analyzeEnum      info
              ConstDef     info -> ConstDef     <$> analyzeConst     info
              FunctionDef  info -> FunctionDef  <$> analyzeFunction  info
      contextCache <>= M.fromList do
        name <- NE.toList exportedNames
        pure (name, WithLocation defLocation <$> resolvedDefinition)
      pure resolvedDefinition

analyzeTypeAlias
  :: TypeAliasInfo Parsed
  -> AnalysisM (TypeAliasInfo Resolved)
analyzeTypeAlias TypeAliasInfo {..} = do
  setTypeParameters _aliasParams
  resolved <- resolveType (ForbidPlaceholder "type alias definition") _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolved

analyzeStruct
  :: StructInfo Parsed
  -> AnalysisM (StructInfo Resolved)
analyzeStruct info = do
  setTypeParameters (_structParams info)
  structValues ((traverse . traverse) (resolveType $ ForbidPlaceholder "struct fields definition")) info

analyzeEnum
  :: EnumInfo Parsed
  -> AnalysisM (EnumInfo Resolved)
analyzeEnum EnumInfo {..} =
  pure $ EnumInfo _enumName _enumValues

analyzeConst
  :: ConstInfo Parsed
  -> AnalysisM (ConstInfo Resolved)
analyzeConst ConstInfo {..} = do
  resolvedType <- resolveType (ForbidPlaceholder "const definition") _constType
  resolvedExpr <- go _constExpr
  unless (resolvedType `typeMatches` _exprType resolvedExpr) $
    report $ ErrorWrongType [resolvedType] (_exprType resolvedExpr)
  pure $ ConstInfo _constName resolvedType resolvedExpr
  where
    binaryIntExpression
      :: (Int -> Int -> AnalysisM Int)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    binaryIntExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression _ IntType (IntLiteralExpr x) -> pure x
        TypedExpression _ e1t _ ->
          fatal $ ErrorWrongType [IntType] e1t
      rhs <- go e2 >>= \case
        TypedExpression _ IntType (IntLiteralExpr x) -> pure x
        TypedExpression _ e2t _ ->
          fatal $ ErrorWrongType [IntType] e2t
      IntExpression <$> f lhs rhs

    binaryBoolExpression
      :: (Bool -> Bool -> Bool)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    binaryBoolExpression f e1 e2 = do
      lhs <- go e1 >>= \case
        TypedExpression _ BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression _ e1t _ ->
          fatal $ ErrorWrongType [BoolType] e1t
      rhs <- go e2 >>= \case
        TypedExpression _ BoolType (BoolLiteralExpr x) -> pure x
        TypedExpression _ e2t _ ->
          fatal $ ErrorWrongType [BoolType] e2t
      pure $ BoolExpression (f lhs rhs)

    comparisonExpression
      :: (forall a. Ord a => a -> a -> Maybe Bool)
      -> Bool
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    comparisonExpression op defaultCase e1 e2 = do
      TypedExpression _ t1 r1 <- go e1
      TypedExpression _ t2 r2 <- go e2
      unless (t1 `typeMatches` t2) $
        report $ ErrorWrongType [t1] t2
      BoolExpression . fromMaybe defaultCase <$> compareExpressions op r1 r2

    compareExpect
      :: Ord a
      => (Ordering -> Bool)
      -> a
      -> a
      -> Maybe Bool
    compareExpect checkOrdering x y =
      case compare x y of
        EQ -> Nothing
        o  -> Just $ checkOrdering o

    compareExpressions
      :: (forall a. Ord a => a -> a -> Maybe Bool)
      -> Expression Resolved
      -> Expression Resolved
      -> AnalysisM (Maybe Bool)
    compareExpressions op e1 e2 =
      case (e1, e2) of
        (StructExpr structType lhsFields, StructExpr _ rhsFields) -> do
          (structInfo, _) <- checkStructType structType `onNothingM`
            error "ICE"
          comparisons <- for (_structValues structInfo) \(fieldName, _) -> do
            (_, lhsField) <-
              find ((fieldName ==) . fst) lhsFields `onNothing`
                fatal (ErrorFieldAccessFieldNotFound structType fieldName)
            (_, rhsField) <-
              find ((fieldName ==) . fst) rhsFields `onNothing`
                fatal (ErrorFieldAccessFieldNotFound structType fieldName)
            compareExpressions op (_exprValue lhsField) (_exprValue rhsField)
          pure $ asum comparisons
        (IntLiteralExpr  i1, IntLiteralExpr  i2) -> pure $ op i1 i2
        (CharLiteralExpr c1, CharLiteralExpr c2) -> pure $ op c1 c2
        (BoolLiteralExpr b1, BoolLiteralExpr b2) -> pure $ op b1 b2
        _ -> error "ICE"

    go
      :: WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    go WithLocation {..} = do
      contextLocation .= _location
      case _located of
        PathExpr p ->
          resolveConstValue p
        CastExpr e t  -> do
          resolvedExpr <- go e
          resolvedTargetType <- resolveType (ForbidPlaceholder "cast expression") t
          let resolvedSourceType = _exprType resolvedExpr
              reportCastError :: forall a. AnalysisM a
              reportCastError = fatal $ ErrorWrongCast resolvedSourceType resolvedTargetType
          targetEnum <- lookupEnumType resolvedTargetType
          sourceEnum <- lookupEnumType resolvedSourceType
          case (sourceEnum, targetEnum) of
            (Just _, Just destEnum) -> do
              intValue <- case _exprValue resolvedExpr of
                IntLiteralExpr  i -> pure i
                _                 -> error "ICE"
              when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                report $ ErrorEnumOutOfBounds destEnum intValue
              pure $ RValueExpression resolvedTargetType $ IntLiteralExpr intValue
            (Nothing, Just destEnum) -> do
              intValue <- case _exprValue resolvedExpr of
                IntLiteralExpr  i -> pure i
                CharLiteralExpr c -> pure $ ord c
                BoolLiteralExpr b -> pure $ fromEnum b
                _                 -> reportCastError
              when (intValue < 0 || intValue >= length (_enumValues destEnum)) $
                report $ ErrorEnumOutOfBounds destEnum intValue
              pure $ RValueExpression resolvedTargetType $ IntLiteralExpr intValue
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
          TypedExpression _ structType structValue <- go expr
          case structValue of
            StructExpr _ fields -> do
              fmap snd $
                find ((field ==) . fst) fields `onNothing`
                  fatal (ErrorFieldAccessFieldNotFound structType field)
            _ -> fatal $ ErrorFieldAccessNotAStruct structType
        CallExpr _ _  -> fatal undefined
        ArrayExpr _ -> undefined
        IndexExpr _ _ -> undefined
        StructExpr path fields -> do
          (resolvedType, structDetails) <- resolveStructType path
          (structInfo, paramMapping) <- structDetails
            `onNothing` error "ICE"
          resolvedFields <- (traverse . traverse) go fields
          fullyResolvedType <- analyzeStructFields resolvedType structInfo paramMapping resolvedFields
          pure $ RValueExpression fullyResolvedType $ StructExpr fullyResolvedType resolvedFields
        IntLiteralExpr    i -> pure $ IntExpression  i
        BoolLiteralExpr   b -> pure $ BoolExpression b
        CharLiteralExpr   c -> pure $ CharExpression c
        StringLiteralExpr s -> pure $ RValueExpression undefined $ StringLiteralExpr s
        ReferenceExpr _ -> fatal undefined
        BoolNegationExpr e -> go e >>= \case
          BoolExpression x -> pure $ BoolExpression (not x)
          TypedExpression _ t _ -> fatal $ ErrorWrongType [BoolType] t
        IntNegationExpr e -> go e >>= \case
          IntExpression x -> pure $ IntExpression (-x)
          TypedExpression _ t _ -> fatal $ ErrorWrongType [IntType] t
        AdditionExpr e1 e2 -> do
          lhs <- go e1
          case lhs of
            TypedExpression _ IntType (IntLiteralExpr _) -> pure ()
            TypedExpression _ e1t _ -> report $ ErrorWrongType [IntType] e1t
          rhs <- go e2
          case rhs of
            TypedExpression _ IntType (IntLiteralExpr _) -> pure ()
            TypedExpression _ e2t _ -> report $ ErrorWrongType [IntType] e2t
          unless (_exprType lhs `typeMatches` _exprType rhs) $
            report $ ErrorWrongType [_exprType lhs] (_exprType rhs)
          validate
          case (_exprValue lhs, _exprValue rhs) of
            (IntLiteralExpr x, IntLiteralExpr y) -> pure $ IntExpression (x + y)
            _                                    -> error "ICE"
        SubtractionExpr    e1 e2 -> binaryIntExpression (pure ... subtract) e1 e2
        MultiplicationExpr e1 e2 -> binaryIntExpression (pure ... (*)) e1 e2
        ExponentiationExpr e1 e2 -> binaryIntExpression safeExp e1 e2
        DivisionExpr       e1 e2 -> binaryIntExpression (safeDiv div) e1 e2
        ModuloExpr         e1 e2 -> binaryIntExpression (safeDiv mod) e1 e2
        EqualityExpr       e1 e2 -> comparisonExpression (compareExpect (const False)) True  e1 e2
        DifferenceExpr     e1 e2 -> comparisonExpression (compareExpect (const True )) False e1 e2
        GreaterExpr        e1 e2 -> comparisonExpression (compareExpect (== GT)) False e1 e2
        LesserExpr         e1 e2 -> comparisonExpression (compareExpect (== LT)) False e1 e2
        GreaterEqExpr      e1 e2 -> comparisonExpression (compareExpect (== GT)) True  e1 e2
        LesserEqExpr       e1 e2 -> comparisonExpression (compareExpect (== LT)) True  e1 e2
        BoolAndExpr        e1 e2 -> binaryBoolExpression (&&) e1 e2
        BoolOrExpr         e1 e2 -> binaryBoolExpression (||) e1 e2
        RangeInclusiveExpr           _ _ -> undefined
        RangeExclusiveExpr           _ _ -> undefined
        AssignmentExpr               _ _ -> fatal undefined
        AdditionAssignmentExpr       _ _ -> fatal undefined
        SubtractionAssignmentExpr    _ _ -> fatal undefined
        MultiplicationAssignmentExpr _ _ -> fatal undefined
        DivisionAssignmentExpr       _ _ -> fatal undefined
        ModuloAssignmentExpr         _ _ -> fatal undefined
        ExponentiationAssignmentExpr _ _ -> fatal undefined

analyzeStructFields
  :: PathInfo Resolved
  -> StructInfo Resolved
  -> HashMap Identifier (PathInfo Resolved)
  -> NonEmpty (Identifier, TypedExpression)
  -> AnalysisM (PathInfo Resolved)
analyzeStructFields typeName StructInfo {..} paramMapping values = do
  let referenceMap = M.fromList $ NE.toList _structValues
      valuesMap    = M.fromListWith (<>) $ NE.toList $ (fmap . fmap) pure values
  for_ values \(identifier, _) -> do
    unless (M.member identifier referenceMap) $
      report $ ErrorStructUnknownField typeName identifier
  allDiffs <- for _structValues \(fieldName, fieldType) -> do
    case fold $ M.lookup fieldName valuesMap of
      []      -> do
        report $ ErrorStructMissingField typeName fieldName
        pure []
      (_:_:_) -> do
        report $ ErrorStructDuplicatedField typeName fieldName
        pure []
      [expr]  -> do
        catMaybes <$>
          for (typeDiff fieldType (_exprType expr)) \(lhs, rhs) -> do
            case (_pathName lhs, _pathName rhs) of
              (TypeParameter _ paramName, _) -> do
                typePattern <- M.lookup paramName paramMapping `onNothing` error "ICE"
                unless (typePattern `typeMatches` rhs) $
                  report $ ErrorWrongType [typePattern] rhs
                pure $ Just (paramName, pure rhs)
              (_, TypeParameter _ _) ->
                pure Nothing
              _ -> do
                report $ ErrorWrongType [fieldType] (_exprType expr)
                pure Nothing
  let tempMapping :: HashMap Identifier (NonEmpty (PathInfo Resolved))
      tempMapping = M.unionWith (<>) (filterMapping paramMapping) (M.fromListWith (<>) $ concat $ NE.toList allDiffs)
  resolvedTypeParams <- for _structParams \paramName -> do
    possibleTypes <- M.lookup paramName tempMapping `onNothing`
      fatal (ErrorStructAmbiguousType typeName paramName)
    unless (typesAllMatch $ NE.toList possibleTypes) $
      report $ ErrorStructIncompatibleTypes typeName paramName possibleTypes
    pure $ mostSpecificType possibleTypes
  validate
  pure (typeName & pathParams .~ resolvedTypeParams)

analyzeFunction
  :: FunctionInfo Parsed
  -> AnalysisM (FunctionInfo Resolved)
analyzeFunction FunctionInfo {..} = do
  setTypeParameters _funParams
  resolvedType <- traverse (resolveType $ ForbidPlaceholder "function definition") _funReturn
  let argNames = M.fromListWith (+) do
        (argName, _) <- _funArgs
        pure (argName, 1 :: Int)
  for_ (M.toList argNames) \(argName, argCount) -> do
    when (argCount > 1) $
      report $ ErrorFunctionDuplicatedArg argName
  resolvedArgs <- traverse analyzeArg _funArgs
  contextFunType .= fromMaybe UnitType resolvedType
  validate
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
        report $ ErrorReservedIdentifier argName
      let resolvedRole = FunctionArgument argName resolvedType
      whenJustM (lookupRole argName) \names ->
        report $ WarningNameShadow names resolvedRole
      contextScope %= M.insert (pure argName) (pure resolvedRole)
      pure (argName, resolvedType)

analyzeBlock
  :: [WithLocation (Statement Parsed)]
  -> AnalysisM [Statement Resolved]
analyzeBlock statements = do
  previousScope <- use contextScope
  result <- fold <$> try (traverse analyzeStatement statements)
  contextScope .= previousScope
  pure result

analyzeStatement
  :: WithLocation (Statement Parsed)
  -> AnalysisM (Statement Resolved)
analyzeStatement statement = do
  contextLocation .= _location statement
  case _located statement of
    ContinueStmt -> do
      unlessM (use contextWithinLoop) $
        report ErrorContinueNotInLoop
      pure ContinueStmt
    BreakStmt -> do
      unlessM (use contextWithinLoop) $
        report ErrorBreakNotInLoop
      pure BreakStmt
    ReturnStmt returnExpr -> do
      resolvedReturnExpr <- traverse analyzeFunctionExpression returnExpr
      let returnType = maybe UnitType _exprType resolvedReturnExpr
      funReturnType <- use contextFunType
      unless (funReturnType `typeMatches` returnType) $
        report $ ErrorWrongType [funReturnType] returnType
      pure $ ReturnStmt resolvedReturnExpr
    ExpressionStmt parsedExpr -> do
      resolvedExpr <- analyzeFunctionExpression parsedExpr
      unless (_exprType resolvedExpr `typeMatches` UnitType) $
        case _exprValue resolvedExpr of
          CallExpr                     _ _ -> pure ()
          AssignmentExpr               _ _ -> pure ()
          AdditionAssignmentExpr       _ _ -> pure ()
          SubtractionAssignmentExpr    _ _ -> pure ()
          MultiplicationAssignmentExpr _ _ -> pure ()
          DivisionAssignmentExpr       _ _ -> pure ()
          ModuloAssignmentExpr         _ _ -> pure ()
          ExponentiationAssignmentExpr _ _ -> pure ()
          e                                -> report $ WarningUnexpectedTopLevelExpression e
      pure $ ExpressionStmt resolvedExpr
    LetStmt LetInfo {..} -> do
      resolvedExpr <- analyzeFunctionExpression _letExpr
      resolvedType <- for _letType \parsedType -> do
        resolvedType <- resolveType AllowPlaceholder parsedType
        unless (resolvedType `typeMatches` _exprType resolvedExpr) $
          fatal $ ErrorWrongType [resolvedType] $ _exprType resolvedExpr
        pure resolvedType
      when (isReserved _letName) $
        fatal $ ErrorReservedIdentifier _letName
      let resolvedRole = LetVariable _letName $ _exprType resolvedExpr
      whenJustM (lookupRole _letName) \names ->
        report $ WarningNameShadow names resolvedRole
      contextScope %= M.insert (pure _letName) (pure resolvedRole)
      pure $ LetStmt $ LetInfo _letName resolvedType resolvedExpr
    IfStmt ifInfo -> IfStmt <$> analyzeIf ifInfo
    WhileStmt WhileInfo {..} -> do
      resolvedExpr <- analyzeFunctionExpression _whileExpr
      unless (_exprType resolvedExpr `typeMatches` BoolType) $
        report $ ErrorWhileExprNotBoolean $ _exprType resolvedExpr
      wasInLoop <- use contextWithinLoop
      contextWithinLoop .= True
      resolvedBody <- analyzeBlock _whileBody
      contextWithinLoop .= wasInLoop
      validate
      pure $ WhileStmt $ WhileInfo resolvedExpr resolvedBody
    ForStmt ForInfo {..} -> do
      resolvedExpr <- analyzeFunctionExpression _forRangeExpr
      -- TODO: validate range / vector / array type
      innerType <- error "ranges not implemented yet"
      when (isReserved _forVariableName) $
        fatal $ ErrorReservedIdentifier _forVariableName
      -- TODO: document this use of LetVariable
      let resolvedRole = LetVariable _forVariableName innerType
      whenJustM (lookupRole _forVariableName) \names ->
        report $ WarningNameShadow names resolvedRole
      previousScope <- use contextScope
      wasInLoop <- use contextWithinLoop
      contextWithinLoop .= True
      contextScope %= M.insert (pure _forVariableName) (pure resolvedRole)
      resolvedBody <- fold <$> try (traverse analyzeStatement _forBody)
      contextScope .= previousScope
      contextWithinLoop .= wasInLoop
      pure $ ForStmt $ ForInfo _forVariableName resolvedExpr resolvedBody

analyzeIf
  :: IfInfo Parsed
  -> AnalysisM (IfInfo Resolved)
analyzeIf IfInfo {..} = do
  resolvedExpr <- analyzeFunctionExpression _ifExpr
  unless (_exprType resolvedExpr `typeMatches` BoolType) $
    report $ ErrorIfExprNotBoolean $ _exprType resolvedExpr
  resolvedBody <- analyzeBlock _ifBody
  resolvedElse <- traverse analyzeElse _ifElse
  validate
  pure $ IfInfo resolvedExpr resolvedBody resolvedElse

analyzeElse
  :: ElseInfo Parsed
  -> AnalysisM (ElseInfo Resolved)
analyzeElse = \case
  ElseIf    ifInfo -> ElseIf    <$> analyzeIf    ifInfo
  ElseBlock block  -> ElseBlock <$> analyzeBlock block

analyzeFunctionCallArgs
  :: PathInfo Resolved
  -> FunctionType Resolved
  -> HashMap Identifier (PathInfo Resolved)
  -> [TypedExpression]
  -> AnalysisM (PathInfo Resolved, HashMap Identifier (PathInfo Resolved))
analyzeFunctionCallArgs resolvedPath FunctionType {..} paramMapping values = do
  allDiffs <- sequence . concat <$>
    for (zip _funtypeArgs values) \((argName, argType), expr) -> do
      case (argType, _exprValue expr) of
        (ByReference _, ReferenceExpr _) -> pure ()
        (ByReference _, _)               -> report $ ErrorFunctionCallArgExpectingReference argName
        (ByValue _,     ReferenceExpr _) -> report $ ErrorFunctionCallArgExpectingValue     argName
        (ByValue _,     _)               -> pure ()
      let fieldType = functionArgType argType
      for (typeDiff fieldType (_exprType expr)) \(lhs, rhs) -> do
        case (_pathName lhs, _pathName rhs) of
          (TypeParameter _ paramName, _) -> do
            typePattern <- M.lookup paramName paramMapping `onNothing`
              error "ICE"
            unless (typePattern `typeMatches` rhs) $
              report $ ErrorWrongType [typePattern] rhs
            pure $ Just (paramName, pure rhs)
          (_, TypeParameter _ _) ->
            pure Nothing
          _ -> do
            report $ ErrorWrongType [fieldType] (_exprType expr)
            pure Nothing
  validate
  case allDiffs of
    Nothing ->
      pure (resolvedPath, paramMapping)
    Just ds -> do
      let tempMapping :: HashMap Identifier [PathInfo Resolved]
          tempMapping = M.unionWith (<>) (filterMapping paramMapping) (M.fromListWith (<>) ds)
      resolvedTypeParams <- for _funtypeParams \paramName -> do
        possibleTypes <- M.lookup paramName tempMapping `onNothing`
          fatal (ErrorFunctionAmbiguousType resolvedPath paramName)
        possibleTypesNE <- NE.nonEmpty possibleTypes `onNothing`
          fatal (ErrorFunctionAmbiguousType resolvedPath paramName)
        unless (typesAllMatch possibleTypes) $
          report $ ErrorFunctionIncompatibleTypes resolvedPath paramName possibleTypesNE
        pure $ mostSpecificType possibleTypesNE
      validate
      let newMapping = M.fromList $ zip _funtypeParams resolvedTypeParams
      pure (resolvedPath & pathParams .~ resolvedTypeParams, newMapping)

analyzeFunctionExpression
  :: WithLocation (Expression Parsed)
  -> AnalysisM TypedExpression
analyzeFunctionExpression expr = do
  contextLocation .= _location expr
  case _located expr of
    CastExpr e t -> do
      resolvedExpr <- analyzeFunctionExpression e
      resolvedTargetType <- resolveType (ForbidPlaceholder "cast expression") t
      let resolvedSourceType = _exprType resolvedExpr
          reportCastError :: forall a. AnalysisM a
          reportCastError = fatal $ ErrorWrongCast resolvedSourceType resolvedTargetType
          resultCast =
            RValueExpression resolvedTargetType $
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
              pure $ RValueExpression VoidType value
            _ -> case _pathName $ _exprType resolvedExpr of
              TypeParameter _ _ ->
                pure resultCast
              _ -> do
                reportCastError
        (Just _, Nothing) -> do
          case (resolvedTargetType, _exprValue resolvedExpr) of
            (IntType, IntLiteralExpr i) ->
              pure $ IntExpression i
            (IntType, value) ->
              pure $ RValueExpression IntType value
            (CharType, IntLiteralExpr i) ->
              pure $ CharExpression $ chr i
            (CharType, value) ->
              pure $ RValueExpression CharType value
            (BoolType, IntLiteralExpr i) ->
              pure $ BoolExpression $ i /= 0
            (BoolType, value) ->
              pure $ RValueExpression BoolType value
            _ -> case _pathName resolvedTargetType of
                TypeParameter _ _ ->
                  pure resultCast
                _ -> do
                  reportCastError
        (Nothing, Nothing) -> do
          case (resolvedTargetType, resolvedSourceType, _exprValue resolvedExpr) of
            (IntType,  _, IntLiteralExpr  i) -> pure $ IntExpression i
            (IntType,  _, CharLiteralExpr c) -> pure $ IntExpression $ ord c
            (IntType,  _, BoolLiteralExpr b) -> pure $ IntExpression $ fromEnum b
            (IntType,  IntType,  value)      -> pure $ RValueExpression IntType value
            (IntType,  CharType, value)      -> pure $ RValueExpression IntType value
            (IntType,  BoolType, value)      -> pure $ RValueExpression IntType value
            (CharType, _, IntLiteralExpr  i) -> pure $ CharExpression $ chr i
            (CharType, _, CharLiteralExpr c) -> pure $ CharExpression c
            (CharType, IntType,  value)      -> pure $ RValueExpression CharType value
            (CharType, CharType, value)      -> pure $ RValueExpression CharType value
            (BoolType, _, IntLiteralExpr  i) -> pure $ BoolExpression $ i /= 0
            (BoolType, _, CharLiteralExpr c) -> pure $ BoolExpression $ ord c > 0
            (BoolType, _, BoolLiteralExpr b) -> pure $ BoolExpression b
            (BoolType, IntType,  value)      -> pure $ RValueExpression BoolType value
            (BoolType, CharType, value)      -> pure $ RValueExpression BoolType value
            (BoolType, BoolType, value)      -> pure $ RValueExpression BoolType value
            _ -> case ( _pathName (_exprType resolvedExpr)
                      , _pathName resolvedTargetType
                      ) of
              (TypeParameter _ _, _) -> pure resultCast
              (_, TypeParameter _ _) -> pure resultCast
              _                      -> reportCastError
    PathExpr p ->
      resolveExprValue p
    FieldAccessExpr subExpr field -> do
      lhs@(TypedExpression lValue structType structValue) <- analyzeFunctionExpression subExpr
      case structValue of
        StructExpr _ fields -> do
          fmap snd $
            find ((field ==) . fst) fields `onNothing`
              fatal (ErrorFieldAccessFieldNotFound structType field)
        _ -> checkStructType structType >>= \case
          Nothing ->
            pure $ TypedExpression lValue structType $ FieldAccessExpr lhs field
          Just (structInfo, paramMapping) -> do
            (_, fieldType) <- find ((field ==) . fst) (_structValues structInfo) `onNothing`
              fatal (ErrorFieldAccessFieldNotFound structType field)
            resolvedFieldType <- substituteTypes paramMapping fieldType
            pure $ TypedExpression lValue resolvedFieldType $ FieldAccessExpr lhs field
    CallExpr funPath arguments  -> do
      (resolvedPath, funType, mapping) <- resolveFunctionCallValue funPath
      resolvedArgs <- traverse analyzeFunctionExpression arguments
      let expected = length (_funtypeArgs funType)
          actual   = length resolvedArgs
      when (expected /= actual) $
        report $ ErrorFunctionCallWrongNumberOfArguments resolvedPath expected actual
      (fullyResolvedPath, fullMapping) <- analyzeFunctionCallArgs resolvedPath funType mapping resolvedArgs
      -- TODO: register function for instantiation
      returnType <- substituteTypes fullMapping $ fromMaybe UnitType (_funtypeReturn funType)
      pure $ RValueExpression returnType $ CallExpr fullyResolvedPath resolvedArgs
    IntLiteralExpr    i -> pure $ IntExpression  i
    BoolLiteralExpr   b -> pure $ BoolExpression b
    CharLiteralExpr   c -> pure $ CharExpression c
    StringLiteralExpr s -> pure $ RValueExpression undefined $ StringLiteralExpr s
    ArrayExpr _   -> undefined
    IndexExpr _ _ -> undefined
    StructExpr path fields -> do
      (resolvedType, structDetails) <- resolveStructType path
      rawResolvedFields <- (traverse . traverse) analyzeFunctionExpression fields
      let resolvedFields = rawResolvedFields & traverse . traverse . exprIsLValue .~ False
      fullyResolvedType <- case structDetails of
        Nothing ->
          pure resolvedType
        Just (structInfo, paramMapping) ->
          analyzeStructFields resolvedType structInfo paramMapping resolvedFields
      pure $ RValueExpression fullyResolvedType $ StructExpr fullyResolvedType resolvedFields
    ReferenceExpr path -> do
      TypedExpression _ resolvedType resolvedValue <- resolveExprValue path
      resolvedPath <- case resolvedValue of
        PathExpr res@(PathInfo (FunctionArgument _ _) _) -> pure res
        PathExpr res@(PathInfo (LetVariable      _ _) _) -> pure res
        _ -> fatal $ ErrorReferenceNotLocalVariable resolvedValue
      pure $ RValueExpression resolvedType (ReferenceExpr resolvedPath)
    BoolNegationExpr e -> analyzeFunctionExpression e >>= \case
      BoolExpression x ->
        pure $ BoolExpression (not x)
      inner@(TypedExpression _ resolvedType _) -> do
        unless (resolvedType `typeMatches` BoolType) $
          fatal $ ErrorWrongType [BoolType] resolvedType
        pure $ RValueExpression BoolType (BoolNegationExpr inner)
    IntNegationExpr e -> analyzeFunctionExpression e >>= \case
      IntExpression x ->
        pure $ IntExpression (-x)
      inner@(TypedExpression _ resolvedType _) -> do
        unless (resolvedType `typeMatches` IntType) $
          fatal $ ErrorWrongType [IntType] resolvedType
        pure $ RValueExpression IntType (IntNegationExpr inner)
    AdditionExpr e1 e2 -> do
      lhs <- analyzeFunctionExpression e1
      rhs <- analyzeFunctionExpression e2
      case (lhs, rhs) of
        (IntExpression l, IntExpression r) ->
          pure $ IntExpression (l + r)
        -- (StringExpression l, StringExpression r) ->
        --   pure $ StringExpression (l ++ r)
        _ -> do
          if | _exprType lhs `typeMatches` IntType -> do
                 unless (_exprType rhs `typeMatches` IntType) $
                   fatal $ ErrorWrongType [IntType] $ _exprType rhs
                 pure $ RValueExpression IntType $ AdditionExpr lhs rhs
             | otherwise -> do
                 fatal $ ErrorWrongType [IntType {-, StringType -}] $ _exprType lhs
    SubtractionExpr    e1 e2 -> binaryIntExpression  (pure ... subtract) SubtractionExpr    e1 e2
    MultiplicationExpr e1 e2 -> binaryIntExpression  (pure ... (*))      MultiplicationExpr e1 e2
    ExponentiationExpr e1 e2 -> binaryIntExpression  safeExp             ExponentiationExpr e1 e2
    DivisionExpr       e1 e2 -> binaryIntExpression  (safeDiv div)       DivisionExpr       e1 e2
    ModuloExpr         e1 e2 -> binaryIntExpression  (safeDiv mod)       ModuloExpr         e1 e2
    BoolAndExpr        e1 e2 -> binaryBoolExpression (&&)                BoolAndExpr        e1 e2
    BoolOrExpr         e1 e2 -> binaryBoolExpression (||)                BoolOrExpr         e1 e2
    EqualityExpr       e1 e2 -> comparisonExpression (compareExpect (const False)) EqualityExpr   True  e1 e2
    DifferenceExpr     e1 e2 -> comparisonExpression (compareExpect (const True )) DifferenceExpr False e1 e2
    GreaterExpr        e1 e2 -> comparisonExpression (compareExpect (== GT))       GreaterExpr    False e1 e2
    LesserExpr         e1 e2 -> comparisonExpression (compareExpect (== LT))       LesserExpr     False e1 e2
    GreaterEqExpr      e1 e2 -> comparisonExpression (compareExpect (== GT))       GreaterEqExpr  True  e1 e2
    LesserEqExpr       e1 e2 -> comparisonExpression (compareExpect (== LT))       LesserEqExpr   True  e1 e2
    RangeInclusiveExpr _  _  -> undefined
    RangeExclusiveExpr _  _  -> undefined
    AssignmentExpr     e1 e2 -> do
      lhs@(TypedExpression isLValue t1 r1) <- analyzeFunctionExpression e1
      rhs@(TypedExpression _ t2 _r2) <- analyzeFunctionExpression e2
      unless isLValue $
        fatal $ ErrorRValueAssignment r1
      unless (t1 `typeMatches` t2) $
        fatal $ ErrorWrongType [t1] t2
      pure $ RValueExpression UnitType $ AssignmentExpr lhs rhs
    AdditionAssignmentExpr       e1 e2 -> compoundAssignment AdditionAssignmentExpr       e1 e2
    SubtractionAssignmentExpr    e1 e2 -> compoundAssignment SubtractionAssignmentExpr    e1 e2
    MultiplicationAssignmentExpr e1 e2 -> compoundAssignment MultiplicationAssignmentExpr e1 e2
    DivisionAssignmentExpr       e1 e2 -> compoundAssignment DivisionAssignmentExpr       e1 e2
    ModuloAssignmentExpr         e1 e2 -> compoundAssignment ModuloAssignmentExpr         e1 e2
    ExponentiationAssignmentExpr e1 e2 -> compoundAssignment ExponentiationAssignmentExpr e1 e2
  where
    compoundAssignment exprCons e1 e2 = do
      lhs@(TypedExpression isLValue t1 r1) <- analyzeFunctionExpression e1
      rhs@(TypedExpression _ t2 _r2) <- analyzeFunctionExpression e2
      unless isLValue $
        fatal $ ErrorRValueAssignment r1
      unless (t1 `typeMatches` IntType) $
        fatal $ ErrorWrongType [IntType] t1
      unless (t2 `typeMatches` IntType) $
        fatal $ ErrorWrongType [IntType] t2
      pure $ RValueExpression UnitType $ exprCons lhs rhs

    compileTimeEnumCast enumType enumInfo intValue = do
      when (intValue < 0 || intValue >= length (_enumValues enumInfo)) $
        report $ ErrorEnumOutOfBounds enumInfo intValue
      pure $ RValueExpression enumType $ IntLiteralExpr intValue

    binaryIntExpression
      :: (Int -> Int -> AnalysisM Int)
      -> (TypedExpression -> TypedExpression -> Expression Resolved)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    binaryIntExpression f c e1 e2 = do
      lhs <- analyzeFunctionExpression e1
      rhs <- analyzeFunctionExpression e2
      case (lhs, rhs) of
        (IntExpression l, IntExpression r) ->
          IntExpression <$> f l r
        _ -> do
          unless (_exprType lhs `typeMatches` IntType) $
            report $ ErrorWrongType [IntType] $ _exprType lhs
          unless (_exprType rhs `typeMatches` IntType) $
            report $ ErrorWrongType [IntType] $ _exprType rhs
          validate
          pure $ RValueExpression IntType $ c lhs rhs

    binaryBoolExpression
      :: (Bool -> Bool -> Bool)
      -> (TypedExpression -> TypedExpression -> Expression Resolved)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    binaryBoolExpression f c e1 e2 = do
      lhs <- analyzeFunctionExpression e1
      rhs <- analyzeFunctionExpression e2
      case (lhs, rhs) of
        (BoolExpression l, BoolExpression r) ->
          pure $ BoolExpression $ f l r
        _ -> do
          unless (_exprType lhs `typeMatches` BoolType) $
            report $ ErrorWrongType [BoolType] $ _exprType lhs
          unless (_exprType rhs `typeMatches` BoolType) $
            report $ ErrorWrongType [BoolType] $ _exprType rhs
          validate
          pure $ RValueExpression BoolType $ c lhs rhs

    comparisonExpression
      :: (forall a. Ord a => a -> a -> Maybe Bool)
      -> (TypedExpression -> TypedExpression -> Expression Resolved)
      -> Bool
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    comparisonExpression op c defaultCase e1 e2 = do
      lhs@(TypedExpression _ t1 r1) <- analyzeFunctionExpression e1
      rhs@(TypedExpression _ t2 r2) <- analyzeFunctionExpression e2
      unless (t1 `typeMatches` t2) $
        report $ ErrorWrongType [t1] t2
      runMaybeT (compareExpressions op r1 r2) <&> \case
        Nothing -> RValueExpression BoolType $ c lhs rhs
        Just mb -> BoolExpression $ fromMaybe defaultCase mb

    compareExpect
      :: Ord a
      => (Ordering -> Bool)
      -> a
      -> a
      -> Maybe Bool
    compareExpect checkOrdering x y =
      case compare x y of
        EQ -> Nothing
        o  -> Just $ checkOrdering o

    compareExpressions
      :: (forall a. Ord a => a -> a -> Maybe Bool)
      -> Expression Resolved
      -> Expression Resolved
      -> MaybeT AnalysisM (Maybe Bool)
    compareExpressions op e1 e2 =
      case (e1, e2) of
        (StructExpr structType lhsFields, StructExpr _ rhsFields) -> do
          (structInfo, _) <- lift (checkStructType structType) `onNothingM`
            mzero
          comparisons <- for (_structValues structInfo) \(fieldName, _) -> do
            (_, lhsField) <- lift $
              find ((fieldName ==) . fst) lhsFields `onNothing`
                fatal (ErrorFieldAccessFieldNotFound structType fieldName)
            (_, rhsField) <- lift $
              find ((fieldName ==) . fst) rhsFields `onNothing`
                fatal (ErrorFieldAccessFieldNotFound structType fieldName)
            compareExpressions op (_exprValue lhsField) (_exprValue rhsField)
          pure $ asum comparisons
        (IntLiteralExpr  i1, IntLiteralExpr  i2) -> pure $ op i1 i2
        (CharLiteralExpr c1, CharLiteralExpr c2) -> pure $ op c1 c2
        (BoolLiteralExpr b1, BoolLiteralExpr b2) -> pure $ op b1 b2
        _ -> mzero


  {-
    comparisonExpression
      :: (forall a. Ord a => a -> a -> Bool)
      -> (TypedExpression -> TypedExpression -> Expression Resolved)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
    comparisonExpression op c e1 e2 = do
      lhs@TypedExpression t1 r1 <- go e1
      rhs@TypedExpression t2 r2 <- go e2
      unless (t1 `typeMatches` t2) $
        fatal $ ErrorWrongType [t1] t2
      compareExpressions op c r1 r2

    compareExpressions
      :: (forall a. Ord a => a -> a -> Bool)
      -> (TypedExpression -> TypedExpression -> TypedExpression)
      -> TypedExpression
      -> TypedExpression
      -> TypedExpression
    compareExpressions op c lhs rhs = do
      case (_exprValue lhs, _exprValue rhs) of
        (StructExpr _ f1, StructExpr _ f2) ->
          let sortedLHSFields = map snd $ L.sortOn fst $ NE.toList f1
              sortedRHSFields = map snd $ L.sortOn fst $ NE.toList f2
              innerExprs = zipWith (compareExpressions op c) sortedLHSFields sortedRHSFields
          in combineComparisons innerExprs
        (IntLiteralExpr  i1, IntLiteralExpr  i2) -> BoolLiteral $ op i1 i2
        (CharLiteralExpr c1, CharLiteralExpr c2) -> BoolLiteral $ op c1 c2
        (BoolLiteralExpr b1, BoolLiteralExpr b2) -> BoolLiteral $ op b1 b2
        _ -> c lhs rhs
    -}

    -- combineComparisons
    --   :: [TypedExpression]
    --   -> TypedExpression


{-
  where
    binaryIntExpression
      :: (Int -> Int -> Int)
      -> WithLocation (Expression Parsed)
      -> WithLocation (Expression Parsed)
      -> AnalysisM TypedExpression
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
      -> AnalysisM TypedExpression
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
      -> AnalysisM TypedExpression
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

filterMapping
  :: Applicative f
  => HashMap Identifier (PathInfo Resolved)
  -> HashMap Identifier (f (PathInfo Resolved))
filterMapping = M.mapMaybe \pathInfo ->
  case _pathName pathInfo of
    Placeholder -> Nothing
    _           -> Just $ pure pathInfo

safeDiv
  :: (Int -> Int -> Int)
  -> (Int -> Int -> AnalysisM Int)
safeDiv f x y = do
  when (y == 0) $ fatal ErrorDivideByZero
  pure $ f x y

safeExp :: Int -> Int -> AnalysisM Int
safeExp x y = do
  when (y < 0) $ fatal ErrorNegativeExponent
  pure $ x ^ y
