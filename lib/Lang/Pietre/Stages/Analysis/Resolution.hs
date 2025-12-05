module Lang.Pietre.Stages.Analysis.Resolution (resolve) where

import "this" Prelude

import Control.Lens                                 hiding (mapping, op)
import Data.Functor.Compose
import Data.HashMap.Strict.Extra                    qualified as M

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Parsed       as Parsed
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution.Monad
import Lang.Pietre.Stages.Analysis.Resolution.Scope


--------------------------------------------------------------------------------
-- API

resolve
  :: MonadDiagnosis m
  => HashMap ModuleName Interface
  -> ModuleName
  -> Module
  -> m ( HashMap Identifier Role
       , [(BaseName, WithLocation Resolved.Definition)]
       )
resolve dependencies moduleName Module {..} = do
  importedScope <- createImportedScope dependencies _modImports
  (exported, definitions, localScope) <- parseLocalDeclarations moduleName _modDefinitions
  let
    combineMaps = M.unionWith (<>)
    builtinScope = M.fromList $ map (fmap pure) builtins
    topLevelScope = builtinScope `combineMaps` importedScope `combineMaps` localScope
  resolvedDefinitions <- ensureNested $
    for definitions
      \(declarationBaseName, def) -> tryNested do
        let definitionLocation = _location def
        resolved <- runResolveT declarationBaseName topLevelScope definitionLocation $ resolveDefinition $ _located def
        pure (declarationBaseName, resolved <$ def)
  pure (exported, resolvedDefinitions)


--------------------------------------------------------------------------------
-- Implementation

resolveDefinition
  :: MonadDiagnosis m
  => Parsed.Definition
  -> ResolveT m Resolved.Definition
resolveDefinition = \case
  TypeAliasDef info -> TypeAliasDef <$> resolveTypeAlias info
  StructDef    info -> StructDef    <$> resolveStruct info
  ConstDef     info -> ConstDef     <$> resolveConst info
  FunctionDef  info -> FunctionDef  <$> resolveFunction info
  EnumDef      info -> pure $ EnumDef info

resolveTypeAlias
  :: MonadDiagnosis m
  => Parsed.TypeAliasInfo
  -> ResolveT m Resolved.TypeAliasInfo
resolveTypeAlias TypeAliasInfo {..} = do
  expandScopeWithTypeParameters _aliasParams
  resolvedValue <- resolvePath _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolvedValue

resolveStruct
  :: MonadDiagnosis m
  => Parsed.StructInfo
  -> ResolveT m Resolved.StructInfo
resolveStruct StructInfo {..} = do
  expandScopeWithTypeParameters _structParams
  resolvedValues <- ensureNested $ traverse2 (tryNested . resolvePath) _structValues
  pure $ StructInfo _structName _structParams resolvedValues

resolveConst
  :: MonadDiagnosis m
  => Parsed.ConstInfo
  -> ResolveT m Resolved.ConstInfo
resolveConst ConstInfo {..} = do
  resolvedType <- try $ resolvePath _constType
  resolvedExpr <- try $ resolveExpression _constExpr
  ensure $ liftA2 (ConstInfo _constName) resolvedType resolvedExpr

resolveFunction
  :: MonadDiagnosis m
  => Parsed.FunctionInfo
  -> ResolveT m Resolved.FunctionInfo
resolveFunction FunctionInfo {..} = do
  expandScopeWithTypeParameters $ _funParams _funType
  resolvedType <- resolveFunctionType _funType
  resolvedBody <-
    resolveBlock
      (expandScopeWithFunctionArguments $ _funArgs resolvedType)
      _funBody
  pure $ FunctionInfo _funName resolvedType resolvedBody

resolveFunctionType
  :: MonadDiagnosis m
  => Parsed.FunctionType
  -> ResolveT m Resolved.FunctionType
resolveFunctionType FunctionType {..} = do
  resolvedArgs   <- getCompose $ traverse2 (Compose . try . resolveFunctionArg) _funArgs
  resolvedReturn <- getCompose $ traverse  (Compose . try . resolvePath)        _funReturn
  ensure $ liftA2 (FunctionType _funParams) resolvedArgs resolvedReturn

resolveFunctionArg
  :: MonadDiagnosis m
  => Parsed.FunctionArgType
  -> ResolveT m Resolved.FunctionArgType
resolveFunctionArg = \case
  ByValue     path -> ByValue     <$> resolvePath path
  ByReference path -> ByReference <$> resolvePath path

resolveBlock
  :: MonadDiagnosis m
  => ResolveT m ()
  -> Parsed.Block
  -> ResolveT m Resolved.Block
resolveBlock updateScope statements =
  bracket
    setupBlock
    teardownBlock
    processBlock
  where
    setupBlock =
      use rcScope
    teardownBlock parentScope =
      rcScope .= parentScope
    processBlock _ = do
      updateScope
      ensureNested $
        for statements \stmt -> tryNested do
          rcLocation .= _location stmt
          traverse resolveStatement stmt

resolveStatement
  :: MonadDiagnosis m
  => Parsed.Statement
  -> ResolveT m Resolved.Statement
resolveStatement = \case
  IfStmt info ->
    IfStmt <$> resolveIf info
  ForStmt info ->
    ForStmt <$> resolveFor info
  WhileStmt info ->
    WhileStmt <$> resolveWhile info
  LetStmt info ->
    LetStmt <$> resolveLet info
  ReturnStmt info ->
    ReturnStmt <$> traverse resolveExpression info
  ContinueStmt ->
    pure ContinueStmt
  BreakStmt ->
    pure BreakStmt
  ExpressionStmt info ->
    ExpressionStmt <$> resolveExpression info

resolveIf
  :: MonadDiagnosis m
  => Parsed.IfInfo
  -> ResolveT m Resolved.IfInfo
resolveIf IfInfo {..} = do
  resolvedExpr <- try $ resolveExpression _ifExpr
  resolvedBody <- try $ resolveBlock pass _ifBody
  resolvedElse <- getCompose $ traverse (Compose . try . resolveElse) _ifElse
  ensure $ liftA3 IfInfo resolvedExpr resolvedBody resolvedElse

resolveElse
  :: MonadDiagnosis m
  => Parsed.ElseInfo
  -> ResolveT m Resolved.ElseInfo
resolveElse = \case
  ElseIf    info  -> ElseIf    <$> resolveIf info
  ElseBlock block -> ElseBlock <$> resolveBlock pass block

resolveFor
  :: MonadDiagnosis m
  => Parsed.ForInfo
  -> ResolveT m Resolved.ForInfo
resolveFor ForInfo {..} = do
  resolvedExpr <- try $ resolveExpression _forRangeExpr
  resolvedBody <- try $
    resolveBlock
      (expandScopeWithVariable _forVariableName)
      _forBody
  ensure $ liftA2 (ForInfo _forVariableName) resolvedExpr resolvedBody

resolveWhile
  :: MonadDiagnosis m
  => Parsed.WhileInfo
  -> ResolveT m Resolved.WhileInfo
resolveWhile WhileInfo {..} = do
  resolvedExpr <- try $ resolveExpression _whileExpr
  resolvedBody <- try $ resolveBlock pass _whileBody
  ensure $ liftA2 WhileInfo resolvedExpr resolvedBody

resolveLet
  :: MonadDiagnosis m
  => Parsed.LetInfo
  -> ResolveT m Resolved.LetInfo
resolveLet LetInfo {..} = do
  resolvedType <- getCompose $ traverse (Compose . try . resolvePath) _letType
  resolvedExpr <- try $ resolveExpression _letExpr
  expandScopeWithVariable _letName
  ensure $ liftA2 (LetInfo _letName) resolvedType resolvedExpr

resolveExpression
  :: MonadDiagnosis m
  => WithLocation Parsed.Expression
  -> ResolveT m (WithLocation Resolved.Expression)
resolveExpression expr = do
  rcLocation .= _location expr
  result <- case _located expr of
    PathExpr path ->
      PathExpr <$> resolvePath path
    FieldAccessExpr lhs rhs ->
      liftA2 FieldAccessExpr (resolveExpression lhs) (pure rhs)
    CallExpr lhs args -> do
      resolvedPath <- try $ resolvePath lhs
      resolvedArgs <- getCompose $ traverse (tryNested . resolveExpression) args
      ensure $ liftA2 CallExpr resolvedPath resolvedArgs
    ArrayExpr exprs ->
      ArrayExpr <$> traverse resolveExpression exprs
    IndexExpr lhs rhs ->
      liftA2 IndexExpr (resolveExpression lhs) (resolveExpression rhs)
    StructExpr path fields ->
      liftA2 StructExpr (resolvePath path) (traverse2 resolveExpression fields)
    BoolLiteralExpr b ->
      pure $ BoolLiteralExpr b
    IntLiteralExpr i ->
      pure $ IntLiteralExpr i
    CharLiteralExpr c ->
      pure $ CharLiteralExpr c
    StringLiteralExpr s ->
      pure $ StringLiteralExpr s
    ReferenceExpr path ->
      ReferenceExpr <$> resolvePath path
    IntNegationExpr e ->
      IntNegationExpr <$> resolveExpression e
    BoolNegationExpr e ->
      BoolNegationExpr <$> resolveExpression e
    AdditionExpr lhs rhs ->
      liftA2 AdditionExpr (resolveExpression lhs) (resolveExpression rhs)
    SubtractionExpr lhs rhs ->
      liftA2 SubtractionExpr (resolveExpression lhs) (resolveExpression rhs)
    MultiplicationExpr lhs rhs ->
      liftA2 MultiplicationExpr (resolveExpression lhs) (resolveExpression rhs)
    DivisionExpr lhs rhs ->
      liftA2 DivisionExpr (resolveExpression lhs) (resolveExpression rhs)
    ModuloExpr lhs rhs ->
      liftA2 ModuloExpr (resolveExpression lhs) (resolveExpression rhs)
    ExponentiationExpr lhs rhs ->
      liftA2 ExponentiationExpr (resolveExpression lhs) (resolveExpression rhs)
    EqualityExpr lhs rhs ->
      liftA2 EqualityExpr (resolveExpression lhs) (resolveExpression rhs)
    DifferenceExpr lhs rhs ->
      liftA2 DifferenceExpr (resolveExpression lhs) (resolveExpression rhs)
    GreaterExpr lhs rhs ->
      liftA2 GreaterExpr (resolveExpression lhs) (resolveExpression rhs)
    LesserExpr lhs rhs ->
      liftA2 LesserExpr (resolveExpression lhs) (resolveExpression rhs)
    GreaterEqExpr lhs rhs ->
      liftA2 GreaterEqExpr (resolveExpression lhs) (resolveExpression rhs)
    LesserEqExpr lhs rhs ->
      liftA2 LesserEqExpr (resolveExpression lhs) (resolveExpression rhs)
    BoolAndExpr lhs rhs ->
      liftA2 BoolAndExpr (resolveExpression lhs) (resolveExpression rhs)
    BoolOrExpr lhs rhs ->
      liftA2 BoolOrExpr (resolveExpression lhs) (resolveExpression rhs)
    CastExpr e castType ->
      liftA2 CastExpr (resolveExpression e) (resolvePath castType)
    RangeInclusiveExpr lhs rhs ->
      liftA2 RangeInclusiveExpr (resolveExpression lhs) (resolveExpression rhs)
    RangeExclusiveExpr lhs rhs ->
      liftA2 RangeExclusiveExpr (resolveExpression lhs) (resolveExpression rhs)
    AssignmentExpr lhs rhs ->
      liftA2 AssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    AdditionAssignmentExpr lhs rhs ->
      liftA2 AdditionAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    SubtractionAssignmentExpr lhs rhs ->
      liftA2 SubtractionAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    MultiplicationAssignmentExpr lhs rhs ->
      liftA2 MultiplicationAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    DivisionAssignmentExpr lhs rhs ->
      liftA2 DivisionAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    ModuloAssignmentExpr lhs rhs ->
      liftA2 ModuloAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
    ExponentiationAssignmentExpr lhs rhs ->
      liftA2 ExponentiationAssignmentExpr (resolveExpression lhs) (resolveExpression rhs)
  pure $ result <$ expr

resolvePath
  :: MonadDiagnosis m
  => Parsed.PathInfo
  -> ResolveT m Resolved.PathInfo
resolvePath PathInfo {..} = do
  resolvedName   <- try $ resolvePathBody _pathBase
  resolvedParams <- getCompose $ traverse (Compose . try . resolvePath) _pathParams
  ensure $ liftA2 PathInfo resolvedName resolvedParams

resolvePathBody
  :: MonadDiagnosis m
  => Path
  -> ResolveT m Role
resolvePathBody path = do
  roles@(role :| others) <-
    lookupName path `onNothingM`
      fatal (ErrorRoleNotFound path)
  unless (null others) $
    fatal $ ErrorAmbiguousPath path roles
  pure role
