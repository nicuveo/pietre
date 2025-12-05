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
        resolved <- runResolve declarationBaseName topLevelScope definitionLocation $ resolveDefinition $ _located def
        pure (declarationBaseName, resolved <$ def)
  pure (exported, resolvedDefinitions)


--------------------------------------------------------------------------------
-- Implementation

resolveDefinition
  :: Parsed.Definition
  -> Resolve Resolved.Definition
resolveDefinition = \case
  TypeAliasDef info -> TypeAliasDef <$> resolveTypeAlias info
  StructDef    info -> StructDef    <$> resolveStruct info
  ConstDef     info -> ConstDef     <$> resolveConst info
  FunctionDef  info -> FunctionDef  <$> resolveFunction info
  EnumDef      info -> pure $ EnumDef info

resolveTypeAlias
  :: Parsed.TypeAliasInfo
  -> Resolve Resolved.TypeAliasInfo
resolveTypeAlias TypeAliasInfo {..} = do
  expandScopeWithTypeParameters _aliasParams
  resolvedValue <- resolvePath _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolvedValue

resolveStruct
  :: Parsed.StructInfo
  -> Resolve Resolved.StructInfo
resolveStruct StructInfo {..} = do
  expandScopeWithTypeParameters _structParams
  resolvedValues <- ensureNested $ traverse2 (tryNested . resolvePath) _structValues
  pure $ StructInfo _structName _structParams resolvedValues

resolveConst
  :: Parsed.ConstInfo
  -> Resolve Resolved.ConstInfo
resolveConst ConstInfo {..} = do
  resolvedType <- try $ resolvePath _constType
  resolvedExpr <- try $ resolveExpression _constExpr
  ensure $ liftA2 (ConstInfo _constName) resolvedType resolvedExpr

resolveFunction
  :: Parsed.FunctionInfo
  -> Resolve Resolved.FunctionInfo
resolveFunction FunctionInfo {..} = do
  expandScopeWithTypeParameters $ _funParams _funType
  resolvedType <- resolveFunctionType _funType
  resolvedBody <-
    resolveBlock
      (expandScopeWithFunctionArguments $ _funArgs resolvedType)
      _funBody
  pure $ FunctionInfo _funName resolvedType resolvedBody

resolveFunctionType
  :: Parsed.FunctionType
  -> Resolve Resolved.FunctionType
resolveFunctionType FunctionType {..} = do
  resolvedArgs   <- getCompose $ traverse3 (tryNested . resolvePath) _funArgs
  resolvedReturn <- getCompose $ traverse  (tryNested . resolvePath) _funReturn
  ensure $ liftA2 (FunctionType _funParams) resolvedArgs resolvedReturn

resolveBlock
  :: Resolve ()
  -> Parsed.Block
  -> Resolve Resolved.Block
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
  :: Parsed.Statement
  -> Resolve Resolved.Statement
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
  :: Parsed.IfInfo
  -> Resolve Resolved.IfInfo
resolveIf IfInfo {..} = do
  resolvedExpr <- try $ resolveExpression _ifExpr
  resolvedBody <- try $ resolveBlock pass _ifBody
  resolvedElse <- getCompose $ traverse (Compose . try . resolveElse) _ifElse
  ensure $ liftA3 IfInfo resolvedExpr resolvedBody resolvedElse

resolveElse
  :: Parsed.ElseInfo
  -> Resolve Resolved.ElseInfo
resolveElse = \case
  ElseIf    info  -> ElseIf    <$> resolveIf info
  ElseBlock block -> ElseBlock <$> resolveBlock pass block

resolveFor
  :: Parsed.ForInfo
  -> Resolve Resolved.ForInfo
resolveFor ForInfo {..} = do
  resolvedExpr <- try $ resolveExpression _forRangeExpr
  resolvedBody <- try $
    resolveBlock
      (expandScopeWithVariable _forVariableName)
      _forBody
  ensure $ liftA2 (ForInfo _forVariableName) resolvedExpr resolvedBody

resolveWhile
  :: Parsed.WhileInfo
  -> Resolve Resolved.WhileInfo
resolveWhile WhileInfo {..} = do
  resolvedExpr <- try $ resolveExpression _whileExpr
  resolvedBody <- try $ resolveBlock pass _whileBody
  ensure $ liftA2 WhileInfo resolvedExpr resolvedBody

resolveLet
  :: Parsed.LetInfo
  -> Resolve Resolved.LetInfo
resolveLet LetInfo {..} = do
  resolvedType <- getCompose $ traverse (Compose . try . resolvePath) _letType
  resolvedExpr <- try $ resolveExpression _letExpr
  expandScopeWithVariable _letName
  ensure $ liftA2 (LetInfo _letName) resolvedType resolvedExpr

resolveExpression
  :: WithLocation Parsed.Expression
  -> Resolve (WithLocation Resolved.Expression)
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
      binaryExpr IndexExpr lhs rhs
    StructExpr path fields -> do
      resolvedPath <- try $ resolvePath path
      resolvedFields <- getCompose $ traverse2 (tryNested . resolveExpression) fields
      ensure $ liftA2 StructExpr resolvedPath resolvedFields
    CastExpr e castType -> do
      resolvedExpr <- try $ resolveExpression e
      resolvedType <- try $ resolvePath castType
      ensure $ liftA2 CastExpr resolvedExpr resolvedType
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
      binaryExpr AdditionExpr lhs rhs
    SubtractionExpr lhs rhs ->
      binaryExpr SubtractionExpr lhs rhs
    MultiplicationExpr lhs rhs ->
      binaryExpr MultiplicationExpr lhs rhs
    DivisionExpr lhs rhs ->
      binaryExpr DivisionExpr lhs rhs
    ModuloExpr lhs rhs ->
      binaryExpr ModuloExpr lhs rhs
    ExponentiationExpr lhs rhs ->
      binaryExpr ExponentiationExpr lhs rhs
    EqualityExpr lhs rhs ->
      binaryExpr EqualityExpr lhs rhs
    DifferenceExpr lhs rhs ->
      binaryExpr DifferenceExpr lhs rhs
    GreaterExpr lhs rhs ->
      binaryExpr GreaterExpr lhs rhs
    LesserExpr lhs rhs ->
      binaryExpr LesserExpr lhs rhs
    GreaterEqExpr lhs rhs ->
      binaryExpr GreaterEqExpr lhs rhs
    LesserEqExpr lhs rhs ->
      binaryExpr LesserEqExpr lhs rhs
    BoolAndExpr lhs rhs ->
      binaryExpr BoolAndExpr lhs rhs
    BoolOrExpr lhs rhs ->
      binaryExpr BoolOrExpr lhs rhs
    RangeInclusiveExpr lhs rhs ->
      binaryExpr RangeInclusiveExpr lhs rhs
    RangeExclusiveExpr lhs rhs ->
      binaryExpr RangeExclusiveExpr lhs rhs
    AssignmentExpr lhs rhs ->
      binaryExpr AssignmentExpr lhs rhs
    AdditionAssignmentExpr lhs rhs ->
      binaryExpr AdditionAssignmentExpr lhs rhs
    SubtractionAssignmentExpr lhs rhs ->
      binaryExpr SubtractionAssignmentExpr lhs rhs
    MultiplicationAssignmentExpr lhs rhs ->
      binaryExpr MultiplicationAssignmentExpr lhs rhs
    DivisionAssignmentExpr lhs rhs ->
      binaryExpr DivisionAssignmentExpr lhs rhs
    ModuloAssignmentExpr lhs rhs ->
      binaryExpr ModuloAssignmentExpr lhs rhs
    ExponentiationAssignmentExpr lhs rhs ->
      binaryExpr ExponentiationAssignmentExpr lhs rhs
  pure $ result <$ expr
  where
    binaryExpr con lhs rhs = do
      resolvedLHS <- try $ resolveExpression lhs
      resolvedRHS <- try $ resolveExpression rhs
      ensure $ liftA2 con resolvedLHS resolvedRHS

resolvePath
  :: Parsed.PathInfo
  -> Resolve Resolved.PathInfo
resolvePath PathInfo {..} = do
  resolvedName   <- try $ resolvePathBody _pathBase
  resolvedParams <- getCompose $ traverse (Compose . try . resolvePath) _pathParams
  ensure $ liftA2 PathInfo resolvedName resolvedParams

resolvePathBody
  :: Path
  -> Resolve Role
resolvePathBody path = do
  roles@(role :| others) <-
    lookupName path `onNothingM`
      fatal (ErrorRoleNotFound path)
  unless (null others) $
    fatal $ ErrorAmbiguousPath path roles
  pure role
