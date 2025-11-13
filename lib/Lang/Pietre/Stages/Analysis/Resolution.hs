module Lang.Pietre.Stages.Analysis.Resolving (resolve) where

import "this" Prelude

import Control.Lens                                 hiding (mapping, op)
import Control.Monad.Loops                          (whileJust)
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Maybe                    (hoistMaybe)
import Data.HashMap.Strict.Extra                    qualified as M
import Data.HashSet                                 qualified as S
import Data.Set                                     qualified as Set

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Common
import Lang.Pietre.Representations.AST.Parsed       as Parsed
import Lang.Pietre.Representations.AST.Resolved     as Resolved
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Analysis.Resolution.Monad


--------------------------------------------------------------------------------
-- API

resolve
  :: Monad m
  => ModuleName
  -> BaseName
  -> Scope
  -> WithLocation Parsed.Definition
  -> m (WithLocation Resolved.Definition)
resolve moduleName declarationName topLevelScope def = do
  runResolveT moduleName declarationName topLevelScope (_location def) $
    traverse resolveDefinition def


--------------------------------------------------------------------------------
-- Implementation

resolveDefinition
  :: Monad m
  => Parsed.Definition
  -> ResolveT m Resolved.Definition
resolveDefinition = \case
  TypeAliasDef info -> resolveTypeAlias info
  EnumDef      info -> pure $ EnumDef info
  StructDef    info -> resolveStruct info
  ConstDef     info -> resolveConst info
  FunctionDef  info -> resolveFunction info

resolveTypeAlias
  :: Monad m
  => TypeAliasInfo Parsed
  -> ResolveT m (TypeAliasInfo Resolved)
resolveTypeAlias TypeAliasInfo {..} = do
  expandScopeWithTypeParameters _aliasParams
  resolvedValue <- resolvePath _aliasValue
  pure $ TypeAliasInfo _aliasName _aliasParams resolvedValue

resolveStruct
  :: Monad m
  => StructInfo Parsed
  -> ResolveT m (StructInfo Resolved)
resolveStruct StructInfo {..} = do
  expandScopeWithTypeParameters _structParams
  resolvedValues <- ensure =<< getCompose (traverse2 (Compose . try . resolvePath) _structValues)
  pure $ StructInfo _structName _structParams resolvedValues

resolveConst
  :: Monad m
  => ConstInfo Parsed
  -> ResolveT m (ConstInfo Resolved)
resolveConst ConstInfo {..} = do
  resolvedType <- try $ resolvePath _constType
  resolvedExpr <- try $ resolveExpression _constExpr
  ensure $ liftA2 (ConstInfo _constName) resolvedType resolvedExpr

resolveFunction
  :: Monad m
  => FunctionInfo Parsed
  -> ResolveT m (FunctionInfo Resolved)
resolveFunction ConstInfo {..} = do
  expandScopeWithTypeParameters _structParams
  resolvedType <- resolveFunctionType _funType
  resolvedBody <-
    resolveBlock
      (expandScopeWithFunctionArguments $ _funArgs resolvedType)
      _funBody
  pure $ FunctionInfo _funName resolvedType resolvedBody

resolveFunctionType
  :: Monad m
  => FunctionType Parsed
  -> ResolveT m (FunctionType Resolved)
resolveFunctionType FunctionType {..} = do
  resolvedArgs   <- getCompose $ traverse2 (Compose . try . resolveFunctionArg) _funArgs
  resolvedReturn <- getCompose $ traverse  (Compose . try . resolvePath)        _funReturn
  ensure $ liftA2 (FunctionType _funParams) resolvedArgs resolvedReturn

resolveFunctionArg
  :: Monad m
  => FunctionType Parsed
  -> ResolveT m (FunctionType Resolved)
resolveFunctionArg = \case
  ByValue     path -> ByValue     <$> resolvePath path
  ByReference path -> ByReference <$> resolvePath path

resolveBlock
  :: Monad m
  => ResolveT m ()
  -> Block Parsed
  -> ResolveT m (Block Resolved)
resolveBlock updateScope statements = do
  parentScope <- use rcScope
  updateScope
  resolvedStatements <- for statements \stmt -> do
    rcLocation .= _location stmt
    sequence <$> traverse (try . resolveStatement) stmt
  rcScope .= parentScope
  ensure $ sequence $ resolvedStatements

resolveStatement
  :: Monad m
  => Statement Parsed
  -> ResolveT m (Statement Resolved)
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
  :: Monad m
  => IfInfo Parsed
  -> ResolveT m (IfInfo Resolved)
resolveIf IfInfo {..} = do
  resolvedExpr <- try $ resolveExpression _ifExpr
  resolvedBody <- try $ resolveBlock pass _ifBody
  resolvedElse <- getCompose $ traverse (Compose . try . resolveElse) _ifElse
  ensure $ liftA3 IfInfo resolvedExpr resolvedBody resolveElse

resolveElse
  :: Monad m
  => ElseInfo Parsed
  -> ResolveT m (ElseInfo Resolved)
resolveElse = \case
  ElseIf    info  -> ElseIf    <$> resolveIf info
  ElseBlock block -> ElseBlock <$> resolveBlock pass block

resolveFor
  :: Monad m
  => ForInfo Parsed
  -> ResolveT m (ForInfo Resolved)
resolveFor ForInfo {..} = do
  resolvedExpr <- try $ resolveExpression _forRangeExpr
  resolvedBody <- try $
    resolveBlock
      (expandScopeWithVariable (_forVariableName) Nothing)
      _forBody
  ensure $ liftA2 (ForInfo _forVariableName) resolvedExpr resolveBody

resolveWhile
  :: Monad m
  => WhileInfo Parsed
  -> ResolveT m (WhileInfo Resolved)
resolveWhile WhileInfo {..} = do
  resolvedExpr <- try $ resolveExpression _whileExpr
  resolvedBody <- try $ resolveBlock pass _whileBody
  ensure $ liftA2 WhileInfo resolvedExpr resolveBody

resolveLet
  :: Monad m
  => LetInfo Parsed
  -> ResolveT m (LetInfo Resolved)
resolveLet LetInfo {..} = do
  resolvedType <- getCompose $ traverse (Compose . try . resolvePath) _letType
  resolvedExpr <- try $ resolveExpression _letExpr
  expandScopeWithVariable _letName (join resolvedType)
  ensure $ liftA2 (LetInfo _letName) resolvedType resolvedExpr

resolveExpression
  :: Monad m
  => WithLocation (Expression Parsed)
  -> ResolveT m (Expression Resolved)
resolveExpression expr = do
  rcLocation .= _location expr
  case _located expr of
    PathExpr path ->
      PathExpr <$> resolvePath
    FieldAccessExpr lhs rhs ->
      liftA2 FieldAccessExpr (resolveExpression lhs) (pure rhs)
    CallExpr lhs args -> do
      liftA2 CallExpr (resolvePath lhs) (traverse resolveExpression args)
    ArrayExpr exprs ->
      ArrayExpr <$> traverse resolveExpression exprs
    IndexExpr lhs rhs ->
      liftA2 IndexExpr (resolveExpression lhs) (resolveExpression rhs)
    StructExpr path fields ->
      liftA2 StructExpr (resolvePath path) (traverse resolveExpression fields)
    BoolLiteralExpr b ->
      pure $ BoolLiteralExpr b
    IntLiteralExpr i ->
      pure $ IntLiteralExpr i
    CharLiteralExpr c ->
      pure $ CharLiteralExpr c
    StringLiteralExpr s ->
      pure $ StringLiteralExpr s
    ReferenceExpr path ->
      ReferenceExpr <$> resolveExpression path
    IntNegationExpr expr ->
      IntNegationExpr <$> resolveExpression expr
    BoolNegationExpr expr ->
      BoolNegationExpr <$> resolveExpression expr
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
    CastExpr expr castType ->
      liftA2 CastExpr (resolveExpression expr) (resolvePath castType)
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

resolvePath
  :: Monad m
  -> PathInfo Parsed
  -> ResolveT m (PathInfo Resolved)
resolvePath PathInfo {..} = do
  resolvedName   <- try $ resolvePathBody _pathName
  resolvedParams <- getCompose $ traverse (Compose . try . resolvePath) _pathParams
  ensure $ liftA2 PathInfo resolvedName resolvedParams

resolvePathBody
  :: Monad m
  -> Path
  -> ResolveT m Role
resolveName path = do
  roles@(role :| others) <-
    lookupName path `onNothingM`
      fatal (ErrorRoleNotFound path)
  unless (null others) $
    fatal $ ErrorAmbiguousPath path roles
  pure role
