module Lang.Pietre.Stages.Simplification (simplifyModule) where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Batteries.BuiltIn
import Lang.Pietre.Representations.AST
import Lang.Pietre.Stages.Analysis


--------------------------------------------------------------------------------
-- Public API

simplifyModule
  :: ResolvedModule
  -> ResolvedModule
simplifyModule =
  resmodDefinitionCache . traverse . traverse %~ simplify


--------------------------------------------------------------------------------
-- Internal implementation

class Simplifiable a where
  simplify :: a -> a

instance Simplifiable (Definition Resolved) where
  simplify = \case
    TypeAliasDef info -> TypeAliasDef $ simplify info
    StructDef    info -> StructDef    $ simplify info
    ConstDef     info -> ConstDef     $ simplify info
    FunctionDef  info -> FunctionDef  $ simplify info
    EnumDef      info -> EnumDef info

instance Simplifiable (TypeAliasInfo Resolved) where
  simplify = aliasValue %~ simplify

instance Simplifiable (StructInfo Resolved) where
  simplify = structValues . traverse . traverse %~ simplify

instance Simplifiable (ConstInfo Resolved) where
  simplify ConstInfo {..} = ConstInfo
    _constName
    (simplify _constType)
    (simplify _constExpr)

instance Simplifiable (FunctionInfo Resolved) where
  simplify FunctionInfo {..} = FunctionInfo
    _funName
    _funParams
    (fmap2 simplify _funArgs)
    (fmap  simplify _funReturn)
    (fmap  simplify _funBody)

instance Simplifiable (FunctionType Resolved) where
  simplify FunctionType {..} = FunctionType
    _funtypeParams
    (fmap2 simplify _funtypeArgs)
    (fmap  simplify _funtypeReturn)

instance Simplifiable (FunctionArgType Resolved) where
  simplify = \case
    ByValue     p -> ByValue     $ simplify p
    ByReference p -> ByReference $ simplify p

instance Simplifiable (Statement Resolved) where
  simplify = \case
    IfStmt         info -> IfStmt         $ simplify info
    ForStmt        info -> ForStmt        $ simplify info
    WhileStmt      info -> WhileStmt      $ simplify info
    LetStmt        info -> LetStmt        $ simplify info
    ReturnStmt     expr -> ReturnStmt     $ fmap simplify expr
    ExpressionStmt expr -> ExpressionStmt $ simplify expr
    ContinueStmt        -> ContinueStmt
    BreakStmt           -> BreakStmt

instance Simplifiable (IfInfo Resolved) where
  simplify IfInfo {..} = IfInfo
    (simplify _ifExpr)
    (fmap simplify _ifBody)
    (fmap simplify _ifElse)

instance Simplifiable (ElseInfo Resolved) where
  simplify = \case
    ElseIf    info  -> ElseIf $ simplify info
    ElseBlock stmts -> ElseBlock $ fmap simplify stmts

instance Simplifiable (ForInfo Resolved) where
  simplify ForInfo {..} = ForInfo
    _forVariableName
    (simplify _forRangeExpr)
    (fmap simplify _forBody)

instance Simplifiable (WhileInfo Resolved) where
  simplify WhileInfo {..} = WhileInfo
    (simplify _whileExpr)
    (fmap simplify _whileBody)

instance Simplifiable (LetInfo Resolved) where
  simplify LetInfo {..} = LetInfo
    _letName
    (fmap simplify _letType)
    (simplify _letExpr)

instance Simplifiable (PathInfo Resolved) where
  simplify = pathParams . traverse %~ simplify

instance Simplifiable TypedExpression where
  simplify TypedExpression {..} = TypedExpression
    (simplify _exprType)
    (simplify _exprValue)

instance Simplifiable (Expression Resolved) where
  simplify = rewrite \case
    AdditionExpr lhs (IntExpression 0) -> Just $ _exprValue lhs
    AdditionExpr (IntExpression 0) rhs -> Just $ _exprValue rhs

    SubtractionExpr lhs (IntExpression 0) -> Just $ _exprValue lhs
    SubtractionExpr (IntExpression 0) rhs -> Just $ IntNegationExpr rhs

    MultiplicationExpr lhs (IntExpression 1) -> Just $ _exprValue lhs
    MultiplicationExpr (IntExpression 1) rhs -> Just $ _exprValue rhs

    DivisionExpr lhs (IntExpression 1) -> Just $ _exprValue lhs

    ExponentiationExpr lhs (IntExpression 1) -> Just $ _exprValue lhs

    BoolAndExpr (BoolExpression True ) rhs -> Just $ _exprValue rhs
    BoolAndExpr (BoolExpression False) _   -> Just $ BoolLiteralExpr False

    BoolOrExpr (BoolExpression True ) _   -> Just $ BoolLiteralExpr True
    BoolOrExpr (BoolExpression False) rhs -> Just $ _exprValue rhs

    BoolNegationExpr (TypedExpression BoolType (BoolNegationExpr expr)) -> Just $ _exprValue expr
    IntNegationExpr  (TypedExpression IntType  (IntNegationExpr  expr)) -> Just $ _exprValue expr

    EqualityExpr (BoolExpression True) rhs  -> Just $ _exprValue rhs
    EqualityExpr lhs (BoolExpression True)  -> Just $ _exprValue lhs
    EqualityExpr (BoolExpression False) rhs -> Just $ BoolNegationExpr rhs
    EqualityExpr lhs (BoolExpression False) -> Just $ BoolNegationExpr lhs

    DifferenceExpr (BoolExpression True) rhs  -> Just $ BoolNegationExpr rhs
    DifferenceExpr lhs (BoolExpression True)  -> Just $ BoolNegationExpr lhs
    DifferenceExpr (BoolExpression False) rhs -> Just $ _exprValue rhs
    DifferenceExpr lhs (BoolExpression False) -> Just $ _exprValue lhs

    _ -> Nothing
