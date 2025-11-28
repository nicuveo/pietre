module Lang.Pietre.Stages.Simplification (simplifyModule) where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Interface


--------------------------------------------------------------------------------
-- Public API

simplifyModule
  :: Interface
  -> Interface
simplifyModule =
  interfaceSymbols . traverse %~ simplify


--------------------------------------------------------------------------------
-- Internal implementation

class Simplifiable a where
  simplify :: a -> a

instance Simplifiable FunctionInfo where
  simplify FunctionInfo {..} = FunctionInfo
    _funType
    (fmap2 simplify _funBody)

instance Simplifiable Statement where
  simplify = \case
    IfStmt         info -> IfStmt         $ simplify info
    ForStmt        info -> ForStmt        $ simplify info
    WhileStmt      info -> WhileStmt      $ simplify info
    LetStmt        info -> LetStmt        $ simplify info
    ReturnStmt     expr -> ReturnStmt     $ simplify expr
    ExpressionStmt expr -> ExpressionStmt $ simplify expr
    ContinueStmt        -> ContinueStmt
    BreakStmt           -> BreakStmt

instance Simplifiable IfInfo where
  simplify IfInfo {..} = IfInfo
    (simplify _ifExpr)
    (simplify _ifBody)
    (simplify _ifElse)

instance Simplifiable ElseInfo where
  simplify = \case
    ElseIf    info  -> ElseIf $ simplify info
    ElseBlock stmts -> ElseBlock $ simplify stmts

instance Simplifiable ForInfo where
  simplify ForInfo {..} = ForInfo
    _forVariableName
    _forVariableType
    (simplify _forRangeExpr)
    (simplify _forBody)

instance Simplifiable WhileInfo where
  simplify WhileInfo {..} = WhileInfo
    (simplify _whileExpr)
    (simplify _whileBody)

instance Simplifiable LetInfo where
  simplify LetInfo {..} = LetInfo
    _letName
    (simplify _letValue)

instance (Functor f, Simplifiable a) => Simplifiable (f a) where
  simplify = fmap simplify

instance Simplifiable RangeExpression where
  simplify = unimplemented

instance Simplifiable LValueExpression where
  simplify = unimplemented

instance Simplifiable Expression where
  simplify = unimplemented

{-
instance Simplifiable TypedExpression where
  simplify = rewrite \ref -> case _exprValue ref of
    AdditionExpr lhs (IntExpression 0) -> Just lhs
    AdditionExpr (IntExpression 0) rhs -> Just rhs

    SubtractionExpr lhs (IntExpression 0) -> Just lhs
    SubtractionExpr (IntExpression 0) rhs -> Just (ref & exprValue .~ IntNegationExpr rhs)

    MultiplicationExpr lhs (IntExpression 1) -> Just lhs
    MultiplicationExpr (IntExpression 1) rhs -> Just rhs

    DivisionExpr lhs (IntExpression 1) -> Just lhs

    ExponentiationExpr lhs (IntExpression 1) -> Just lhs

    BoolAndExpr (BoolExpression True ) rhs -> Just rhs
    BoolAndExpr (BoolExpression False) _   -> Just (ref & exprValue .~ BoolLiteralExpr False)
    BoolAndExpr lhs (BoolExpression True ) -> Just lhs

    BoolOrExpr (BoolExpression True ) _    -> Just (ref & exprValue .~ BoolLiteralExpr True)
    BoolOrExpr (BoolExpression False) rhs  -> Just rhs
    BoolOrExpr lhs (BoolExpression False)  -> Just lhs

    BoolNegationExpr (TypedExpression _ _ BoolType (BoolNegationExpr expr)) -> Just expr
    IntNegationExpr  (TypedExpression _ _ IntType  (IntNegationExpr  expr)) -> Just expr

    EqualityExpr (BoolExpression True) rhs  -> Just rhs
    EqualityExpr lhs (BoolExpression True)  -> Just lhs
    EqualityExpr (BoolExpression False) rhs -> Just (ref & exprValue .~ BoolNegationExpr rhs)
    EqualityExpr lhs (BoolExpression False) -> Just (ref & exprValue .~ BoolNegationExpr lhs)

    DifferenceExpr (BoolExpression True) rhs  -> Just (ref & exprValue .~ BoolNegationExpr rhs)
    DifferenceExpr lhs (BoolExpression True)  -> Just (ref & exprValue .~ BoolNegationExpr lhs)
    DifferenceExpr (BoolExpression False) rhs -> Just rhs
    DifferenceExpr lhs (BoolExpression False) -> Just lhs

    _ -> Nothing
-}
