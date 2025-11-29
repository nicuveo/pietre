module Lang.Pietre.Stages.Simplification (simplifyModule) where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Representations.AST.Validated
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.Location


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
    _forRangeExpr
    (simplify _forBody)

instance Simplifiable WhileInfo where
  simplify WhileInfo {..} = WhileInfo
    (simplify _whileExpr)
    (simplify _whileBody)

instance Simplifiable LetInfo where
  simplify LetInfo {..} = LetInfo
    _letName
    (simplify _letValue)

instance Simplifiable a => Simplifiable [a] where
  simplify = fmap simplify

instance Simplifiable a => Simplifiable (Maybe a) where
  simplify = fmap simplify

instance Simplifiable a => Simplifiable (WithLocation a) where
  simplify = fmap simplify

instance Simplifiable (Typed LValueExpression) where
  simplify = id

instance Simplifiable (Typed ConstExpression) where
  simplify = id

instance Simplifiable (Typed Expression) where
  simplify = rewrite \ref -> case _typedValue ref of
    AdditionExpr lhs (IntExpr 0) -> Just lhs
    AdditionExpr (IntExpr 0) rhs -> Just rhs

    SubtractionExpr lhs (IntExpr 0) -> Just lhs
    SubtractionExpr (IntExpr 0) rhs -> Just (ref & typedValue .~ IntNegationExpr rhs)

    MultiplicationExpr lhs (IntExpr 1) -> Just lhs
    MultiplicationExpr (IntExpr 1) rhs -> Just rhs

    DivisionExpr lhs (IntExpr 1) -> Just lhs

    ExponentiationExpr lhs (IntExpr 1) -> Just lhs

    BoolAndExpr (BoolExpr True ) rhs -> Just rhs
    BoolAndExpr (BoolExpr False) _   -> Just (ref & typedValue .~ BoolLiteralExpr False)
    BoolAndExpr lhs (BoolExpr True ) -> Just lhs

    BoolOrExpr (BoolExpr True ) _    -> Just (ref & typedValue .~ BoolLiteralExpr True)
    BoolOrExpr (BoolExpr False) rhs  -> Just rhs
    BoolOrExpr lhs (BoolExpr False)  -> Just lhs

    BoolNegationExpr (Typed BoolType (BoolNegationExpr expr)) -> Just expr
    IntNegationExpr  (Typed IntType  (IntNegationExpr  expr)) -> Just expr

    EqualityExpr (BoolExpr True) rhs  -> Just rhs
    EqualityExpr lhs (BoolExpr True)  -> Just lhs
    EqualityExpr (BoolExpr False) rhs -> Just (ref & typedValue .~ BoolNegationExpr rhs)
    EqualityExpr lhs (BoolExpr False) -> Just (ref & typedValue .~ BoolNegationExpr lhs)

    DifferenceExpr (BoolExpr True) rhs  -> Just (ref & typedValue .~ BoolNegationExpr rhs)
    DifferenceExpr lhs (BoolExpr True)  -> Just (ref & typedValue .~ BoolNegationExpr lhs)
    DifferenceExpr (BoolExpr False) rhs -> Just rhs
    DifferenceExpr lhs (BoolExpr False) -> Just lhs

    _ -> Nothing
