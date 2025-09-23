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
  resmodDefinitions . traverse . traverse %~ simplify


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
    (simplify _funType)
    (fmap simplify _funBody)

instance Simplifiable (FunctionType Resolved) where
  simplify FunctionType {..} = FunctionType
    _funParams
    (fmap2 simplify _funArgs)
    (fmap  simplify _funReturn)

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
  simplify = rewrite \ref -> case _exprValue ref of
    AdditionExpr lhs (IntExpression 0) -> Just lhs
    AdditionExpr (IntExpression 0) rhs -> Just rhs

    SubtractionExpr lhs (IntExpression 0) -> Just lhs
    SubtractionExpr (IntExpression 0) rhs -> Just (ref & exprValue .~ IntNegationExpr rhs)

    MultiplicationExpr lhs (IntExpression 1) -> Just lhs
    MultiplicationExpr (IntExpression 1) rhs -> Just rhs
    MultiplicationExpr lhs (IntExpression 0) | isPure lhs -> Just $ IntExpression 0
    MultiplicationExpr (IntExpression 0) rhs | isPure rhs -> Just $ IntExpression 0

    DivisionExpr lhs (IntExpression 1) -> Just lhs
    -- TODO: handle division by 0

    ExponentiationExpr lhs (IntExpression 1) -> Just lhs
    ExponentiationExpr lhs (IntExpression 0) | isPure lhs -> Just $ IntExpression 1

    BoolAndExpr (BoolExpression True ) rhs              -> Just rhs
    BoolAndExpr (BoolExpression False) _                -> Just (ref & exprValue .~ BoolLiteralExpr False)
    BoolAndExpr lhs (BoolExpression True )              -> Just lhs
    BoolAndExpr lhs (BoolExpression False) | isPure lhs -> Just $ BoolExpression False

    BoolOrExpr (BoolExpression True ) _                -> Just (ref & exprValue .~ BoolLiteralExpr True)
    BoolOrExpr (BoolExpression False) rhs              -> Just rhs
    BoolOrExpr lhs (BoolExpression True ) | isPure lhs -> Just $ BoolExpression True
    BoolOrExpr lhs (BoolExpression False)              -> Just lhs

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
