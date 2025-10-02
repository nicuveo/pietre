module Lang.Pietre.Stages.Lowering where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra
import Data.HashMap.Strict                    qualified as M

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST        as AST
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR         as IR
import Lang.Pietre.Stages.Lowering.Collection
import Lang.Pietre.Stages.Lowering.Monad

lowerModule
  :: Interface
  -> IR
lowerModule moduleInteface@Interface {..} =
  fmap (lowerFunction moduleInteface) _interfaceSymbols

lowerFunction
  :: Interface
  -> AST.FunctionInfo Resolved
  -> IR.Function
lowerFunction interface FunctionInfo {..} =
  runLowering interface do
    sealBlock startLabel
    processBlock startLabel _funBody
    allBlocks <- use lsBlocks
    -- TODO: filter out unreachable blocks
    pure $ Function startLabel $ collectBlocks allBlocks startLabel

sealBlock
  :: Label
  -> LoweringM ()
sealBlock label = do
  whenJustM (uses lsPotential $ M.lookup label) \_arguments -> do
    {-
      for each potential argument:
        register as block argument
        for each parent:
          update terminator with argument
          register argument value
        if all argument values are the same:
          revert parent terminator changes
          transform instructions to replace all instances of the argument with the unique value
    -}
    pass
  seal label

processBlock
  :: Label
  -> AST.Block Resolved
  -> LoweringM ()
processBlock label block = do
  startBlock label
  go block
  where
    go = \case
      [] -> do
        currentResumeLabel >>= \case
          Just nextLabel ->
            endBlock $ Jump $ mkTarget nextLabel
          Nothing -> do
            -- TODO: emit error if function doesn't return unit type
            endBlock $ Return Nothing
      (stmt:stmts) -> do
        reachable <- isReachable
        if not reachable
        then do
          endBlock Panic
          when (not $ null stmts) $
            -- emit warning: unreachable code
            unimplemented
        else do
          processStatement stmt
          inABlock <- isWithinBlock
          if inABlock
          then do
            go stmts
          else do
            when (not $ null stmts) $
              -- emit warning: unreachable code
              unimplemented

processStatement
  :: AST.Statement Resolved
  -> LoweringM ()
processStatement = \case
  ContinueStmt -> do
    nextLabel <- currentContinueLabel `onNothingM`
      -- TODO: report error instead, remove analysis phase diagnostic
      reportICE "IR lowering" "continue statement not in a loop" []
    endBlock $ Jump $ mkTarget nextLabel
  BreakStmt -> do
    nextLabel <- currentBreakLabel `onNothingM`
      -- TODO: report error instead, remove analysis phase diagnostic
      reportICE "IR lowering" "continue statement not in a loop" []
    endBlock $ Jump $ mkTarget nextLabel
  ReturnStmt typedExpression -> do
    -- when (not $ null stmts) $ emit warning: unreachable code
    register <- traverse forceExpression typedExpression
    endBlock $ Return register
  ExpressionStmt expr -> do
    void $ processExpression expr
  LetStmt LetInfo {..} -> do
    register <- forceExpression _letExpr
    current <- currentBlock
    lsRegisters %= M.insert (current, _letName) register
  IfStmt ifInfo -> do
    resumeLabel <- mkLabel
    withInnerScope resumeLabel $
      processIf resumeLabel ifInfo
    seal resumeLabel
    startBlock resumeLabel
  WhileStmt WhileInfo {..} -> do
    conditionLabel <- mkLabel
    loopBlockLabel <- mkLabel
    resumeLabel    <- mkLabel

    -- finish pre-loop block
    endBlock $ Jump $ mkTarget conditionLabel

    -- condition block
    startBlock conditionLabel
    register <- forceExpression _whileExpr
    endBlock $ Branch
      (mkTarget loopBlockLabel)
      (mkTarget resumeLabel)
      register

    -- loop body block
    seal loopBlockLabel
    withLoop conditionLabel resumeLabel $
      processBlock loopBlockLabel _whileBody

    -- start new outer block
    seal conditionLabel
    seal resumeLabel
    startBlock resumeLabel
  ForStmt _ ->
    unimplemented

processIf
  :: Label
  -> AST.IfInfo Resolved
  -> LoweringM ()
processIf resumeLabel IfInfo {..} = do
  register <- forceExpression _ifExpr
  ifBlockLabel <- mkLabel
  elseBlockLabel <- maybe (pure resumeLabel) (const mkLabel) _ifElse
  endBlock $ Branch
    (mkTarget ifBlockLabel)
    (mkTarget elseBlockLabel)
    register

  seal ifBlockLabel
  processBlock ifBlockLabel _ifBody

  for_ _ifElse \case
    ElseBlock elseBody -> do
      seal elseBlockLabel
      processBlock elseBlockLabel elseBody
    ElseIf ifInfo -> do
      startBlock elseBlockLabel
      processIf resumeLabel ifInfo


forceExpression
  :: TypedExpression
  -> LoweringM Register
forceExpression expr =
  processExpression expr `onNothingM` reportICE
    "IR lowering"
    "process expression did not return a register"
    ["expr: " ++ show expr]

-- ASSUMPTION: always end within a block
processExpression
  :: TypedExpression
  -> LoweringM (Maybe Register)
processExpression TypedExpression {..} = case _exprValue of
  BoolLiteralExpr b -> do
    target <- mkRegister BoolType
    appendInstruction $ AssignB target b
  IntLiteralExpr i -> do
    target <- mkRegister IntType
    appendInstruction $ AssignI target i
  CharLiteralExpr c -> do
    target <- mkRegister CharType
    appendInstruction $ AssignC target c
  StringLiteralExpr _ ->
    unimplemented
  ReferenceExpr _ ->
    unimplemented
  IntNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister IntType
    appendInstruction $ NegateI target previous
  BoolNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister BoolType
    appendInstruction $ NegateB target previous
  AdditionExpr lhs rhs -> do
    -- TODO: handle string concatenation
    basicBinaryOperation Add lhs rhs
  SubtractionExpr lhs rhs ->
    basicBinaryOperation Subtract lhs rhs
  MultiplicationExpr lhs rhs ->
    basicBinaryOperation Multiply lhs rhs
  DivisionExpr lhs rhs ->
    basicBinaryOperation Divide lhs rhs
  ModuloExpr lhs rhs ->
    basicBinaryOperation Modulo lhs rhs
  ExponentiationExpr lhs rhs ->
    basicBinaryOperation Exponent lhs rhs
  EqualityExpr lhs rhs ->
    basicBinaryOperation CmpEQ lhs rhs
  DifferenceExpr lhs rhs ->
    basicBinaryOperation CmpNE lhs rhs
  GreaterExpr lhs rhs ->
    basicBinaryOperation CmpGT lhs rhs
  LesserExpr lhs rhs ->
    basicBinaryOperation CmpLT lhs rhs
  GreaterEqExpr lhs rhs ->
    basicBinaryOperation CmpGE lhs rhs
  LesserEqExpr lhs rhs ->
    basicBinaryOperation CmpLE lhs rhs
  BoolAndExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (mkTarget rhsLabel) (Target resumeLabel [lhsRegister]) lhsRegister

    seal rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    seal resumeLabel
    startBlock resumeLabel
    target <- mkRegister BoolType
    appendArgument target
    pure $ Just target
  BoolOrExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (Target resumeLabel [lhsRegister]) (mkTarget rhsLabel) lhsRegister

    seal rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    seal resumeLabel
    startBlock resumeLabel
    target <- mkRegister BoolType
    appendArgument target
    pure $ Just target
  CastExpr e t -> do
    previous <- forceExpression e
    target <- mkRegister =<< translateType t
    -- TODO: do we want to insert cast bound checks here?
    appendInstruction $ Cast target previous
  RangeInclusiveExpr _lhs _rhs ->
    unimplemented
  RangeExclusiveExpr _lhs _rhs ->
    unimplemented
  AssignmentExpr lhs rhs -> do
    value <- forceExpression rhs
    basicAssign lhs value
  AdditionAssignmentExpr lhs rhs ->
    compoundAssign Add lhs rhs
  SubtractionAssignmentExpr lhs rhs ->
    compoundAssign Subtract lhs rhs
  MultiplicationAssignmentExpr lhs rhs ->
    compoundAssign Multiply lhs rhs
  DivisionAssignmentExpr lhs rhs ->
    compoundAssign Divide lhs rhs
  ModuloAssignmentExpr lhs rhs ->
    compoundAssign Modulo lhs rhs
  ExponentiationAssignmentExpr lhs rhs ->
    compoundAssign Exponent lhs rhs
  PathExpr _ ->
    unimplemented
  FieldAccessExpr _ _ ->
    unimplemented
  CallExpr _ _ ->
    unimplemented
  ArrayExpr _ ->
    unimplemented
  IndexExpr _ _ ->
    unimplemented
  StructExpr _ _ ->
    unimplemented
  where
    compoundAssign instruction lhs rhs = do
      value <- basicBinaryOperation instruction lhs rhs
      basicAssign lhs value
    basicAssign lhs value = do
      unimplemented lhs value
      pure Nothing
    basicBinaryOperation instruction lhs rhs = do
      r1 <- forceExpression lhs
      r2 <- forceExpression rhs
      when (_registerType r1 /= _registerType r2) $ reportICE
        "IR lowering"
        "basic binary operations has incompatible register types"
        [ "lhs register: " ++ show r1
        , "rhs register: " ++ show r2
        ]
      target <- mkRegister $ _registerType r1
      appendInstruction $ instruction target r1 r2

translateType :: a
translateType = unimplemented
