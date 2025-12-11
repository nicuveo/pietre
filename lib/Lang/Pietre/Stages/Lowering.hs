module Lang.Pietre.Stages.Lowering (lowerModule) where

import "this" Prelude

import Control.Lens
import Control.Monad.Extra
import Data.HashMap.Strict                       qualified as M
import GHC.Stack

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.AST.Validated as AST
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR            as IR
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Lowering.Collection
import Lang.Pietre.Stages.Lowering.Monad


lowerModule
  :: MonadDiagnosis m
  => Interface
  -> m IR
lowerModule moduleInterface@Interface {..} =
  M.traverseWithKey (lowerFunction moduleInterface) _interfaceSymbols

lowerFunction
  :: MonadDiagnosis m
  => Interface
  -> Name
  -> AST.FunctionInfo
  -> m IR.Function
lowerFunction interface Name {..} FunctionInfo {..} =
  runLowering interface _nameBase do
    sealBlock startLabel
    startBlock startLabel
    for_ (_funArgs _funType) \(argName, argInfo) -> do
      register <- mkRegister $ forceType $ functionArgType argInfo
      appendArgument startLabel register
      lsRegisters %= M.insert (startLabel, argName) register
    processBlock _funBody
    allBlocks <- use lsBlocks
    -- TODO: filter out unreachable blocks
    pure $ Function startLabel $ collectBlocks allBlocks startLabel

sealBlock
  :: Label
  -> Lowering ()
sealBlock label = do
  placeholderArgs <- uses lsPlaceholders (M.lookup label)
  void
    $ M.traverseWithKey (registerBlockArgument label)
    $ fold placeholderArgs
  seal label

-- ASSUMPTION: process block MUST be called while already in a block
processBlock
  :: AST.Block
  -> Lowering ()
processBlock = \case
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
        processBlock stmts
      else do
        when (not $ null stmts) $
          -- emit warning: unreachable code
          unimplemented

processStatement
  :: WithLocation AST.Statement
  -> Lowering ()
processStatement WithLocation {..} = do
  lsLocation .= Just _location
  case _located of
    ContinueStmt -> do
      nextLabel <- currentContinueLabel `onNothingM`
        fatal ErrorContinueNotInLoop
      endBlock $ Jump $ mkTarget nextLabel
    BreakStmt -> do
      nextLabel <- currentBreakLabel `onNothingM`
        fatal ErrorBreakNotInLoop
      endBlock $ Jump $ mkTarget nextLabel
    ReturnStmt typedExpression -> do
      -- when (not $ null stmts) $ emit warning: unreachable code
      register <- traverse forceExpression typedExpression
      endBlock $ Return register
    ExpressionStmt expr -> do
      void $ processExpression expr
    LetStmt LetInfo {..} -> do
      register <- forceExpression _letValue
      current <- currentBlock
      lsRegisters %= M.insert (current, _letName) register
    IfStmt ifInfo -> do
      resumeLabel <- mkLabel
      withInnerScope resumeLabel $
        processIf resumeLabel ifInfo
      sealBlock resumeLabel
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
      sealBlock loopBlockLabel
      withLoop conditionLabel resumeLabel do
        startBlock loopBlockLabel
        processBlock _whileBody

      -- start new outer block
      sealBlock conditionLabel
      sealBlock resumeLabel
      startBlock resumeLabel
    ForStmt _ ->
      unimplemented

processIf
  :: Label
  -> AST.IfInfo
  -> Lowering ()
processIf resumeLabel IfInfo {..} = do
  register <- forceExpression _ifExpr
  ifBlockLabel <- mkLabel
  elseBlockLabel <- maybe (pure resumeLabel) (const mkLabel) _ifElse
  endBlock $ Branch
    (mkTarget ifBlockLabel)
    (mkTarget elseBlockLabel)
    register

  sealBlock ifBlockLabel
  startBlock ifBlockLabel
  processBlock _ifBody

  for_ _ifElse \case
    ElseBlock elseBody -> do
      sealBlock elseBlockLabel
      startBlock elseBlockLabel
      processBlock elseBody
    ElseIf ifInfo -> do
      startBlock elseBlockLabel
      processIf resumeLabel ifInfo


forceExpression
  :: Typed AST.Expression
  -> Lowering Register
forceExpression expr =
  processExpression expr `onNothingM` reportICE
    "IR lowering"
    "process expression did not return a register"
    ["expr: " ++ show expr]

-- ASSUMPTION: always end within a block
processExpression
  :: Typed AST.Expression
  -> Lowering (Maybe Register)
processExpression Typed {..} = case _typedValue of
  BoolLiteralExpr b -> do
    target <- mkRegister IR.BoolType
    appendInstruction $ AssignB target b
  IntLiteralExpr i -> do
    target <- mkRegister IR.IntType
    appendInstruction $ AssignI target i
  CharLiteralExpr c -> do
    target <- mkRegister IR.CharType
    appendInstruction $ AssignC target c
  StringLiteralExpr _ ->
    unimplemented
  IntNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister IR.IntType
    appendInstruction $ NegateI target previous
  BoolNegationExpr e -> do
    previous <- forceExpression e
    target   <- mkRegister IR.BoolType
    appendInstruction $ NegateB target previous
  AdditionExpr lhs rhs -> do
    -- TODO: handle string concatenation
    basicBinaryOperation Add IR.IntType lhs rhs
  SubtractionExpr lhs rhs ->
    basicBinaryOperation Subtract IR.IntType lhs rhs
  MultiplicationExpr lhs rhs ->
    basicBinaryOperation Multiply IR.IntType lhs rhs
  DivisionExpr lhs rhs ->
    basicBinaryOperation Divide IR.IntType lhs rhs
  ModuloExpr lhs rhs ->
    basicBinaryOperation Modulo IR.IntType lhs rhs
  ExponentiationExpr lhs rhs ->
    basicBinaryOperation Exponent IR.IntType lhs rhs
  EqualityExpr lhs rhs ->
    basicBinaryOperation CmpEQ IR.BoolType lhs rhs
  DifferenceExpr lhs rhs ->
    basicBinaryOperation CmpNE IR.BoolType lhs rhs
  GreaterExpr lhs rhs ->
    basicBinaryOperation CmpGT IR.BoolType lhs rhs
  LesserExpr lhs rhs ->
    basicBinaryOperation CmpLT IR.BoolType lhs rhs
  GreaterEqExpr lhs rhs ->
    basicBinaryOperation CmpGE IR.BoolType lhs rhs
  LesserEqExpr lhs rhs ->
    basicBinaryOperation CmpLE IR.BoolType lhs rhs
  BoolAndExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (mkTarget rhsLabel) (Target resumeLabel [lhsRegister]) lhsRegister

    sealBlock rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    sealBlock resumeLabel
    startBlock resumeLabel
    target <- mkRegister IR.BoolType
    appendArgument resumeLabel target
    pure $ Just target
  BoolOrExpr lhs rhs -> do
    lhsRegister <- forceExpression lhs
    rhsLabel    <- mkLabel
    resumeLabel <- mkLabel
    endBlock $ Branch (Target resumeLabel [lhsRegister]) (mkTarget rhsLabel) lhsRegister

    sealBlock rhsLabel
    startBlock rhsLabel
    rhsRegister <- forceExpression rhs
    endBlock $ Jump $ Target resumeLabel [rhsRegister]

    sealBlock resumeLabel
    startBlock resumeLabel
    target <- mkRegister IR.BoolType
    appendArgument resumeLabel target
    pure $ Just target
  CastExpr e t -> do
    previous <- forceExpression e
    target <- mkRegister $ forceType t
    -- TODO: do we want to insert cast bound checks here?
    appendInstruction $ Cast target previous
  AssignmentExpr lhs rhs -> do
    value <- forceExpression rhs
    assignRegister lhs value
    pure Nothing
  AdditionAssignmentExpr lhs rhs ->
    compoundAssign Add IR.IntType lhs rhs
  SubtractionAssignmentExpr lhs rhs ->
    compoundAssign Subtract IR.IntType lhs rhs
  MultiplicationAssignmentExpr lhs rhs ->
    compoundAssign Multiply IR.IntType lhs rhs
  DivisionAssignmentExpr lhs rhs ->
    compoundAssign Divide IR.IntType lhs rhs
  ModuloAssignmentExpr lhs rhs ->
    compoundAssign Modulo IR.IntType lhs rhs
  ExponentiationAssignmentExpr lhs rhs ->
    compoundAssign Exponent IR.IntType lhs rhs
  FieldAccessExpr structInfo structExpr fieldName -> do
    structRegister <- forceExpression structExpr
    let (fieldIndex, fieldType) = retrieveFieldInfo structInfo fieldName
    target <- mkRegister fieldType
    appendInstruction $ GetField target structRegister fieldIndex
  FunctionCallExpr functionName FunctionTypeInfo {..} callArguments -> do
    arguments <- traverse forceExpression callArguments
    target <- traverse mkRegister $ translateType _funReturn
    appendInstruction $ InvokeN target functionName arguments
  VariableCallExpr varName functionTypeInfo callArguments -> do
    arguments <- traverse forceExpression callArguments
    target <- traverse mkRegister $ translateType $ _funReturn functionTypeInfo
    let functionType = forceType $ AST.FunctionType functionTypeInfo
    funRegister <- resolveName varName functionType
    appendInstruction $ InvokeR target funRegister arguments
  StructExpr _ fields -> do
    registers <- for fields \(_, e) -> forceExpression e
    target <- mkRegister $ forceType _typeInfo
    appendInstruction $ Combine target registers
  LocalVariableExpr varName -> do
    Just <$> resolveName varName (forceType _typeInfo)
  ReferenceArgumentExpr varName -> do
    Just <$> resolveName varName (forceType _typeInfo)
  FunctionNameExpr funName _ -> do
    target <- mkRegister $ forceType _typeInfo
    appendInstruction $ AssignA target funName
  ArrayExpr _ ->
    unimplemented
  IndexExpr _ _ ->
    unimplemented
  RangeExpr _ ->
    unimplemented
  where
    compoundAssign instruction regType lhs rhs = do
      r1 <- processLValueExpression lhs
      r2 <- forceExpression rhs
      target <- mkRegister regType
      value <- appendInstruction (instruction target r1 r2) `onNothingM`
        reportICE
          "IR lowering"
          "assignment rhs did not yield a valid register"
          []
      assignRegister lhs value
      pure Nothing

    basicBinaryOperation instruction regType lhs rhs = do
      r1 <- forceExpression lhs
      r2 <- forceExpression rhs
      target <- mkRegister regType
      appendInstruction $ instruction target r1 r2

processLValueExpression
  :: Typed AST.LValueExpression
  -> Lowering Register
processLValueExpression Typed {..} = case _typedValue of
  LocalVariableLExpr     varName -> do
    resolveName varName $ forceType _typeInfo
  ReferenceArgumentLExpr varName -> do
    resolveName varName $ forceType _typeInfo
  _ -> unimplemented


assignRegister
  :: Typed AST.LValueExpression
  -> Register
  -> Lowering ()
assignRegister lhs value = do
  case _typedValue lhs of
    LocalVariableLExpr varName -> do
      label <- currentBlock
      lsRegisters %= M.insert (label, varName) value
    _ -> unimplemented

forceType
  :: HasCallStack
  => AST.ConcreteType
  -> IR.Type
forceType concreteType = fromMaybe incorrectTypeError $ translateType concreteType
  where
    incorrectTypeError =
      reportICE
        "IR lowering"
        "encountered void or unit when looking for a register type"
        ["type info: " ++ show concreteType]

translateType
  :: AST.ConcreteType
  -> Maybe IR.Type
translateType = \case
  AST.IntType  -> Just IR.IntType
  AST.BoolType -> Just IR.BoolType
  AST.CharType -> Just IR.CharType
  AST.UnitType -> Nothing
  AST.VoidType -> Nothing
  AST.EnumType _ values -> Just $ IR.EnumType $ length values
  AST.StructType StructTypeInfo {..} -> do
    params <- traverse translateType _structTypeParams
    pure $ IR.StructType _structBaseName params
  AST.FunctionType FunctionTypeInfo {..} -> do
    args <- traverse (translateType . AST.functionArgType . snd) _funArgs
    pure $ IR.FunctionType args (translateType _funReturn)

retrieveFieldInfo
  :: AST.StructInfo ConcreteFunctor
  -> Identifier
  -> (Int, IR.Type)
retrieveFieldInfo structInfo@StructInfo {..} fieldName =
  go 0 $ toList _structValues
  where
    go _ [] = fieldNotFoundError
    go !i ((fn, ft):sfs)
      | fn == fieldName = (i, forceType ft)
      | otherwise       = go (i+1) sfs

    fieldNotFoundError =
      reportICE
        "IR lowering"
        "struct field not found in struct definition"
        [ "field name: " ++ show fieldName
        , "struct info: " ++ show structInfo
        ]

resolveName
  :: Identifier
  -> IR.Type
  -> Lowering Register
resolveName name regType = do
  label <- currentBlock
  resolveNameIn label name regType

resolveNameIn
  :: Label
  -> Identifier
  -> IR.Type
  -> Lowering Register
resolveNameIn label name regType =
  uses lsRegisters (M.lookup (label, name)) `onNothingM` do
    ifM (isSealed label)
      searchRecursively
      introduceTempRegister
  where
    searchRecursively = do
      ps <- parents label
      case ps of
        [parent] ->
          resolveNameIn parent name regType
        _ -> do
          blockArgument <- mkRegister regType
          registerBlockArgument label name blockArgument
          pure blockArgument

    introduceTempRegister =
      uses lsPlaceholders (M.lookup label >=> M.lookup name) `onNothingM` do
        tempRegister <- mkRegister regType
        lsPlaceholders %= M.insertWith
          M.union
          label
          (M.singleton name tempRegister)
        pure tempRegister

registerBlockArgument
  :: Label
  -> Identifier
  -> Register
  -> Lowering ()
registerBlockArgument label name blockArgument = do
  ps <- parents label
  appendArgument label blockArgument
  lsRegisters %= M.insert (label, name) blockArgument
  for_ ps \parent -> do
    source <- resolveNameIn parent name (_registerType blockArgument)
    blockInfo parent . blockTerminator %= appendTerminatorArg source
  where
    appendTerminatorArg source = \case
      Jump target -> Jump (target & tgtArgs <>~ [source])
      Branch tgt1 tgt2 r
        | label == _tgtLabel tgt1 -> Branch (tgt1 & tgtArgs <>~ [source]) tgt2 r
        | otherwise               -> Branch tgt1 (tgt2 & tgtArgs <>~ [source]) r
      terminator -> reportICE
        "IR lowering"
        "parent of a block does not point back to it"
        [ "parent terminator: " ++ show terminator
        , "child label: " ++ show label
        ]
