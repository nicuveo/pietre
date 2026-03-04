{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Stages.Generation where

import "this" Prelude

import Data.HashSet                         qualified as S
import Data.List                            qualified as L
import Data.Sequence                        qualified as Seq
import Data.Tuple

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.IR       as IR
import Lang.Pietre.Representations.Name

{-

|    let a = 3;
||   let b = a + 5;
|||  let c = a - 1;
 ||
 ||  if c < 0 {
 |   }
 |   let d = 4;
 |
 |   print(b);

-}

generateBytecode
  :: Name
  -> IR.Function
  -> Seq (BC.Instruction Unresolved)
generateBytecode functionName IR.Function {..} =
  flip foldMap _funBlocks \(Label label, block) ->
    BC.Entrance (functionName, label) :<|
    generateBlockCode functionName block


type BytecodeGen = StateT Stack (Reader (HashSet Register))
type Stack = [Register]

runBytecodeGen
  :: HashSet Register
  -> Stack
  -> BytecodeGen a
  -> (Stack, a)
runBytecodeGen outputRegisters stack action = action
  & flip runStateT stack
  & flip runReader outputRegisters
  & swap

generateBlockCode
  :: Name
  -> IR.Block
  -> Seq (BC.Instruction Unresolved)
generateBlockCode _functionName Block {..} =
  let
    (finalStack, fold -> instructionsBytecode) =
      L.mapAccumL generateInstructionBytecode _blockArguments $
        annotateInstructions _blockTerminator _blockInstructions
    terminatorBytecode =
      generateTerminatorBytecode finalStack _blockTerminator
  in
    instructionsBytecode <> terminatorBytecode

generateInstructionBytecode
  :: Stack
  -> (IR.Instruction, HashSet Register)
  -> (Stack, Seq (BC.Instruction Unresolved))
generateInstructionBytecode stack (instruction, outputRegisters) =
  runBytecodeGen outputRegisters stack $ case instruction of
    IR.Add target arg1 arg2 ->
      generateOp target [arg1, arg2] [BC.Add]
    IR.Subtract target arg1 arg2 -> do
      generateOp target [arg1, arg2] [BC.Subtract]
    IR.Multiply target arg1 arg2 ->
      generateOp target [arg1, arg2] [BC.Multiply]
    IR.Divide target arg1 arg2 ->
      generateOp target [arg1, arg2] [BC.Divide]
    IR.Modulo target arg1 arg2 ->
      generateOp target [arg1, arg2] [BC.Mod]
    IR.NegateI target arg ->
      generateOp target [arg] [BC.PushInt 0, BC.PushInt 2, BC.PushInt 1, BC.Roll, BC.Subtract]
    IR.NegateB target arg ->
      generateOp target [arg] [BC.Not]
    IR.AssignI target intLiteral -> do
      modify (target:)
      pure [BC.PushInt intLiteral]
    _ -> unimplemented
  where
    generateOp target args bc = do
      argsBytecode <- traverse (uncurry rollArgument) (zip [1..] args)
      opBytecode   <- applyOpN (length args) target bc
      pure $ fold argsBytecode <> opBytecode

generateTerminatorBytecode
  :: Stack
  -> IR.Terminator
  -> Seq (BC.Instruction Unresolved)
generateTerminatorBytecode _stack = const [] -- TODO

applyOpN
  :: Int
  -> Register
  -> Seq (BC.Instruction Unresolved)
  -> BytecodeGen (Seq (BC.Instruction Unresolved))
applyOpN n target bytecode = do
  modify \stack -> target : drop n stack
  pure bytecode


rollArgument
  :: Int
  -> Register
  -> BytecodeGen (Seq (BC.Instruction Unresolved))
rollArgument argIndex argRegister = do
  stack <- get
  let rollInstructions = computeRoll argRegister stack
  put $ argRegister : L.delete argRegister stack
  shouldDuplicate <- asks (S.member argRegister)
  postRollInstructions <- if
    | shouldDuplicate && argIndex == 1 -> do
        modify \s -> take 1 s <> s
        pure [BC.Duplicate]
    | shouldDuplicate -> do
        steps <- gets (sum . map registerSize . take argIndex)
        let depth = registerSize argRegister + steps
        modify \s -> take argIndex s ++ argRegister : drop argIndex s
        pure [BC.Duplicate, BC.PushInt depth, BC.PushInt steps, BC.Roll]
    | otherwise -> do
        pure []
  pure $ rollInstructions <> postRollInstructions

      {-
          stack: [i4,i2,i1]
          instruction: Add i5 i4 i1
          outputRegisters: {i5, i2, i1}

          Push 3
          Push 2
          Roll
          // [i2, i1, i4]
          Push 2
          Push 1
          Roll
          Duplicate
          // [i2, i4, i1, i1]
          Push 3
          Push 1
          Roll
          // [i2, i1, i4, i1]
          Add

        -}



registerSize
  :: Register
  -> Int
registerSize = const 1

computeRoll
  :: Register
  -> [Register]
  -> Seq (BC.Instruction Unresolved)
computeRoll target stack =
  let (depth, steps) = go 0 stack
  in
    if steps == 0
    then []
    else [PushInt depth, PushInt steps, Roll]
  where
    go _ [] =
      reportICE "computeRoll" "could not find target register" ["target: " ++ show target, "stack: " ++ show stack]
    go !depth (r:rs)
      | r == target = (depth + registerSize r, depth)
      | otherwise   = go (depth + registerSize r) rs

annotateInstructions
  :: IR.Terminator
  -> [IR.Instruction]
  -> Seq (IR.Instruction, HashSet Register)
annotateInstructions terminator =
  snd . L.mapAccumR computeOutput (terminatorRegisters terminator) . Seq.fromList
  where
    computeOutput desiredOutput instruction =
      (desiredInput instruction desiredOutput, (instruction, desiredOutput))

    adjustOutput target args =
      maybe id S.delete target .
      S.union (S.fromList args)

    desiredInput = \case
      IR.Add      target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.Subtract target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.Multiply target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.Divide   target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.Modulo   target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.Exponent target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpEQ    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpNE    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpLT    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpLE    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpGT    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.CmpGE    target arg1 arg2 -> adjustOutput (Just target) [arg1, arg2]
      IR.NegateI  target arg       -> adjustOutput (Just target) [arg]
      IR.NegateB  target arg       -> adjustOutput (Just target) [arg]
      IR.Cast     target arg       -> adjustOutput (Just target) [arg]
      IR.AssignI  target _         -> S.delete target
      IR.AssignB  target _         -> S.delete target
      IR.AssignC  target _         -> S.delete target
      IR.AssignA  target _         -> S.delete target
      IR.InvokeN  target _    args -> adjustOutput target args
      IR.InvokeR  target func args -> adjustOutput target (func : args)
      IR.Combine  _target _         -> unimplemented
      IR.GetField _target _ _       -> unimplemented
      IR.SetField _target _ _ _     -> unimplemented



terminatorRegisters
  :: IR.Terminator
  -> HashSet Register
terminatorRegisters = \case
  IR.Jump target ->
    fromTarget target
  IR.Branch branch1 branch2 condition ->
    fromTarget branch1 <> fromTarget branch2 <> S.singleton condition
  IR.Return returnValue ->
    maybe S.empty S.singleton returnValue
  IR.Panic ->
    S.empty
  where
    fromTarget = S.fromList . _tgtArgs


{-
generateTerminator
  :: Name
  -> [IR.Register]
  -> IR.Terminator
  -> Seq (BC.Instruction Unresolved)
generateTerminator = undefined
-}
