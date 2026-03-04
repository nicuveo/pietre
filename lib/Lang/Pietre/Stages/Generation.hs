{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Stages.Generation where

import "this" Prelude

import Data.HashMap.Strict                  qualified as M
import Data.HashSet                         qualified as S
import Data.List                            qualified as L
import Data.Sequence                        ((|>))
import Data.Sequence                        qualified as Seq
import Data.Tuple

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.IR       as IR
import Lang.Pietre.Representations.Name


generateBytecode
  :: Name
  -> IR.Function
  -> Seq (BC.Instruction Unresolved)
generateBytecode functionName IR.Function {..} =
  flip foldMap _funBlocks \(label, block) ->
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
generateBlockCode functionName Block {..} =
  let
    (finalStack, fold -> instructionsBytecode) =
      L.mapAccumL generateInstructionBytecode _blockArguments $
        annotateInstructions _blockTerminator _blockInstructions
    terminatorBytecode =
      generateTerminatorBytecode functionName finalStack _blockTerminator
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
    IR.CmpGT target arg1 arg2 ->
      generateOp target [arg2, arg1] [BC.Greater]
    IR.CmpLT target arg1 arg2 ->
      generateOp target [arg1, arg2] [BC.Greater]
    IR.CmpEQ target arg1 arg2 ->
      generateOp target [arg1, arg2, arg2, arg1] [BC.Greater, BC.PushInt 3, BC.PushInt 1, BC.Roll, BC.Greater, BC.Add, BC.Not]
    IR.CmpNE target arg1 arg2 ->
      generateOp target [arg1, arg2, arg2, arg1] [BC.Greater, BC.PushInt 3, BC.PushInt 1, BC.Roll, BC.Greater, BC.Add]
    IR.CmpGE _target _arg1 _arg2 ->
      unimplemented
    IR.AssignI target intLiteral -> do
      modify (target:)
      pure [BC.PushInt intLiteral]
    _ -> unimplemented
  where
    generateOp target args bc = do
      currentStack <- get
      let desiredStack = args ++ filter (`S.member` outputRegisters) currentStack
          instructions = rearrangeStack currentStack desiredStack
      put desiredStack
      opBytecode <- applyOpN (length args) target bc
      pure $ instructions <> opBytecode

generateTerminatorBytecode
  :: Name
  -> Stack
  -> IR.Terminator
  -> Seq (BC.Instruction Unresolved)
generateTerminatorBytecode functionName currentStack = \case
  IR.Panic ->
    [Terminate]
  IR.Jump Target {..} ->
    rearrangeStack currentStack _tgtArgs <> [PushAddr (functionName, _tgtLabel), BC.Return]
  IR.Return Nothing ->
    Seq.fromList (BC.Pop <$ currentStack) <> [BC.Return]
  IR.Return (Just r) ->
    rearrangeStack currentStack [r] <> [PushInt 2, PushInt 1, Roll, BC.Return]
  IR.Branch _trueTarget _falseTarget _register ->
    unimplemented

applyOpN
  :: Int
  -> Register
  -> Seq (BC.Instruction Unresolved)
  -> BytecodeGen (Seq (BC.Instruction Unresolved))
applyOpN n target bytecode = do
  modify \stack -> target : drop n stack
  pure bytecode


rearrangeStack
  :: Stack
  -> Stack
  -> Seq (BC.Instruction Unresolved)
rearrangeStack startingStack desiredStack =
  let
    (cleaningInstructions, unsortedStack) = clean Seq.empty inputRegisters startingStack
    rollInstructions = snd $ L.mapAccumR rollStackRegister unsortedStack $ zip [0..] desiredStack
  in
    cleaningInstructions <> fold (reverse rollInstructions)
  where
    inputRegisters  = M.fromListWith (+) $ map (,1) startingStack
    outputRegisters = M.fromListWith (+) $ map (,1) desiredStack

    clean
      :: Seq (BC.Instruction Unresolved)
      -> HashMap Register Int
      -> Stack
      -> (Seq (BC.Instruction Unresolved), Stack)
    clean !instructions _ [] = (instructions, [])
    clean !instructions registers stack@(r:_)
      | registers == outputRegisters = (instructions, stack)
      | otherwise =
        let delta = (M.lookupDefault 0 r outputRegisters) - (registers M.! r)
        in if
          | delta == 0 ->
              let (newStack, newInstructions) = rollStack (length stack) 1 stack
              in clean (instructions <> newInstructions) registers newStack
          | delta > 0 ->
              let duplication = Seq.replicate delta BC.Duplicate
                  (newStack, newInstructions) = rollStack (length stack+delta) (delta+1) $ replicate delta r <> stack
              in clean (instructions <> duplication <> newInstructions) (M.adjust (+delta) r registers) newStack
          | otherwise ->
              clean (instructions |> BC.Pop) (M.update subtractOrDelete r registers) (drop 1 stack)

    subtractOrDelete 1 = Nothing
    subtractOrDelete x = Just (x-1)

    rollStackRegister
      :: Stack
      -> (Int, Register)
      -> (Stack, Seq (BC.Instruction Unresolved))
    rollStackRegister stack (i, r)
      | stack !! i == r = (stack, [])
      | rIndex <- findRegister r stack =
          rollStack (i+1) (rIndex+1) stack

    rollStack :: Int -> Int -> Stack -> (Stack, Seq (BC.Instruction Unresolved))
    rollStack depth steps stack =
      let (splitAt steps -> (segment1, segment2), bottom) = splitAt depth stack
      in ( segment2 <> segment1 <> bottom
         , [PushInt depth, PushInt steps, Roll]
         )

    findRegister r stack = fromMaybe
      (reportICE "rearrangeStack" "could not find register" ["register: " ++ show r, "stack: " ++ show stack])
      (L.elemIndex r stack)
{-

source: [r1,r2,r3,r4]
target: [r2,r3,r2]

source: {r1: 1, r2: 1, ... }
target: {r2: 2, r3: 1}

POP        // [r2,r3,r4]
DUPLICATE  // [r2,r2,r3,r4]
ROLL 4 2   // [r3,r4,r2,r2]
ROLL 4 1   // [r4,r2,r2,r3]
POP        // [r2,r2,r3]
ROLL 3 1   // [r2,r3,r2]

-}



registerSize
  :: Register
  -> Int
registerSize = const 1

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
