{-# LANGUAGE OverloadedLists #-}

module Lang.Pietre.Stages.Generation where

import "this" Prelude

import Control.Lens
import Data.HashSet                         qualified as S
import Data.List                            qualified as L
import Data.Sequence                        qualified as Seq

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.IR       as IR
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Generation.Monad


generateBytecode
  :: Name
  -> IR.Function
  -> InstructionBuffer
generateBytecode functionName IR.Function {..} =
  runGeneration functionName do
    blocksCode <- traverse (uncurry generateBlockBytecode) _funBlocks
    pure $ fold blocksCode

generateBlockBytecode
  :: IR.Label
  -> IR.Block
  -> Generate InstructionBuffer
generateBlockBytecode label Block {..} = do
  functionName <- view giFunctionName
  generateWith _blockArguments do
    appendInstructions [Entrance (functionName, label)]
    traverse_ (uncurry generateInstructionBytecode) $
      annotateInstructions _blockTerminator _blockInstructions
    generateTerminatorBytecode _blockTerminator

generateInstructionBytecode
  :: HashSet Register
  -> IR.Instruction
  -> Generate ()
generateInstructionBytecode outputRegisters = \case
  IR.Add target arg1 arg2 ->
    go target [arg1, arg2] [BC.Add]
  IR.Subtract target arg1 arg2 -> do
    go target [arg1, arg2] [BC.Subtract]
  IR.Multiply target arg1 arg2 ->
    go target [arg1, arg2] [BC.Multiply]
  IR.Divide target arg1 arg2 ->
    go target [arg1, arg2] [BC.Divide]
  IR.Modulo target arg1 arg2 ->
    go target [arg1, arg2] [BC.Mod]
  IR.NegateI target arg ->
    go target [arg] [BC.PushInt 0, BC.PushInt 2, BC.PushInt 1, BC.Roll, BC.Subtract]
  IR.NegateB target arg ->
    go target [arg] [BC.Not]
  IR.CmpGT target arg1 arg2 ->
    go target [arg2, arg1] [BC.Greater]
  IR.CmpLT target arg1 arg2 ->
    go target [arg1, arg2] [BC.Greater]
  IR.CmpGE target arg1 arg2 ->
    go target [arg1, arg2] [BC.Greater, BC.Not]
  IR.CmpLE target arg1 arg2 ->
    go target [arg2, arg1] [BC.Greater, BC.Not]
  IR.CmpEQ target arg1 arg2 ->
    go target [arg1, arg2, arg2, arg1] [BC.Greater, BC.PushInt 3, BC.PushInt 1, BC.Roll, BC.Greater, BC.Add, BC.Not]
  IR.CmpNE target arg1 arg2 ->
    go target [arg1, arg2, arg2, arg1] [BC.Greater, BC.PushInt 3, BC.PushInt 1, BC.Roll, BC.Greater, BC.Add]
  IR.Exponent _target _arg1 _arg2 ->
    unimplemented
  IR.AssignI _target _intLiteral -> do
    -- modify (target:)
    -- pure [BC.PushInt intLiteral]
    pass
  _ ->
    unimplemented
  where
    go = appendOperation outputRegisters

generateTerminatorBytecode
  :: IR.Terminator
  -> Generate ()
generateTerminatorBytecode = \case
  IR.Panic ->
    appendInstructions [Terminate]
  IR.Jump Target {..} -> do
    functionName <- view giFunctionName
    rearrangeStack _tgtArgs
    appendInstructions [PushAddr (functionName, _tgtLabel), BC.Return]
  IR.Return Nothing -> do
    rearrangeStack []
    appendInstructions [BC.Return]
  IR.Return (Just r) -> do
    rearrangeStack [r]
    -- TODO: handle bigger registers
    appendRoll 2 1
    appendInstructions [BC.Return]
  IR.Branch _trueTarget _falseTarget _register ->
    unimplemented




registerSize
  :: Register
  -> Int
registerSize = const 1

annotateInstructions
  :: IR.Terminator
  -> [IR.Instruction]
  -> Seq (HashSet Register, IR.Instruction)
annotateInstructions terminator =
  snd . L.mapAccumR computeOutput (terminatorRegisters terminator) . Seq.fromList
  where
    computeOutput desiredOutput instruction =
      (desiredInput instruction desiredOutput, (desiredOutput, instruction))

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
