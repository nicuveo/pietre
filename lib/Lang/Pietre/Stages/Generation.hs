module Lang.Pietre.Stages.Generation where

import "this" Prelude

import Data.List                            qualified as L

import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.IR       as IR
import Lang.Pietre.Representations.Name


generateBytecode
  :: Name
  -> IR.Function
  -> Seq (BC.Instruction Unresolved)
generateBytecode functionName IR.Function {..} =
  flip foldMap _funBlocks \(Label label, block) ->
    BC.Entrance (functionName, label) :<|
    generateBlockCode functionName block

generateBlockCode
  :: Name
  -> IR.Block
  -> Seq (BC.Instruction Unresolved)
generateBlockCode functionName Block {..} =
  let
    (stackEnd, instructions) = L.mapAccumL step _blockArguments _blockInstructions
  in
    mconcat instructions <>
    generateTerminator functionName stackEnd _blockTerminator
  where
    step _stackShape = undefined

generateTerminator
  :: Name
  -> [IR.Register]
  -> IR.Terminator
  -> Seq (BC.Instruction Unresolved)
generateTerminator = undefined
