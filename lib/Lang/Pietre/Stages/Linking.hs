module Lang.Pietre.Stages.Linking (link) where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as M
import Data.Sequence                        qualified as Seq

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Representations.Binary   as BI
import Lang.Pietre.Representations.Bytecode as BC
import Lang.Pietre.Representations.Name
import Lang.Pietre.Stages.Linking.Monad


link
  :: MonadDiagnosis m
  => Name
  -> HashMap Name InstructionBuffer
  -> m Binary
link main functions = do
  let compiledFunctions = runLinker $ traverse visit functions
  compiledMain <- M.lookup main compiledFunctions `onNothing`
    reportError (Diagnostic Nothing Nothing ErrorNoMainSymbol)
  -- TODO: also check that main has the right type
  let mainAddress = _fFunctionEntrance compiledMain
  pure $ Binary mainAddress $ Seq.fromList $ M.elems compiledFunctions

visit
  :: InstructionBuffer
  -> Link (Function (Seq (Instruction Resolved)))
visit instructions = do
  lcEntranceCount .= 0
  functionEntrance <- use lcCurrent
  result <- for instructions \case
    PushAddr name -> do
      target <- views liAddresses (M.! name)
      pure $ PushInt target
    Entrance name -> do
      address <- use lcCurrent
      lcEntranceCount += 1
      lcCurrent += 1
      lcRegistry %= ((name, address):)
      pure $ Entrance ()
    PushInt x -> pure $ PushInt x
    Pop       -> pure Pop
    Add       -> pure Add
    Subtract  -> pure Subtract
    Multiply  -> pure Multiply
    Divide    -> pure Divide
    Mod       -> pure Mod
    Not       -> pure Not
    Greater   -> pure Greater
    Duplicate -> pure Duplicate
    Roll      -> pure Roll
    InInt     -> pure InInt
    InChar    -> pure InChar
    OutInt    -> pure OutInt
    OutChar   -> pure OutChar
    Return    -> pure Return
    Terminate -> pure Terminate
    Branch    -> pure Branch
  totalCount <- use lcEntranceCount
  pure $ Function result totalCount functionEntrance
