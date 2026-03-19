module Lang.Pietre.Stages.Linking (link) where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as M
import Data.Sequence                        qualified as Seq

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
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
  compiledFunctions <- runLinker do
    traverse collectAddresses functions
    ensureNested $ traverse (tryNested . replaceAddresses) functions
  compiledMain <- M.lookup main compiledFunctions `onNothing`
    reportError (Diagnostic Nothing Nothing ErrorNoMainSymbol)
  -- TODO: also check that main has the right type
  let mainAddress = _fFunctionEntrance compiledMain
  pure $ Binary mainAddress $ Seq.fromList $ M.elems compiledFunctions

collectAddresses
  :: Monad m
  => InstructionBuffer
  -> Link m ()
collectAddresses instructions = do
  for_ instructions \case
    Entrance name -> do
      address <- use lcCurrent
      lcCurrent += 1
      lcRegistry %= M.insert name address
    _ -> pass

replaceAddresses
  :: MonadDiagnosis m
  => InstructionBuffer
  -> Link m (Function (Seq (Instruction Resolved)))
replaceAddresses instructions = do
  result <- ensureNested $ for instructions $ tryNested . \case
    PushAddr name -> do
      target <- uses lcRegistry (M.lookup name) `onNothingM`
        reportError (Diagnostic Nothing Nothing $ ErrorSymbolNotFound $ fst name)
      pure $ PushInt target
    Entrance _ -> pure $ Entrance ()
    PushInt x  -> pure $ PushInt x
    Pop        -> pure Pop
    Add        -> pure Add
    Subtract   -> pure Subtract
    Multiply   -> pure Multiply
    Divide     -> pure Divide
    Mod        -> pure Mod
    Not        -> pure Not
    Greater    -> pure Greater
    Duplicate  -> pure Duplicate
    Roll       -> pure Roll
    InInt      -> pure InInt
    InChar     -> pure InChar
    OutInt     -> pure OutInt
    OutChar    -> pure OutChar
    Return     -> pure Return
    Terminate  -> pure Terminate
    Branch     -> pure Branch
  let totalCount = Seq.length $ Seq.filter isEntrance result
  functionEntrance <- findFirstEntrance instructions
  pure $ Function result totalCount functionEntrance

findFirstEntrance
  :: MonadDiagnosis m
  => Seq (Instruction Unresolved)
  -> Link m Int
findFirstEntrance instructions = do
  let addr = fromMaybe entranceNotFound do
        find isEntrance instructions >>= \case
          Entrance a -> Just a
          _          -> Nothing
  uses lcRegistry (M.lookup addr) `onNothingM`
    entranceNotFound
  where
    entranceNotFound =
      reportICE "Linking.replaceAddresses" "function entrance not found" []

isEntrance :: Instruction a -> Bool
isEntrance (Entrance _) = True
isEntrance _            = False
