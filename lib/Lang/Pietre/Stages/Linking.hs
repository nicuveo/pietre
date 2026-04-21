module Lang.Pietre.Stages.Linking (link) where

import "this" Prelude

import Control.Lens                         hiding ((:<))
import Control.Monad.Extra
import Control.Monad.Loops
import Data.HashMap.Strict                  qualified as Map
import Data.Sequence                        (ViewL (..), viewl)
import Data.Sequence                        qualified as Seq
import Data.Set.Ordered                     qualified as Set

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
    mainInstructions <-
      Map.lookup main functions `onNothing`
        reportError (Diagnostic Nothing Nothing ErrorNoMainSymbol)
    lcFuncQueue |>= (main, mainInstructions)
    collectAddresses functions
    includedFunctions <- uses lcFuncSeen (Seq.fromList . toList)
    ensureNested $
      for includedFunctions \functionName -> tryNested do
        instructions <- Map.lookup functionName functions `onNothing`
          reportError (Diagnostic Nothing Nothing $ ErrorSymbolNotFound functionName)
        replaceAddresses instructions
  compiledMain <- sequenceHead compiledFunctions `onNothing`
    reportError (Diagnostic Nothing Nothing ErrorNoMainSymbol)
  -- TODO: also check that main has the right type
  let mainAddress = _fFunctionEntrance compiledMain
  pure $ Binary mainAddress compiledFunctions

  where
    sequenceHead = viewl >>> \case
      EmptyL   -> Nothing
      (x :< _) -> Just x

collectAddresses
  :: MonadDiagnosis m
  => HashMap Name InstructionBuffer
  -> Link m ()
collectAddresses functions = do
  success <- whileJust popNextFunction \(functionName, functionInstructions) -> do
    try $ unlessM (uses lcFuncSeen $ Set.member functionName) do
      lcFuncSeen %= (Set.|> functionName)
      for_ functionInstructions \case
        PushAddr (otherFunctionName, _) -> do
          unlessM (uses lcFuncSeen $ Set.member otherFunctionName) do
            otherFunctionInstructions <- Map.lookup otherFunctionName functions `onNothing`
              reportError (Diagnostic Nothing Nothing $ ErrorSymbolNotFound otherFunctionName)
            lcFuncQueue |>= (otherFunctionName, otherFunctionInstructions)
        Entrance name -> do
          address <- use lcCurrent
          lcCurrent += 1
          lcRegistry %= Map.insert name address
        _ -> pass
  void $ ensure $ sequence success

  where
    popNextFunction = do
      uses lcFuncQueue Seq.viewl >>= \case
        EmptyL -> pure Nothing
        functionInfo :< otherFunctions -> do
          lcFuncQueue .= otherFunctions
          pure $ Just functionInfo

replaceAddresses
  :: MonadDiagnosis m
  => InstructionBuffer
  -> Link m (Function (Seq (Instruction Resolved)))
replaceAddresses instructions = do
  result <- ensureNested $ for instructions $ tryNested . \case
    PushAddr name -> do
      target <- uses lcRegistry (Map.lookup name) `onNothingM`
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
  uses lcRegistry (Map.lookup addr) `onNothingM`
    entranceNotFound
  where
    entranceNotFound =
      reportICE "Linking.replaceAddresses" "function entrance not found" []

isEntrance :: Instruction a -> Bool
isEntrance (Entrance _) = True
isEntrance _            = False
