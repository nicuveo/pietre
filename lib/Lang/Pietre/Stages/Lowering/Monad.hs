{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Lowering.Monad
  ( -- * monad
    Lowering
  , runLowering
    -- * state
  , liInterface
  , lsRegisters
  , lsPlaceholders
  , lsBlocks
  , lsLocation
  , startLabel
  , mkLabel
  , mkRegister
  , isSealed
  , seal
  , isReachable
  , parents
  , currentParents
  , currentScope
  , currentResumeLabel
  , currentContinueLabel
  , currentBreakLabel
  , withInnerScope
  , withLoop
  , appendInstruction
  , appendArgument
    -- * block
  , startBlock
  , endBlock
  , isWithinBlock
  , currentBlock
  , blockInfo
    -- * error reporting
  , warn
  , fatal
  ) where

import "this" Prelude

import Control.Lens                           hiding (index)
import Data.HashMap.Strict                    qualified as M
import Data.HashSet                           qualified as S
import Data.List                              qualified as L

import Lang.Pietre.Internal.Diagnosis
import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Identifier
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR
import Lang.Pietre.Representations.Location
import Lang.Pietre.Representations.Name


type Lowering = DiagnosisT (ReaderT LoweringInfo (State LoweringState))

data LoweringInfo = LoweringInfo
  { _liInterface :: Interface
  , _liDeclName  :: BaseName
  }

data LoweringState = LoweringState
  { _lsNextLabel    :: Int
  , _lsNextRegister :: Int
  , _lsRegisters    :: HashMap (Label, Identifier) Register
  , _lsPlaceholders :: HashMap Label (HashMap Identifier Register)
  , _lsSealed       :: HashSet Label
  , _lsBlocks       :: HashMap Label Block
  , _lsScope        :: [BlockScope]
  , _lsCurrent      :: Maybe Label
  , _lsLocation     :: Maybe Location
  }
  deriving Show

data BlockScope = BlockScope
  { bsResume   :: Label
  , bsContinue :: Maybe Label
  , bsBreak    :: Maybe Label
  }
  deriving Show

runLowering
  :: MonadDiagnosis m
  => Interface
  -> BaseName
  -> Lowering a
  -> m a
runLowering interface declName action =
  action
    & runDiagnosisT
    & flip runReaderT (LoweringInfo interface declName)
    & flip evalState initialState
    & subsume
  where
    initialState = LoweringState
      { _lsNextLabel    = 1
      , _lsNextRegister = 0
      , _lsRegisters    = M.empty
      , _lsPlaceholders = M.empty
      , _lsSealed       = S.empty
      , _lsBlocks       = M.empty
      , _lsScope        = []
      , _lsCurrent      = Nothing
      , _lsLocation     = Nothing
      }

makeLenses ''LoweringInfo
makeLenses ''LoweringState


startLabel :: Label
startLabel = Label 0

mkLabel :: Lowering Label
mkLabel = do
  index <- use lsNextLabel
  lsNextLabel += 1
  pure $ Label index

mkRegister :: Type -> Lowering Register
mkRegister regType = do
  index <- use lsNextRegister
  lsNextRegister += 1
  pure $ Register index regType


isSealed :: Label -> Lowering Bool
isSealed label = uses lsSealed (S.member label)

seal :: Label -> Lowering ()
seal label = lsSealed %= S.insert label


isReachable :: Lowering Bool
isReachable = currentBlock >>= \case
  Label 0 -> pure True
  label   -> not . null <$> parents label


currentBlock :: Lowering Label
currentBlock = use lsCurrent `onNothingM` reportICE
  "IR lowering"
  "tried to access non-existent current block"
  []

isWithinBlock :: Lowering Bool
isWithinBlock = uses lsCurrent isJust

currentScope :: Lowering (Maybe BlockScope)
currentScope = uses lsScope listToMaybe

currentResumeLabel :: Lowering (Maybe Label)
currentResumeLabel = do
  scope <- currentScope
  pure $ fmap bsResume scope

currentContinueLabel :: Lowering (Maybe Label)
currentContinueLabel = do
  scope <- currentScope
  pure $ bsContinue =<< scope

currentBreakLabel :: Lowering (Maybe Label)
currentBreakLabel = do
  scope <- currentScope
  pure $ bsBreak =<< scope


withInnerScope :: Label -> Lowering a -> Lowering a
withInnerScope resumeLabel action = do
  scope <- currentScope
  let newScope = BlockScope resumeLabel (bsContinue =<< scope) (bsBreak =<< scope)
  lsScope %= (newScope:)
  result <- action
  lsScope %= L.drop 1
  pure result

withLoop :: Label -> Label -> Lowering a -> Lowering a
withLoop continueLabel breakLabel action = do
  let newScope = BlockScope
        { bsResume   = continueLabel
        , bsContinue = Just continueLabel
        , bsBreak    = Just breakLabel
        }
  lsScope %= (newScope:)
  result <- action
  lsScope %= L.drop 1
  pure result


blockInfo :: Label -> Lens' LoweringState Block
blockInfo label = lsBlocks . at label . anon defaultBlock (const False)
  where
    defaultBlock = Block
      { _blockParents      = []
      , _blockArguments    = []
      , _blockInstructions = []
      , _blockTerminator   = Panic
      }


parents :: Label -> Lowering [Label]
parents = fmap _blockParents . getBlockInfo

currentParents :: Lowering [Label]
currentParents = currentBlock >>= parents

registerParent :: Label -> Label -> Lowering ()
registerParent label parent =
  blockInfo label . blockParents %= (parent:)

getBlockInfo :: Label -> Lowering Block
getBlockInfo label = use (blockInfo label)

{-
currentBlockInfo :: Lowering Block
currentBlockInfo = currentBlock >>= getBlockInfo
-}

appendInstruction :: Instruction -> Lowering (Maybe Register)
appendInstruction inst = do
  label <- currentBlock
  blockInfo label . blockInstructions %= (<> [inst])
  pure $ case inst of
    Add      target _ _   -> Just target
    Subtract target _ _   -> Just target
    Multiply target _ _   -> Just target
    Divide   target _ _   -> Just target
    Modulo   target _ _   -> Just target
    Exponent target _ _   -> Just target
    CmpEQ    target _ _   -> Just target
    CmpNE    target _ _   -> Just target
    CmpLT    target _ _   -> Just target
    CmpLE    target _ _   -> Just target
    CmpGT    target _ _   -> Just target
    CmpGE    target _ _   -> Just target
    NegateI  target _     -> Just target
    NegateB  target _     -> Just target
    Cast     target _     -> Just target
    AssignI  target _     -> Just target
    AssignB  target _     -> Just target
    AssignC  target _     -> Just target
    AssignA  target _     -> Just target
    Combine  target _     -> Just target
    GetField target _ _   -> Just target
    SetField target _ _ _ -> Just target
    InvokeN  target _ _   -> target
    InvokeR  target _ _   -> target

appendArgument :: Label -> Register -> Lowering ()
appendArgument label reg =
  blockInfo label . blockArguments %= (<> [reg])


startBlock :: Label -> Lowering ()
startBlock label =
  lsCurrent %= \case
    Nothing -> Just label
    Just l  -> reportICE
      "IR lowering"
      "tried to start a new block without finishing the previous one"
      [ "prev label: " ++ show l
      , "new  label: " ++ show label
      ]

endBlock :: Terminator -> Lowering ()
endBlock term = do
  label <- currentBlock
  blockInfo label . blockTerminator .= term
  case term of
    Jump   tgt -> do
      registerParent (_tgtLabel tgt) label
    Branch tgt1 tgt2 _ -> do
      registerParent (_tgtLabel tgt1) label
      registerParent (_tgtLabel tgt2) label
    Return _ -> pass
    Panic -> pass
  lsCurrent .= Nothing


currentLocation :: Lowering Location
currentLocation = do
  use lsLocation `onNothingM`
    reportICE
      "lowering error reporting"
      "current location not found"
      []

fatal :: Message -> Lowering a
fatal message = do
  declName <- view liDeclName
  declLocation <- currentLocation
  reportError $ Diagnostic (Just declName) declLocation message

warn :: Message -> Lowering ()
warn message = do
  declName <- view liDeclName
  declLocation <- currentLocation
  reportWarning $ Diagnostic (Just declName) declLocation message
