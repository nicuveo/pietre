{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Lowering.Collection where

import "this" Prelude

import Control.Lens                   hiding (Empty, children)
import Control.Monad.Extra
import Control.Monad.Loops
import Data.HashMap.Strict            qualified as M
import Data.HashSet                   qualified as S
import Data.Sequence                  qualified as L

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.IR


type CollectM = ReaderT (HashMap Label Block) (State CollectState)

data CollectState = CollectState
  { _csQueue   :: Seq Label
  , _csVisited :: HashSet Label
  }

makeLenses ''CollectState


runCollect
  :: HashMap Label Block
  -> Label
  -> CollectM a
  -> a
runCollect blocks start action = action
  & flip runReaderT blocks
  & flip evalState (CollectState (pure start) S.empty)

retrieveBlock
  :: Label
  -> CollectM Block
retrieveBlock label = do
  blocks <- ask
  M.lookup label blocks `onNothing` reportICE
    "IR lowering"
    "block info not found"
    [ "block info: " ++ show blocks
    , "missing block: " ++ show label
    ]

visit :: Label -> CollectM (Label, Block)
visit label = do
  csVisited %= S.insert label
  process =<< retrieveBlock label
  where
    process block =
      case _blockTerminator block of
        Branch (_tgtLabel -> child1) (_tgtLabel -> child2) _ ->
          finalize block [child1, child2]
        Return _ ->
          finalize block []
        Panic ->
          finalize block []
        Jump (_tgtLabel -> child) -> do
          childBlock <- retrieveBlock child
          if _blockParents childBlock == [label]
          then process $ merge block childBlock
          else finalize block [child]

    finalize block children = do
      csQueue %= (<> L.fromList children)
      pure (label, block)

    merge block1 block2 = Block
      { _blockParents      = _blockParents      block1
      , _blockArguments    = _blockArguments    block1
      , _blockInstructions = _blockInstructions block1 <> _blockInstructions block2
      , _blockTerminator   = _blockTerminator   block2
      }

step :: CollectM (Maybe (Label, Block))
step =
  use csQueue >>= \case
    Empty            -> pure Nothing
    label :<| labels -> do
      csQueue .= labels
      ifM (uses csVisited $ S.member label)
        step
        (Just <$> visit label)

collectBlocks
  :: HashMap Label Block
  -> Label
  -> NonEmpty (Label, Block)
collectBlocks blocks start =
  toNonEmpty $ runCollect blocks start $ unfoldM step
  where
    toNonEmpty = \case
      (x:xs) -> x :| xs
      _ -> reportICE
        "IR lowering"
        "no blocks to collect"
        ["block info: " ++ show blocks]
