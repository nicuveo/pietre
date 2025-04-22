{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Linking (link) where

import "this" Prelude

import Control.Lens
import Control.Monad.State.Lazy             qualified as S
import Data.HashMap.Strict                  qualified as M

import Lang.Pietre.Representations.Bytecode


type LinkerM = S.State LinkerState

data LinkerState = LinkerState
  { _lsCurrent       :: Int
  , _lsAddresses     :: ~(HashMap Text Int)
  , _lsRegister      :: [(Text, Int)]
  , _lsEntranceCount :: Int
  }

initialState :: HashMap Text Int -> LinkerState
initialState addresses = LinkerState 1 addresses [] 0

makeLenses 'LinkerState

visit
  :: [Instruction Unresolved]
  -> LinkerM (Int, [Instruction Resolved])
visit function = do
  lsEntranceCount .= 0
  result <- for function \case
    PushAddr name -> do
      target <- uses lsAddresses (M.! name)
      pure $ PushInt target
    Entrance name -> do
      address <- use lsCurrent
      lsEntranceCount += 1
      lsCurrent += 1
      lsRegister %= ((name, address):)
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
  totalCount <- use lsEntranceCount
  pure (totalCount, result)


link
  :: HashMap Text [Instruction Unresolved]
  -> Text
  -> [(Int, [Instruction Resolved])]
link functions main = result
  where
    (result, finalState) =
      S.runState go $ initialState $ M.fromList $ _lsRegister finalState
    go = do
      start  <- visit (functions M.! main)
      others <- sequence do
        (name, instructions) <- M.toList functions
        guard $ name /= main
        pure $ visit instructions
      pure $ start : others
