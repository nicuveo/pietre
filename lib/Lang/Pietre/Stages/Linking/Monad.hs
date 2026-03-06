{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Linking.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as M

import Lang.Pietre.Representations.Bytecode


type Link = ReaderT LinkerInfo (State LinkerContext)

data LinkerInfo = LinkerInfo
  { _liAddresses :: ~(HashMap Address Int)
  }

data LinkerContext = LinkerContext
  { _lcCurrent       :: Int
  , _lcRegistry      :: [(Address, Int)]
  , _lcEntranceCount :: Int
  }

makeLenses 'LinkerInfo
makeLenses 'LinkerContext

-- | Addresses 1 and 2 are reserved for the special `start` function,
firstAddressableEntrance :: Int
firstAddressableEntrance = 3

initialState :: LinkerContext
initialState = LinkerContext firstAddressableEntrance [] 0

runLinker :: Link a -> a
runLinker action = result
  where
    (result, finalState) = action
      & flip runReaderT (LinkerInfo $ M.fromList $ _lcRegistry finalState)
      & flip runState initialState
