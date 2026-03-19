{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Linking.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as M

import Lang.Pietre.Representations.Bytecode


type Link m = StateT LinkerContext m

data LinkerContext = LinkerContext
  { _lcCurrent  :: Int
  , _lcRegistry :: HashMap Address Int
  }

makeLenses 'LinkerContext

-- | Addresses 1 and 2 are reserved for the special `start` function,
firstAddressableEntrance :: Int
firstAddressableEntrance = 3

initialState :: LinkerContext
initialState = LinkerContext firstAddressableEntrance M.empty

runLinker :: Monad m => Link m a -> m a
runLinker action = evalStateT action initialState
