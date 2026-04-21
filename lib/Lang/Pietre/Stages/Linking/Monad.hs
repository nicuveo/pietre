{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Stages.Linking.Monad where

import "this" Prelude

import Control.Lens
import Data.HashMap.Strict                  qualified as Map
import Data.Sequence                        qualified as Seq
import Data.Set.Ordered                     qualified as Set

import Lang.Pietre.Representations.Bytecode
import Lang.Pietre.Representations.Name


type Link m = StateT LinkerContext m

data LinkerContext = LinkerContext
  { _lcCurrent   :: Int
  , _lcRegistry  :: HashMap Address Int
  , _lcFuncQueue :: Seq (Name, InstructionBuffer)
  , _lcFuncSeen  :: Set.OSet Name
  }

makeLenses 'LinkerContext

-- | Addresses 1 and 2 are reserved for the special `start` function,
firstAddressableEntrance :: Int
firstAddressableEntrance = 3

initialState :: LinkerContext
initialState = LinkerContext firstAddressableEntrance Map.empty Seq.empty Set.empty

runLinker :: Monad m => Link m a -> m a
runLinker action = evalStateT action initialState
