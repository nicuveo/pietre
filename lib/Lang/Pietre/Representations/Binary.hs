{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Binary where

import "this" Prelude

import Control.Lens

import Lang.Pietre.Representations.Bytecode


data Binary = Binary
  { _bMainAddress :: Int
  , _bFunctions   :: Seq (Function (Seq (Instruction Resolved)))
  }
  deriving Show

data Function i = Function
  { _fInstructions     :: i
  , _fEntranceCount    :: Int
  , _fFunctionEntrance :: Int
  }
  deriving Show

makeLenses 'Binary
makeLenses 'Function
