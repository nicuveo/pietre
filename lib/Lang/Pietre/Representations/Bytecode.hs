module Lang.Pietre.Representations.Bytecode where

import "this" Prelude

data Instruction addr
  = Push Int
  | Pop
  | Call addr
