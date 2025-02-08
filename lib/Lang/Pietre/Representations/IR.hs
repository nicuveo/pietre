module Lang.Pietre.Representations.IR where

import "this" Prelude


data Symbol f
  = Const (f Int)
