module Lang.Pietre.Stages.Lowering where

import "this" Prelude

import Lang.Pietre.Internal.ICE
import Lang.Pietre.Representations.Interface
import Lang.Pietre.Representations.IR
import Lang.Pietre.Representations.Symbol


lowerFunction
  :: SymbolCache
  -> Symbol
  -> Maybe Function
lowerFunction = unimplemented

lowerModule
  :: SymbolCache
  -> Interface
  -> IR
lowerModule = unimplemented
