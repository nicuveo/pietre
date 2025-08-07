module Lang.Pietre.Representations.Symbol where

import Lang.Pietre.Representations.AST

data Symbol
  = Function (FunctionInfo Resolved)
  | Constant (ConstInfo    Resolved)
