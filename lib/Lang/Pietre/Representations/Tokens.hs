module Lang.Pietre.Representations.Tokens where

-- import "this" Prelude

data Token
  = TKeyword Keyword
  | TEOF

data Keyword
  = KConst
