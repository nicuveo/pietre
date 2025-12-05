module Lang.Pietre.Representations.Identifier where

import "this" Prelude


newtype Identifier = Identifier { rawIdentifier :: Text }
  deriving (Show, Eq, Ord, Hashable, IsString, Semigroup, Monoid)
