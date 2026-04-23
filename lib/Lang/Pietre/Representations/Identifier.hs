module Lang.Pietre.Representations.Identifier where

import "this" Prelude


newtype Identifier = Identifier { rawIdentifier :: Text }
  deriving (Eq, Ord, Hashable, IsString, Semigroup, Monoid, Lift)
  deriving Show via Text
