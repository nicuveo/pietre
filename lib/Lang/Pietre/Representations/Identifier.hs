module Lang.Pietre.Representations.Identifier where

import "this" Prelude

import Prettyprinter


newtype Identifier = Identifier { rawIdentifier :: Text }
  deriving (Show, Eq, Ord, Hashable, Pretty, IsString)
