{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Location where

import "this" Prelude

import Control.Lens


data Location = Location
  { _locFilename :: FilePath
  , _locAddress  :: Int
  , _locLine     :: Int
  , _locColumn   :: Int
  }
  deriving (Show, Eq, Ord)

data WithLocation a = WithLocation
  { _location :: Location
  , _located  :: a
  }
  deriving (Show, Functor, Traversable, Foldable)

makeLenses ''Location
makeLenses ''WithLocation


initialLocation :: FilePath -> Location
initialLocation f = Location f 0 1 1

updateLocation :: Location -> Char -> Location
updateLocation (Location f a l c) = \case
  '\n' -> Location f (a+1) (l+1) 1
  _    -> Location f (a+1) l (c+1)
