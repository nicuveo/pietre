{-# LANGUAGE TemplateHaskell #-}

module Lang.Pietre.Representations.Location where

import "this" Prelude

import Control.Lens


data Location = Location
  { _locFilename :: Filepath
  , _locAddress  :: Int
  , _locLine     :: Int
  , _locColumn   :: Int
  }
  deriving (Show, Eq, Ord)

makeLenses ''Position

initialPosition :: Filepath -> Location
initialPosition f = Location f 0 1 1

updatePosition :: Location -> Char -> Location
updatePosition (Location f a l c) = \case
  '\n' -> Location f (a+1) (l+1) 1
  _    -> Location f (a+1) l (c+1)

type WithLocation a = (Location, a)
