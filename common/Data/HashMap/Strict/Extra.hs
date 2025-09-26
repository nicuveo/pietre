module Data.HashMap.Strict.Extra
  ( module P
  , catMaybes
  ) where

import "this" Prelude      hiding (catMaybes)

import Data.HashMap.Strict as P

catMaybes :: HashMap k (Maybe v) -> HashMap k v
catMaybes = P.mapMaybe id
