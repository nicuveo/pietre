module Data.HashMap.Strict.Extra
  ( module P
  , catMaybes
  , traverseWithKey_
  , forWithKey
  , forWithKey_
  ) where

import "this" Prelude      hiding (catMaybes)

import Data.HashMap.Strict as P

catMaybes :: HashMap k (Maybe v) -> HashMap k v
catMaybes = P.mapMaybe id

traverseWithKey_
  :: Applicative f
  => (k -> v1 -> f v2)
  -> HashMap k v1
  -> f ()
traverseWithKey_ = void ... traverseWithKey

forWithKey
  :: Applicative f
  => HashMap k v1
  -> (k -> v1 -> f v2)
  -> f (HashMap k v2)
forWithKey = flip P.traverseWithKey

forWithKey_
  :: Applicative f
  => HashMap k v1
  -> (k -> v1 -> f v2)
  -> f ()
forWithKey_ = flip traverseWithKey_
