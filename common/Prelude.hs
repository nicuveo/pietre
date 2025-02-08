{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

{- |

Internal module that re-exports the regular prelude, plus some common useful
functions.

-}

module Prelude
  ( -- * re-exports from useful "default" modules
    module P
    -- * custom operators
  , (...)
    -- * maybe helpers
  , onNothing
  , onNothingM
    -- * either helpers
  , onLeft
  , onLeftM
    -- * sequence helpers
  , Seq (.., Lone)
    -- * hashmap helpers
  , unionWithM
  , unionWithKeyM
  ) where


-- re-exports

import Control.Applicative        as P (liftA)
import Control.Arrow              as P (first, left, second, (&&&), (***),
                                        (<<<), (>>>))
import Control.Monad              as P
import Control.Monad.Except       as P
import Control.Monad.Identity     as P
import Control.Monad.Reader       as P
import Control.Monad.State.Strict as P
import Control.Monad.Trans.Maybe  as P (MaybeT (..))
import Data.Bifunctor             as P (bimap)
import Data.Bool                  as P (bool)
import Data.Char                  as P (chr, ord)
import Data.Either                as P (lefts, partitionEithers, rights)
import Data.Foldable              as P (asum, fold, foldMap', foldlM, foldrM,
                                        for_, toList, traverse_)
import Data.Function              as P (on, (&))
import Data.Functor               as P (($>), (<&>))
import Data.Functor.Const         as P (Const (..))
import Data.Hashable              as P (Hashable)
import Data.HashMap.Strict        as P (HashMap, mapKeys)
import Data.HashSet               as P (HashSet)
import Data.List                  as P (find, findIndex, foldl', group,
                                        intercalate, intersect, intersperse,
                                        lookup, sort, sortBy, sortOn, union,
                                        unionBy, (\\))
import Data.List.NonEmpty         as P (NonEmpty (..), nonEmpty)
import Data.Maybe                 as P (catMaybes, fromMaybe, isJust, isNothing,
                                        listToMaybe, maybeToList)
import Data.Ord                   as P (comparing)
import Data.Semigroup             as P (Semigroup (..))
import Data.Sequence              as P (Seq)
import Data.String                as P (IsString)
import Data.Text                  as P (Text)
import Data.Traversable           as P (for)
import Data.Void                  as P (Void, absurd)
import GHC.Generics               as P (Generic)
import "base" Prelude             as P hiding (lookup)


-- internal imports

import Data.HashMap.Strict        qualified as M
import Data.Sequence              (Seq (..))


-- operators

(...) :: (c -> d) -> (a -> b -> c) -> (a -> b -> d)
f ... g = \x y -> f (g x y)
infixr 8 ...


-- maybe helpers

onNothing :: Applicative m => Maybe a -> m a -> m a
onNothing a d = maybe d pure a

onNothingM :: Monad m => m (Maybe a) -> m a -> m a
onNothingM a d = a >>= flip onNothing d


-- either helpers

onLeft :: Applicative m => Either e a -> (e -> m a) -> m a
onLeft a f = either f pure a

onLeftM :: Monad m => m (Either e a) -> (e -> m a) -> m a
onLeftM a f = a >>= flip onLeft f


-- sequence helpers

pattern Lone :: a -> Seq a
pattern Lone x = x :<| Empty


-- hashmap helpers

unionWithM ::
  (Monad m, Hashable k) =>
  (v -> v -> m v) ->
  HashMap k v ->
  HashMap k v ->
  m (HashMap k v)
unionWithM = unionWithKeyM . const

unionWithKeyM ::
  (Monad m, Hashable k) =>
  (k -> v -> v -> m v) ->
  HashMap k v ->
  HashMap k v ->
  m (HashMap k v)
unionWithKeyM f m1 m2 = foldM step m1 (M.toList m2)
  where
    step m (k, new) = case M.lookup k m of
      Nothing -> pure $ M.insert k new m
      Just old -> do
        combined <- f k new old
        pure $ M.insert k combined m
