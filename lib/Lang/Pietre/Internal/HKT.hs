module Lang.Pietre.Internal.HKT where

import "this" Prelude

import Unsafe.Coerce


--------------------------------------------------------------------------------
-- * HKT

-- | This type family allows for erasure of 'Identity' in HKTs.
--
-- Given a type @A'@ such that
--
--     data A' f = A
--       { x :: HKT f Int
--       , y :: HKT f Int
--       }
--
-- It means that @A' Identity@ resolves to:
--
--     data A = A
--       { x :: Int
--       , y :: Int
--       }
--
-- This makes working with HKT types easier, since it makes it possible to deal
-- with individual fields without having to manually coerce to and from
-- @Identity@.
type family HKT f a where
  HKT Identity a = a
  HKT f        a = f a


--------------------------------------------------------------------------------
-- * HKT conversion
--
-- We can always convert between @f a@ and @HKT f a@, since for all cases but
-- @Identity@, @f a ~ HKT f a@, and for @Identity@ the conversion is trivial.
-- That means that in all cases, they are representationally equivalent, and the
-- conversion could be performed by 'coerce'. However, using @coerce@ would mean
-- delegating @forall a. Coercible (f a) (HKT f a)@ and @forall a. Coercible
-- (HKT f a) (f a)@ instances all the way to the call sites where @f@ is known,
-- and quantified constraints don't play well with non-injective type
-- families...
--
-- As a result, since we KNOW that @HKT f a@ and @f a@ are representationally
-- equivalent for all @f@ and @a@ WITHOUT EXCEPTION, we can safely use the
-- dreaded 'unsafeCoerce'.

toHKT :: forall f a. f a -> HKT f a
toHKT = unsafeCoerce

fromHKT :: forall f a. HKT f a -> f a
fromHKT = unsafeCoerce


--------------------------------------------------------------------------------
-- * Functor-like functions

-- | @HKT f@ is not a functor for all @f@ because of @Identity@; furthermore
-- @HKT f@ is nonsensical by itself since it's a partial type family
-- application. We therefore can't define @fmap@ for @HKT f@, but we can define
-- an equivalent @hmap@ with identical semantics.
hmap :: forall f a b. Functor f => (a -> b) -> HKT f a -> HKT f b
hmap f = toHKT @f . fmap f . fromHKT @f

-- | Similarly to 'hmap', this provides an equivalent to 'pure'.
hpure :: forall f a. Applicative f => a -> HKT f a
hpure = toHKT @f . pure

type FFunction f g = forall a. f a -> g a

-- | This class describes how a HKT type is kind of a "higher-kind functor": we
-- can convert from @A f@ to @A g@ given a function that converts from @f a@ to
-- @g a@ for all @a@.
class FFunctor t where
  ffmap :: forall f g. (Functor f, Functor g) => FFunction f g -> t f -> t g

-- | Apply a @f a -> g a@ function on a @HKT f a@.
--
-- Used to implement @FFunctor@ instances without having to manually convert
-- back and forth with 'toHKT' and 'fromHKT'.
ffapply :: forall a f g. FFunction f g -> HKT f a -> HKT g a
ffapply f = toHKT @g . f @a . fromHKT @f

-- | Apply a @f a -> g a@ function on a @HKT f a@ where @a@ is itself a HKT type
-- parameterized by @f@: this performs a recurvise @ffmap@, and uses @ffaply@ on
-- the result.
ffrecur :: forall t f g. (Functor f, Functor g, FFunctor t) => FFunction f g -> HKT f (t f) -> HKT g (t g)
ffrecur f = ffapply @(t g) f . hmap @f (ffmap @t f)

-- | Special case of @ffmap@ where the target functor is @Identity@.
--
-- This allows the caller to provide a @f a -> a@ function instead of having to
-- use an explicit @f a -> Identity a@ one.
reify :: (Functor f, FFunctor t) => (forall a. f a -> a) -> t f -> t Identity
reify f = ffmap (Identity . f)

-- | Special case of @ffmap@ where the source functor is @Identity@.
--
-- This allows the caller to provide a @a -> f a@ function instead of having to
-- use an explicit @Identity a -> f a@ one.
abstract :: (Functor f, FFunctor t) => (forall a. a -> f a) -> t Identity -> t f
abstract f = ffmap (f . runIdentity)


--------------------------------------------------------------------------------
-- * Void

data VoidF f

instance FFunctor VoidF where
  ffmap _ = \case {}
