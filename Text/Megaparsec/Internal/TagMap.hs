-- |
-- Module      :  Text.Megaparsec.Internal.TagMap
-- Copyright   :  © 2019 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal definitions. Versioning rules do not apply here. Please do not
-- rely on these unless you really know what you're doing.
--
-- @since 8.0.0

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}
{-# LANGUAGE Rank2Types                 #-}

module Text.Megaparsec.Internal.TagMap
  ( -- * Data types
    Tag
  , TagMap
    -- * Helper functions
  , makeTag
  , insert
  , singleton
  , Text.Megaparsec.Internal.TagMap.lookup
  , tmap
    -- * Implementation-specific types and helpers
  , SomeTag
  , looseTag )
where

import Data.Foldable
import System.Mem.StableName (StableName, makeStableName, eqStableName)
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (Any)
import Unsafe.Coerce (unsafeCoerce)

-- | A TagMap is used for memoization. It associates tags
-- (based on 'StableName's) with an associated value of the same
-- type.
newtype TagMap f = TagMap { unTagMap :: [(SomeTag f, f Any)] }

-- | Value tag used for memoization
newtype Tag f a = Tag { unTag :: StableName (f a) }

-- | Value tag with its inner type @a@ hidden.
data SomeTag f = forall a. SomeTag (StableName (f a))

-- | Convert the outer type @f@ of some TagMap into another type @f'@.
tmap :: forall f f'. (forall a. f a -> f' a) -> TagMap f -> TagMap f'
tmap f = TagMap . fmap f' . unTagMap
  where
    f' :: (SomeTag f, f Any) -> (SomeTag f', f' Any)
    -- This unsafeCoerce is harmless, since the StableName type variable
    -- actually has a phantom role (this is going to be fixed in GHC 8.8)
    -- This is because StableNames operate on values, not types, and
    -- two values of different types can never have the same StableName.
    f' (SomeTag k, v) = (SomeTag (unsafeCoerce k), f v)

-- | Tag equality based on stable name equality ('eqStableName').
instance Eq (SomeTag f) where
  -- Note: This works even though the SomeTags are existentially qualified
  -- because eqStableName does not care about the StableName type variable.
  (SomeTag lhs) == (SomeTag rhs) = lhs `eqStableName` rhs

-- | Forget the inner type @a@ of some @Tag f a@ by transforming it into
-- a @SomeTag f@.
looseTag :: Tag f a -> SomeTag f
looseTag = SomeTag . unTag

-- | Tag a value.
makeTag :: f a -> Tag f a
makeTag !p = Tag . unsafePerformIO . makeStableName $ p
{-# NOINLINE makeTag #-}

-- | /O(1)/. A @'TagMap' f@ with only one element.
singleton :: Functor f => Tag f v -> f v -> TagMap f
singleton !(looseTag -> k) (fmap unsafeCoerce -> v) = TagMap [(k, v)]
{-# INLINE singleton #-}

-- | /O(1)/. Insert a new element into a @'TagMap' f@. Does not check for duplicates.
insert :: Functor f => Tag f v -> f v -> TagMap f -> TagMap f
insert !(looseTag -> k) (fmap unsafeCoerce -> v) = TagMap . ((k, v):) . unTagMap
{-# INLINE insert #-}

-- | /O(n)/. Look up a value @f v@ associated with some @'Tag' f v@.
lookup :: forall f v. Functor f => Tag f v -> TagMap f -> Maybe (f v)
lookup !(looseTag -> k) = fmap conv . find ((== k) . fst) . unTagMap
  where
    conv :: (SomeTag f, f Any) -> f v
    conv = fmap unsafeCoerce . snd
{-# INLINE lookup #-}
