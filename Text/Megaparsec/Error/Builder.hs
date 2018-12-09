-- |
-- Module      :  Text.Megaparsec.Error.Builder
-- Copyright   :  © 2015–2018 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A set of helpers that should make construction of 'ParseError's more
-- concise. This is primarily useful in test suites and for debugging.
--
-- @since 6.0.0

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Megaparsec.Error.Builder
  ( -- * Top-level helpers
    err
  , errFancy
    -- * Error components
  , utok
  , utoks
  , ulabel
  , ueof
  , etok
  , etoks
  , elabel
  , eeof
  , fancy
    -- * Data types
  , ET
  , EF )
where

import           Data.Data              (Data)
import           Data.List.NonEmpty     (NonEmpty (..))
import qualified Data.List.NonEmpty     as NE
import           Data.Set               (Set)
import qualified Data.Set               as E
import           Data.Typeable          (Typeable)
import           GHC.Generics
import           Text.Megaparsec.Error
import           Text.Megaparsec.Stream

import           Data.MonoTraversable

#if !MIN_VERSION_base(4,11,0)
import           Data.Semigroup
#endif

----------------------------------------------------------------------------
-- Data types

-- | Auxiliary type for construction of trivial parse errors.

data ET s = ET (Maybe (ErrorItem (Element (Tokens s)))) (Set (ErrorItem (Element (Tokens s))))
  deriving (Typeable, Generic)

deriving instance Eq (Element (Tokens s)) => Eq (ET s)

deriving instance Ord (Element (Tokens s)) => Ord (ET s)

deriving instance ( Data s
                  , Data (Element (Tokens s))
                  , Ord (Element (Tokens s))
                  ) => Data (ET s)

instance Stream s => Semigroup (ET s) where
  ET us0 ps0 <> ET us1 ps1 = ET (n us0 us1) (E.union ps0 ps1)
    where
      n Nothing  Nothing  = Nothing
      n (Just x) Nothing  = Just x
      n Nothing (Just y)  = Just y
      n (Just x) (Just y) = Just (max x y)

instance Stream s => Monoid (ET s) where
  mempty  = ET Nothing E.empty
  mappend = (<>)

-- | Auxiliary type for construction of fancy parse errors.

newtype EF e = EF (Set (ErrorFancy e))
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Ord e => Semigroup (EF e) where
  EF xs0 <> EF xs1 = EF (E.union xs0 xs1)

instance Ord e => Monoid (EF e) where
  mempty  = EF E.empty
  mappend = (<>)

----------------------------------------------------------------------------
-- Top-level helpers

-- | Assemble a 'ParseError' from offset and @'ET' t@ value. @'ET' t@ is a
-- monoid and can be assembled by combining primitives provided by this
-- module, see below.

err
  :: Int               -- ^ 'ParseError' offset
  -> ET s              -- ^ Error components
  -> ParseError s e    -- ^ Resulting 'ParseError'
err p (ET us ps) = TrivialError p us ps

-- | Like 'err', but constructs a “fancy” 'ParseError'.

errFancy
  :: Int               -- ^ 'ParseError' offset
  -> EF e              -- ^ Error components
  -> ParseError s e    -- ^ Resulting 'ParseError'
errFancy p (EF xs) = FancyError p xs

----------------------------------------------------------------------------
-- Error components

-- | Construct an “unexpected token” error component.

utok :: Stream s => Element (Tokens s) -> ET s
utok = unexp . Tokens . nes

-- | Construct an “unexpected tokens” error component. Empty chunk produces
-- 'EndOfInput'.

utoks :: forall s. Stream s => Tokens s -> ET s
utoks = unexp . canonicalizeTokens @s

-- | Construct an “unexpected label” error component. Do not use with empty
-- strings (for empty strings it's bottom).

ulabel :: Stream s => String -> ET s
ulabel label
  | label == "" = error "Text.Megaparsec.Error.Builder.ulabel: empty label"
  | otherwise = unexp . Label . NE.fromList $ label

-- | Construct an “unexpected end of input” error component.

ueof :: Stream s => ET s
ueof = unexp EndOfInput

-- | Construct an “expected token” error component.

etok :: Stream s => Element (Tokens s) -> ET s
etok = expe . Tokens . nes

-- | Construct an “expected tokens” error component. Empty chunk produces
-- 'EndOfInput'.

etoks :: forall s. Stream s => Tokens s -> ET s
etoks = expe . canonicalizeTokens @s

-- | Construct an “expected label” error component. Do not use with empty
-- strings.

elabel :: Stream s => String -> ET s
elabel label
  | label == "" = error "Text.Megaparsec.Error.Builder.elabel: empty label"
  | otherwise = expe . Label . NE.fromList $ label

-- | Construct an “expected end of input” error component.

eeof :: Stream s => ET s
eeof = expe EndOfInput

-- | Construct a custom error component.

fancy :: ErrorFancy e -> EF e
fancy = EF . E.singleton

----------------------------------------------------------------------------
-- Helpers

-- | Construct appropriate 'ErrorItem' representation for given token
-- stream. Empty string produces 'EndOfInput'.

canonicalizeTokens
  :: forall s . Stream s
  => Tokens s
  -> ErrorItem (Element (Tokens s))
canonicalizeTokens ts =
  case NE.nonEmpty (otoList ts) of
    Nothing -> EndOfInput
    Just xs -> Tokens xs

-- | Lift an unexpected item into 'ET'.

unexp :: Stream s => ErrorItem (Element (Tokens s)) -> ET s
unexp u = ET (pure u) E.empty

-- | Lift an expected item into 'ET'.

expe :: Stream s => ErrorItem (Element (Tokens s)) -> ET s
expe p = ET Nothing (E.singleton p)

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
