-- |
-- Module      :  Text.Megaparsec.Error.Builder
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A set of helpers that should make construction of 'ParseError's more
-- concise. This is primarily useful in test suites and for debugging, you
-- most certainly don't need it for normal usage.
--
-- @since 6.0.0

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Megaparsec.Error.Builder
  ( -- * Top-level helpers
    err
  , errFancy
    -- * Error position
  , posI
  , posN
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

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Data types

-- | Auxiliary type for construction of trivial parse errors.

data ET t = ET (Maybe (ErrorItem t)) (Set (ErrorItem t))
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Ord t => Semigroup (ET t) where
  ET us0 ps0 <> ET us1 ps1 = ET (n us0 us1) (E.union ps0 ps1)
    where
      n Nothing  Nothing = Nothing
      n (Just x) Nothing = Just x
      n Nothing (Just y) = Just y
      n (Just x) (Just y) = Just (max x y)

instance Ord t => Monoid (ET t) where
  mempty  = ET Nothing E.empty
  mappend = (<>)

-- | Auxiliary type for construction of fancy parse errors.

data EF e = EF (Set (ErrorFancy e))
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Ord e => Semigroup (EF e) where
  EF xs0 <> EF xs1 = EF (E.union xs0 xs1)

instance Ord e => Monoid (EF e) where
  mempty  = EF E.empty
  mappend = (<>)

----------------------------------------------------------------------------
-- Top-level helpers

-- | Assemble a 'ParseError' from source position and @'ET' t@ value. To
-- create source position, two helpers are available: 'posI' and 'posN'.
-- @'ET' t@ is a monoid and can be built from primitives provided by this
-- module, see below.

err
  :: NonEmpty SourcePos -- ^ 'ParseError' position
  -> ET t              -- ^ Error components
  -> ParseError t e    -- ^ Resulting 'ParseError'
err pos (ET us ps) = TrivialError pos us ps

-- | Much like 'err', but constructs a “fancy” 'ParseError'.

errFancy
  :: NonEmpty SourcePos -- ^ 'ParseError' position
  -> EF e              -- ^ Error components
  -> ParseError t e    -- ^ Resulting 'ParseError'
errFancy pos (EF xs) = FancyError pos xs

----------------------------------------------------------------------------
-- Error position

-- | Initial source position with empty file name.

posI :: NonEmpty SourcePos
posI = initialPos "" :| []

-- | @'posN' n s@ returns source position achieved by applying 'advanceN'
-- method corresponding to the type of stream @s@.

posN :: forall s. Stream s
  => Int
  -> s
  -> NonEmpty SourcePos
posN n s =
  case takeN_ n s of
    Nothing -> posI
    Just (ts, _) ->
      advanceN (Proxy :: Proxy s) defaultTabWidth (initialPos "") ts :| []

----------------------------------------------------------------------------
-- Error components

-- | Construct an “unexpected token” error component.

utok :: Ord t => t -> ET t
utok = unexp . Tokens . nes

-- | Construct an “unexpected tokens” error component. Empty string produces
-- 'EndOfInput'.

utoks :: Ord t => [t] -> ET t
utoks = unexp . canonicalizeTokens

-- | Construct an “unexpected label” error component. Do not use with empty
-- strings (for empty strings it's bottom).

ulabel :: Ord t => String -> ET t
ulabel = unexp . Label . NE.fromList

-- | Construct an “unexpected end of input” error component.

ueof :: Ord t => ET t
ueof = unexp EndOfInput

-- | Construct an “expected token” error component.

etok :: Ord t => t -> ET t
etok = expe . Tokens . nes

-- | Construct an “expected tokens” error component. Empty string produces
-- 'EndOfInput'.

etoks :: Ord t => [t] -> ET t
etoks = expe . canonicalizeTokens

-- | Construct an “expected label” error component. Do not use with empty
-- strings.

elabel :: Ord t => String -> ET t
elabel = expe . Label . NE.fromList

-- | Construct an “expected end of input” error component.

eeof :: Ord t => ET t
eeof = expe EndOfInput

-- | Construct a custom error component.

fancy :: ErrorFancy e -> EF e
fancy = EF . E.singleton

----------------------------------------------------------------------------
-- Helpers

-- | Construct appropriate 'ErrorItem' representation for given token
-- stream. Empty string produces 'EndOfInput'.

canonicalizeTokens :: [t] -> ErrorItem t
canonicalizeTokens ts =
  case NE.nonEmpty ts of
    Nothing -> EndOfInput
    Just xs -> Tokens xs

-- | Lift an unexpected item into 'ET'.

unexp :: ErrorItem t -> ET t
unexp u = ET (pure u) E.empty

-- | Lift an expected item into 'ET'.

expe :: ErrorItem t -> ET t
expe p = ET Nothing (E.singleton p)

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
