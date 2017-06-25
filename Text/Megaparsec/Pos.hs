-- |
-- Module      :  Text.Megaparsec.Pos
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Textual source position. The position includes name of file, line number,
-- column number, and total number of processed tokens. List of such
-- positions can be used to model a stack of include files.
--
-- You probably do not want to import this module because "Text.Megaparsec"
-- re-exports it anyway.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Text.Megaparsec.Pos
  ( -- * Abstract position
    Pos
  , mkPos
  , unPos
  , pos1
  , defaultTabWidth
  , InvalidPosException (..)
    -- * Source position
  , SourcePos (..)
  , initialPos
  , sourcePosPretty )
where

import Control.DeepSeq
import Control.Exception
import Data.Data (Data)
import Data.Proxy
import Data.Semigroup
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.TypeLits
import Test.QuickCheck
import qualified Data.Text                  as T
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Word (Word)
#endif

----------------------------------------------------------------------------
-- Abstract position

-- | @'Pos' n@ is the type of integer with minimal value @n@. This is used
-- to represent line number, column number, and similar things like
-- indentation level. 'Semigroup' instance can be used to safely and
-- efficiently add 'Pos'es together. 'Monoid' instance is defined only for
-- @'Pos' 0@, which is used to track the total number of tokens processed.
--
-- @since 6.0.0

newtype Pos (n :: Nat) = Pos Word
  deriving (Show, Eq, Ord, Data, Typeable, NFData)

instance KnownNat n => Arbitrary (Pos n) where
  arbitrary = mkPos <$> (getSmall <$> arbitrary `suchThat` (>= m))
    where
      m = fromIntegral (natVal (Proxy :: Proxy n))

-- | Construction of 'Pos' from an instance of 'Integral'. The function
-- throws 'InvalidPosException' when given non-positive argument. Note that
-- the function is polymorphic with respect to 'MonadThrow' @m@, so you can
-- get result inside of 'Maybe', for example.
--
-- @since 6.0.0

mkPos :: forall n. KnownNat n => Word -> Pos n
mkPos a =
  if a < m
    then throw (InvalidPosException m a)
    else Pos a
  where
    m = fromInteger (natVal (Proxy :: Proxy n))
{-# INLINE mkPos #-}

-- | Extract 'Word' from 'Pos'.
--
-- @since 6.0.0

unPos :: Pos n -> Word
unPos (Pos w) = w
{-# INLINE unPos #-}

-- | Position with value 1.
--
-- @since 6.0.0

pos1 :: (CmpNat n 2 ~ 'LT, KnownNat n) => Pos n
pos1 = mkPos 1

-- | Value of tab width used by default. Always prefer this constant when
-- you want to refer to the default tab width because actual value /may/
-- change in future.
--
-- @since 6.0.0

defaultTabWidth :: (CmpNat n 9 ~ 'LT, KnownNat n) => Pos n
defaultTabWidth = mkPos 8

instance Semigroup (Pos n) where
  (Pos x) <> (Pos y) = Pos (x + y)
  {-# INLINE (<>) #-}

instance n ~ 0 => Monoid (Pos n) where
  mempty  = mkPos 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance KnownNat n => Read (Pos n) where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Pos", r2) <- lex r1
      (x,     r3) <- readsPrec 11 r2
      return (mkPos (x :: Word) ,r3)

-- | The exception is thrown by 'mkPos' when its argument is greater than
-- the minimal value implied by the type of resulting value.
--
-- @since 6.0.0

data InvalidPosException = InvalidPosException Word Word
  -- ^ The first value is the minimal allowed value, the second value is the
  -- actual value that was passed to 'mkPos'.
  deriving (Eq, Show, Data, Typeable, Generic)

instance Exception InvalidPosException
instance NFData    InvalidPosException

----------------------------------------------------------------------------
-- Source position

-- | The data type @SourcePos@ represents source positions. It contains the
-- name of the source file, a line number, and a column number. Source line
-- and column positions change intensively during parsing, so we need to
-- make them strict to avoid memory leaks.
--
-- @since 6.0.0

data SourcePos = SourcePos
  { sourceName   :: FilePath -- ^ Name of source file
  , sourceLine   :: !(Pos 1) -- ^ Line number
  , sourceColumn :: !(Pos 1) -- ^ Column number
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance NFData SourcePos

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> sized (\n -> do
          k <- choose (0, n `div` 2)
          vectorOf k arbitrary)
    <*> arbitrary
    <*> arbitrary

-- | Construct initial position (line 1, column 1) given name of source
-- file.

initialPos :: FilePath -> SourcePos
initialPos n = SourcePos n pos1 pos1

-- | Pretty-print a 'SourcePos'.
--
-- @since 6.0.0

sourcePosPretty :: SourcePos -> TB.Builder
sourcePosPretty (SourcePos n l c)
  | null n    = showLC
  | otherwise = TB.fromText (T.pack n) <> ":" <> showLC
  where
    showLC = TB.decimal (unPos l) <> ":" <> TB.decimal (unPos c)
