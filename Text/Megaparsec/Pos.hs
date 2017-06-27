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
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

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
import Data.Semigroup
import Data.Typeable (Typeable)
import GHC.Generics
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

-- | 'Pos' is the type for positive integers. This is used to represent line
-- number, column number, and similar things like indentation level.
-- 'Semigroup' instance can be used to safely and efficiently add 'Pos'es
-- together.
--
-- @since 5.0.0

newtype Pos = Pos Word
  deriving (Show, Eq, Ord, Data, Typeable, NFData)

instance Arbitrary Pos where
  arbitrary = mkPos <$> (getSmall <$> arbitrary `suchThat` (> 0))

-- | Construction of 'Pos' from 'Word'. The function throws
-- 'InvalidPosException' when given a non-positive argument.
--
-- @since 6.0.0

mkPos :: Word -> Pos
mkPos a =
  if a == 0
    then throw InvalidPosException
    else Pos a
{-# INLINE mkPos #-}

-- | Extract 'Word' from 'Pos'.
--
-- @since 5.0.0

unPos :: Pos -> Word
unPos (Pos w) = w
{-# INLINE unPos #-}

-- | Position with value 1.
--
-- @since 6.0.0

pos1 :: Pos
pos1 = mkPos 1

-- | Value of tab width used by default. Always prefer this constant when
-- you want to refer to the default tab width because actual value /may/
-- change in future.
--
-- @since 5.0.0

defaultTabWidth :: Pos
defaultTabWidth = mkPos 8

instance Semigroup Pos where
  (Pos x) <> (Pos y) = Pos (x + y)
  {-# INLINE (<>) #-}

instance Read Pos where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Pos", r2) <- lex r1
      (x,     r3) <- readsPrec 11 r2
      return (mkPos (x :: Word) ,r3)

-- | The exception is thrown by 'mkPos' when its argument is not a positive
-- number.
--
-- @since 5.0.0

data InvalidPosException = InvalidPosException
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
  , sourceLine   :: !Pos     -- ^ Line number
  , sourceColumn :: !Pos     -- ^ Column number
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
