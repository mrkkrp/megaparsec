-- |
-- Module      :  Text.Megaparsec.Pos
-- Copyright   :  © 2015–2016 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Textual source position. The position includes name of file, line number,
-- and column number. List of such positions can be used to model stack of
-- include files.

{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections              #-}

module Text.Megaparsec.Pos
  ( -- * Abstract position
    Pos
  , mkPos
  , unPos
  , unsafePos
  , InvalidPosException (..)
    -- * Source position
  , SourcePos (..)
  , initialPos
  , sourcePosPretty
    -- * Helpers implementing default behaviors
  , defaultUpdatePos
  , defaultTabWidth )
where

import Control.DeepSeq
import Control.Monad.Catch
import Data.Data (Data)
import Data.Semigroup
import Data.Typeable (Typeable)
import GHC.Generics
import Test.QuickCheck
import Unsafe.Coerce

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Word (Word)
#endif

----------------------------------------------------------------------------
-- Abstract position

-- | Positive integer that is used to represent line number, column number,
-- and similar things like indentation level. 'Semigroup' instance can be
-- used to safely and purely add 'Pos'es together.
--
-- @since 5.0.0

newtype Pos = Pos Word
  deriving (Show, Eq, Ord, Data, Typeable, NFData)

instance Arbitrary Pos where
  arbitrary = unsafePos <$> (getSmall <$> arbitrary `suchThat` (> 0))

-- | Construction of 'Pos' from an instance of 'Integral'. The function
-- throws 'InvalidPosException' when given non-positive argument. Note that
-- the function is polymorphic with respect to 'MonadThrow' @m@, so you can
-- get result inside of 'Maybe', for example.
--
-- @since 5.0.0

mkPos :: (Integral a, MonadThrow m) => a -> m Pos
mkPos x =
  if x < 1
    then throwM InvalidPosException
    else (return . Pos . fromIntegral) x
{-# INLINE mkPos #-}

-- | Dangerous construction of 'Pos'. Use when you know for sure that
-- argument is positive.
--
-- @since 5.0.0

unsafePos :: Word -> Pos
unsafePos x =
  if x < 1
    then error "Text.Megaparsec.Pos.unsafePos"
    else Pos x
{-# INLINE unsafePos #-}

-- | Extract 'Word' from 'Pos'.
--
-- @since 5.0.0

unPos :: Pos -> Word
unPos = unsafeCoerce
{-# INLINE unPos #-}

instance Semigroup Pos where
  (Pos x) <> (Pos y) = Pos (x + y)
  {-# INLINE (<>) #-}

instance Read Pos where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Pos", r2) <- lex r1
      (x,     r3) <- readsPrec 11 r2
      (,r3) <$> mkPos (x :: Integer)

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> sized (\n -> do
          k <- choose (0, n `div` 2)
          vectorOf k arbitrary)
    <*> (unsafePos <$> choose (1, 1000))
    <*> (unsafePos <$> choose (1,  100))

-- | The exception is thrown by 'mkPos' when its argument is not a positive
-- number.
--
-- @since 5.0.0

data InvalidPosException = InvalidPosException
  deriving (Eq, Show, Data, Typeable, Generic)

instance Exception InvalidPosException
instance NFData    InvalidPosException

----------------------------------------------------------------------------
-- Source position

-- | The data type @SourcePos@ represents source positions. It contains the
-- name of the source file, a line number, and a column number. Source line
-- and column positions change intensively during parsing, so we need to
-- make them strict to avoid memory leaks.

data SourcePos = SourcePos
  { sourceName   :: FilePath -- ^ Name of source file
  , sourceLine   :: !Pos     -- ^ Line number
  , sourceColumn :: !Pos     -- ^ Column number
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance NFData SourcePos

-- | Construct initial position (line 1, column 1) given name of source
-- file.

initialPos :: String -> SourcePos
initialPos n = SourcePos n u u
  where u = unsafePos 1
{-# INLINE initialPos #-}

-- | Pretty-print a 'SourcePos'.
--
-- @since 5.0.0

sourcePosPretty :: SourcePos -> String
sourcePosPretty (SourcePos n l c)
  | null n    = showLC
  | otherwise = n ++ ":" ++ showLC
  where showLC = show (unPos l) ++ ":" ++ show (unPos c)

----------------------------------------------------------------------------
-- Helpers implementing default behaviors

-- | Update a source position given a character. The first argument
-- specifies tab width. If the character is a newline (\'\\n\') the line
-- number is incremented by 1. If the character is a tab (\'\\t\') the
-- column number is incremented to the nearest tab position. In all other
-- cases, the column is incremented by 1.
--
-- @since 5.0.0

defaultUpdatePos
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Char              -- ^ Current token
  -> (SourcePos, SourcePos) -- ^ Actual position and incremented position
defaultUpdatePos width apos@(SourcePos n l c) ch = (apos, npos)
  where
    u = unsafePos 1
    w = unPos width
    c' = unPos c
    npos =
      case ch of
        '\n' -> SourcePos n (l <> u) u
        '\t' -> SourcePos n l (unsafePos $ c' + w - ((c' - 1) `rem` w))
        _    -> SourcePos n l (c <> u)

-- | Value of tab width used by default. Always prefer this constant when
-- you want to refer to default tab width because actual value /may/ change
-- in future. Current value is @8@.

defaultTabWidth :: Pos
defaultTabWidth = unsafePos 8
