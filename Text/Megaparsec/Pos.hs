-- |
-- Module      :  Text.Megaparsec.Pos
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Textual source position.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TupleSections      #-}

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

import Control.Monad.Catch
import Data.Semigroup
import Data.Typeable (Typeable)

----------------------------------------------------------------------------
-- Pos

-- | Positive integer that is used to represent line number, column number,
-- and similar things like indentation level.

newtype Pos = Pos Word deriving (Show, Eq, Ord)

-- | Construction of 'Pos' from 'Word'. Zero values will throw 'InvalidPos'.

mkPos :: (MonadThrow m, Integral a) => a -> m Pos
mkPos x =
  if x < 1
    then throwM InvalidPosException
    else (return . Pos . fromIntegral) x
{-# INLINE mkPos #-}

-- | Dangerous construction of 'Pos'. Use when you know for sure that
-- argument is positive.

unsafePos :: Word -> Pos
unsafePos x =
  if x < 1
    then error "Text.Megaparsec.Pos.unsafePos"
    else Pos x
{-# INLINE unsafePos #-}

-- | Extract 'Word' from 'Pos'.

unPos :: Pos -> Word
unPos (Pos x) = x
{-# INLINE  unPos #-}

instance Semigroup Pos where
  (Pos x) <> (Pos y) = Pos (x + y)
  {-# INLINE (<>) #-}

instance Read Pos where
  readsPrec d =
    readParen (d > 10) $ \r1 -> do
      ("Pos", r2) <- lex r1
      (x,     r3) <- readsPrec 11 r2
      (,r3) <$> mkPos (x :: Integer)

-- | The exception is thrown by 'mkPos' when its argument is not a positive
-- number.

data InvalidPosException = InvalidPosException deriving (Eq, Show, Typeable)

instance Exception InvalidPosException

----------------------------------------------------------------------------
-- Source position

-- | The data type @SourcePos@ represents source positions. It contains the
-- name of the source file, a line number and a column number.

data SourcePos = SourcePos
  { sourceName   :: String  -- ^ Name of source file
  , sourceLine   :: !Pos    -- ^ Line number
  , sourceColumn :: !Pos    -- ^ Column number
  } deriving (Show, Read, Eq, Ord)

-- | Construct initial position (line 1, column 1) given name of source
-- file.

initialPos :: String -> SourcePos
initialPos n = SourcePos n u u
  where u = unsafePos 1

-- | Pretty-print a 'SourcePos'.

sourcePosPretty :: SourcePos -> String
sourcePosPretty (SourcePos n l c)
  | null n    = showLC
  | otherwise = n ++ ":" ++ showLC
  where showLC = show (unPos l) ++ ":" ++ show (unPos c)

----------------------------------------------------------------------------
-- Helpers implementing default behavior

-- | Update a source position given a character. The first argument
-- specifies tab width. If the character is a newline (\'\\n\') the line
-- number is incremented by 1. If the character is a tab (\'\\t\') the
-- column number is incremented to the nearest tab position, i.e. @column +
-- width - ((column - 1) \`rem\` width)@. In all other cases, the column is
-- incremented by 1.

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
