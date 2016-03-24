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

module Text.Megaparsec.Pos
  ( SourcePos
  , sourceName
  , sourceLine
  , sourceColumn
  , InvalidTextualPosition (..)
  , newPos
  , initialPos
  , incSourceLine
  , incSourceColumn
  , setSourceName
  , setSourceLine
  , setSourceColumn
  , defaultUpdatePos
  , defaultTabWidth )
where

import Control.Exception (Exception, throw)
import Data.Typeable (Typeable)

-- | The abstract data type @SourcePos@ represents source positions. It
-- contains the name of the source (i.e. file name), a line number and a
-- column number. @SourcePos@ is an instance of the 'Show', 'Eq' and 'Ord'
-- class.

data SourcePos = SourcePos
  { -- | Extract the name of the source from a source position.
    sourceName   :: !String
    -- | Extract the line number from a source position.
  , sourceLine   :: !Int
    -- | Extract the column number from a source position.
  , sourceColumn :: !Int }
  deriving (Eq, Ord)

instance Show SourcePos where
  show (SourcePos n l c)
    | null n    = showLC
    | otherwise = n ++ ":" ++ showLC
      where showLC = show l ++ ":" ++ show c

-- | This exception is thrown when some action on 'SourcePos' is performed
-- that would make column number or line number inside this data structure
-- non-positive.
--
-- The 'InvalidTextualPosition' structure includes in order:
--
--     * name of file
--     * line number (possibly non-positive value)
--     * column number (possibly non-positive value)

data InvalidTextualPosition =
  InvalidTextualPosition String Int Int
  deriving (Eq, Show, Typeable)

instance Exception InvalidTextualPosition

-- | Create a new 'SourcePos' with the given source name, line number and
-- column number.
--
-- If line number of column number is not positive, 'InvalidTextualPosition'
-- will be thrown.

newPos :: String -- ^ File name
       -> Int    -- ^ Line number, minimum is 1
       -> Int    -- ^ Column number, minimum is 1
       -> SourcePos
newPos n l c =
  if l < 1 || c < 1
    then throw $ InvalidTextualPosition n l c
    else SourcePos n l c
{-# INLINE newPos #-}

-- | Create a new 'SourcePos' with the given source name, and line number
-- and column number set to 1, the upper left.

initialPos :: String -> SourcePos
initialPos name = newPos name 1 1
{-# INLINE initialPos #-}

-- | Increment the line number of a source position. If resulting line
-- number is not positive, 'InvalidTextualPosition' will be thrown.

incSourceLine :: Int -> SourcePos -> SourcePos
incSourceLine d (SourcePos n l c) = newPos n (l + d) c
{-# INLINE incSourceLine #-}

-- | Increment the column number of a source position. If resulting column
-- number is not positive, 'InvalidTextualPosition' will be thrown.

incSourceColumn :: Int -> SourcePos -> SourcePos
incSourceColumn d (SourcePos n l c) = newPos n l (c + d)
{-# INLINE incSourceColumn #-}

-- | Set the name of the source.

setSourceName :: String -> SourcePos -> SourcePos
setSourceName n (SourcePos _ l c) = newPos n l c
{-# INLINE setSourceName #-}

-- | Set the line number of a source position. If the line number is not
-- positive, 'InvalidTextualPosition' will be thrown.

setSourceLine :: Int -> SourcePos -> SourcePos
setSourceLine l (SourcePos n _ c) = newPos n l c
{-# INLINE setSourceLine #-}

-- | Set the column number of a source position. If the line number is not
-- positive, 'InvalidTextualPosition' will be thrown.

setSourceColumn :: Int -> SourcePos -> SourcePos
setSourceColumn c (SourcePos n l _) = newPos n l c
{-# INLINE setSourceColumn #-}

-- | Update a source position given a character. The first argument
-- specifies tab width. If the character is a newline (\'\\n\') the line
-- number is incremented by 1. If the character is a tab (\'\\t\') the
-- column number is incremented to the nearest tab position, i.e. @column +
-- width - ((column - 1) \`rem\` width)@. In all other cases, the column is
-- incremented by 1.
--
-- If given tab width is not positive, 'defaultTabWidth' will be used.

defaultUpdatePos
  :: Int               -- ^ Tab width
  -> SourcePos         -- ^ Current position
  -> Char              -- ^ Current token
  -> (SourcePos, SourcePos) -- ^ Actual position and incremented position
defaultUpdatePos width apos@(SourcePos n l c) ch = (apos, npos)
  where
    npos =
      case ch of
        '\n' -> SourcePos n (l + 1) 1
        '\t' -> let w = if width < 1 then defaultTabWidth else width
                in SourcePos n l (c + w - ((c - 1) `rem` w))
        _    -> SourcePos n l (c + 1)

-- | Value of tab width used by default. Always prefer this constant when
-- you want to refer to default tab width because actual value /may/ change
-- in future. Current value is @8@.

defaultTabWidth :: Int
defaultTabWidth = 8
