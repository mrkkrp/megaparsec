-- |
-- Module      :  Text.Megaparsec.Pos
-- Copyright   :  © 2015 Megaparsec contributors
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
  , updatePosChar
  , updatePosString
  , defaultTabWidth )
where

import Control.Exception (Exception, throw)
import Data.List (foldl')
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

-- | Create a new 'SourcePos' with the given source name, and line number
-- and column number set to 1, the upper left.

initialPos :: String -> SourcePos
initialPos name = newPos name 1 1

-- | Increment the line number of a source position. If resulting line
-- number is not positive, 'InvalidTextualPosition' will be thrown.

incSourceLine :: SourcePos -> Int -> SourcePos
incSourceLine (SourcePos n l c) d = newPos n (l + d) c

-- | Increment the column number of a source position. If resulting column
-- number is not positive, 'InvalidTextualPosition' will be thrown.

incSourceColumn :: SourcePos -> Int -> SourcePos
incSourceColumn (SourcePos n l c) d = newPos n l (c + d)

-- | Set the name of the source.

setSourceName :: SourcePos -> String -> SourcePos
setSourceName (SourcePos _ l c) n = newPos n l c

-- | Set the line number of a source position. If the line number is not
-- positive, 'InvalidTextualPosition' will be thrown.

setSourceLine :: SourcePos -> Int -> SourcePos
setSourceLine (SourcePos n _ c) l = newPos n l c

-- | Set the column number of a source position. If the line number is not
-- positive, 'InvalidTextualPosition' will be thrown.

setSourceColumn :: SourcePos -> Int -> SourcePos
setSourceColumn (SourcePos n l _) = newPos n l

-- | Update a source position given a character. The first argument
-- specifies tab width. If the character is a newline (\'\\n\') the line
-- number is incremented by 1. If the character is a tab (\'\\t\') the
-- column number is incremented to the nearest tab position, i.e. @column +
-- width - ((column - 1) \`rem\` width)@. In all other cases, the column is
-- incremented by 1.
--
-- If given tab width is not positive, 'defaultTabWidth' will be used.

updatePosChar :: Int       -- ^ Tab width
              -> SourcePos -- ^ Initial position
              -> Char      -- ^ Character at the position
              -> SourcePos
updatePosChar width (SourcePos n l c) ch =
  case ch of
    '\n' -> SourcePos n (l + 1) 1
    '\t' -> let w = if width < 1 then defaultTabWidth else width
            in SourcePos n l (c + w - ((c - 1) `rem` w))
    _    -> SourcePos n l (c + 1)

-- | The expression @updatePosString pos s@ updates the source position
-- @pos@ by calling 'updatePosChar' on every character in @s@, i.e.
--
-- > updatePosString width = foldl (updatePosChar width)

updatePosString :: Int       -- ^ Tab width
                -> SourcePos -- ^ Initial position
                -> String    -- ^ String to process
                -> SourcePos
updatePosString w = foldl' (updatePosChar w)

-- | Value of tab width used by default. This is used as fall-back by
-- 'updatePosChar' and possibly in other cases. Always prefer this constant
-- when you want to refer to default tab width because actual value /may/
-- change in future. Current value is @8@.

defaultTabWidth :: Int
defaultTabWidth = 8
