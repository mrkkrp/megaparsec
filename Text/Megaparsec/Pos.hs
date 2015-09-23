-- |
-- Module      :  Text.Megaparsec.Pos
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
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
  , incSourceLine
  , incSourceColumn
  , setSourceName
  , setSourceLine
  , setSourceColumn
  , newPos
  , initialPos
  , updatePosChar
  , updatePosString )
where

import Data.Data (Data)
import Data.List (foldl')
import Data.Typeable (Typeable)

-- | The abstract data type @SourcePos@ represents source positions. It
-- contains the name of the source (i.e. file name), a line number and a
-- column number. @SourcePos@ is an instance of the 'Show', 'Eq' and 'Ord'
-- class.

data SourcePos = SourcePos
  { -- | Extract the name of the source from a source position.
    sourceName   :: String
    -- | Extract the line number from a source position.
  , sourceLine   :: !Int
    -- | Extract the column number from a source position.
  , sourceColumn :: !Int }
  deriving (Eq, Ord, Data, Typeable)

instance Show SourcePos where
  show (SourcePos n l c)
    | null n    = showLC
    | otherwise = "\"" ++ n ++ "\" " ++ showLC
    where showLC = "line " ++ show l ++ ", column " ++ show c

-- | Create a new 'SourcePos' with the given source name, line number and
-- column number.

newPos :: String -> Int -> Int -> SourcePos
newPos = SourcePos

-- | Create a new 'SourcePos' with the given source name, and line number
-- and column number set to 1, the upper left.

initialPos :: String -> SourcePos
initialPos name = newPos name 1 1

-- | Increment the line number of a source position.

incSourceLine :: SourcePos -> Int -> SourcePos
incSourceLine (SourcePos n l c) d = SourcePos n (l + d) c

-- | Increments the column number of a source position.

incSourceColumn :: SourcePos -> Int -> SourcePos
incSourceColumn (SourcePos n l c) d = SourcePos n l (c + d)

-- | Set the name of the source.

setSourceName :: SourcePos -> String -> SourcePos
setSourceName (SourcePos _ l c) n = SourcePos n l c

-- | Set the line number of a source position.

setSourceLine :: SourcePos -> Int -> SourcePos
setSourceLine (SourcePos n _ c) l = SourcePos n l c

-- | Set the column number of a source position.

setSourceColumn :: SourcePos -> Int -> SourcePos
setSourceColumn (SourcePos n l _) = SourcePos n l

-- | Update a source position given a character. If the character is a
-- newline (\'\\n\') the line number is incremented by 1. If the character
-- is carriage return (\'\\r\') the line number is unaltered, but column
-- number is reset to 1. If the character is a tab (\'\\t\') the column
-- number is incremented to the nearest 8'th column, i.e. @column + 8 -
-- ((column - 1) \`rem\` 8)@. In all other cases, the column is incremented
-- by 1.

updatePosChar :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos n l c) ch =
  case ch of
    '\n' -> SourcePos n (l + 1) 1
    '\t' -> SourcePos n l (c + 8 - ((c - 1) `rem` 8))
    _    -> SourcePos n l (c + 1)

-- | The expression @updatePosString pos s@ updates the source position
-- @pos@ by calling 'updatePosChar' on every character in @s@, i.e. @foldl
-- updatePosChar pos string@.

updatePosString :: SourcePos -> String -> SourcePos
updatePosString = foldl' updatePosChar
