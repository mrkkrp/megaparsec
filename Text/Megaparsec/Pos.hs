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
-- Textual source positions.

module Text.Megaparsec.Pos
    ( SourceName
    , Line
    , Column
    , SourcePos
    , sourceLine
    , sourceColumn
    , sourceName
    , incSourceLine
    , incSourceColumn
    , setSourceLine
    , setSourceColumn
    , setSourceName
    , newPos
    , initialPos
    , updatePosChar
    , updatePosString )
where

import Data.Data (Data)
import Data.List (foldl')
import Data.Typeable (Typeable)

-- | @SourceName@ is a file name, in our case it's @String@.

type SourceName = String

-- | @Line@ represents line number, 1 is the minimum.

type Line = Int

-- | @Column@ is column number, 1 is the the minimum.

type Column = Int

-- | The abstract data type @SourcePos@ represents source positions. It
-- contains the name of the source (i.e. file name), a line number and a
-- column number. @SourcePos@ is an instance of the 'Show', 'Eq' and 'Ord'
-- class.

data SourcePos = SourcePos
    { -- | Extract the name of the source from a source position.
      sourceName   :: SourceName
      -- | Extract the line number from a source position.
    , sourceLine   :: !Line
      -- | Extract the column number from a source position.
    , sourceColumn :: !Column }
    deriving (Eq, Ord, Data, Typeable)

instance Show SourcePos where
  show (SourcePos n l c)
    | null n    = showLC
    | otherwise = "\"" ++ n ++ "\" " ++ showLC
    where showLC = "(line " ++ show l ++ ", column " ++ show c ++ ")"

-- | Create a new 'SourcePos' with the given source name,
-- line number and column number.

newPos :: SourceName -> Line -> Column -> SourcePos
newPos = SourcePos

-- | Create a new 'SourcePos' with the given source name,
-- and line number and column number set to 1, the upper left.

initialPos :: SourceName -> SourcePos
initialPos name = newPos name 1 1

-- | Increment the line number of a source position.

incSourceLine :: SourcePos -> Line -> SourcePos
incSourceLine (SourcePos n l c) d = SourcePos n (l + d) c

-- | Increments the column number of a source position.

incSourceColumn :: SourcePos -> Column -> SourcePos
incSourceColumn (SourcePos n l c) d = SourcePos n l (c + d)

-- | Set the name of the source.

setSourceName :: SourcePos -> SourceName -> SourcePos
setSourceName (SourcePos _ l c) n = SourcePos n l c

-- | Set the line number of a source position.

setSourceLine :: SourcePos -> Line -> SourcePos
setSourceLine (SourcePos n _ c) l = SourcePos n l c

-- | Set the column number of a source position.

setSourceColumn :: SourcePos -> Column -> SourcePos
setSourceColumn (SourcePos n l _) = SourcePos n l

-- | The expression @updatePosString pos s@ updates the source position
-- @pos@ by calling 'updatePosChar' on every character in @s@, i.e. @foldl
-- updatePosChar pos string@.

updatePosString :: SourcePos -> String -> SourcePos
updatePosString = foldl' updatePosChar

-- | Update a source position given a character. If the character is a
-- newline (\'\\n\') or carriage return (\'\\r\') the line number is
-- incremented by 1. If the character is a tab (\'\t\') the column number is
-- incremented to the nearest 8'th column, i.e. @column + 8 - ((column-1)
-- \`mod\` 8)@. In all other cases, the column is incremented by 1.

updatePosChar :: SourcePos -> Char -> SourcePos
updatePosChar (SourcePos n l c) ch =
    case ch of
      '\n' -> SourcePos n (l + 1) 1
      '\t' -> SourcePos n l (c + 8 - ((c - 1) `mod` 8))
      _    -> SourcePos n l (c + 1)
