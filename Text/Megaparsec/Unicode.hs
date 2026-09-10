{-# LANGUAGE Safe #-}

-- |
-- Module      :  Text.Megaparsec.Unicode
-- Copyright   :  © 2024–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions for working with Unicode.
--
-- @since 9.7.0
module Text.Megaparsec.Unicode
  ( stringLength,
    charLength,
    isWideChar,
    isZeroWidthChar,
  )
where

import Data.Array (Array, bounds, (!))
import Data.Char (ord)
import Text.Megaparsec.Unicode.Tables

-- | Calculate the length of a string, taking into account the fact that
-- certain 'Char's may span more than 1 column.
--
-- @since 9.7.0
stringLength :: (Traversable t) => t Char -> Int
stringLength = sum . fmap charLength

-- | Return the length of an individual 'Char'.
--
-- @since 9.7.0
charLength :: Char -> Int
charLength ch
  | n < simpleCharLimit = if isSimpleZeroWidth n then 0 else 1
  -- The two tables are disjoint, so the order of the lookups only affects
  -- speed. Wide characters are looked up first because scripts that use
  -- them use them for nearly every character, while combining marks are
  -- interspersed with characters that are in neither table.
  | inRanges wideCharRanges n = 2
  | inRanges zeroWidthCharRanges n = 0
  | otherwise = 1
  where
    n = ord ch

-- | Determine whether the given 'Char' is “wide”, that is, whether it spans
-- 2 columns instead of one.
--
-- @since 9.7.0
isWideChar :: Char -> Bool
isWideChar ch = n >= simpleCharLimit && inRanges wideCharRanges n
  where
    n = ord ch

-- | Determine whether the given 'Char' is “zero-width”, that is, whether it
-- has no visible representation and does not advance the cursor position.
-- This includes control characters and certain Unicode zero-width
-- characters.
--
-- @since 9.8.0
isZeroWidthChar :: Char -> Bool
isZeroWidthChar ch
  | n < simpleCharLimit = isSimpleZeroWidth n
  | otherwise = inRanges zeroWidthCharRanges n
  where
    n = ord ch

-- | Decide whether a code point below 'simpleCharLimit' is zero-width. Only
-- the control characters and the soft hyphen are; the generator checks that
-- this agrees with the data it produces.
isSimpleZeroWidth :: Int -> Bool
isSimpleZeroWidth n =
  n < 0x20 -- C0 control chars
    || (n >= 0x7f && n <= 0x9f) -- DEL and C1 control chars
    || n == 0xad -- soft hyphen
{-# INLINE isSimpleZeroWidth #-}

-- | Look up a code point in a sorted collection of ranges that neither
-- overlap nor touch.
inRanges :: Array Int (Int, Int) -> Int -> Bool
inRanges ranges n = go (bounds ranges)
  where
    go (lo, hi)
      | hi < lo = False
      | a <= n && n <= b = True
      | n < a = go (lo, pred mid)
      | otherwise = go (succ mid, hi)
      where
        mid = (lo + hi) `div` 2
        (a, b) = ranges ! mid
