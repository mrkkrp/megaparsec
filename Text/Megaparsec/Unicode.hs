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
  )
where

import Data.Array (Array, bounds, listArray, (!))
import Data.Char (ord)

-- | Calculate length of a string taking into account the fact that certain
-- 'Char's may span more than 1 column.
--
-- @since 9.7.0
stringLength :: (Traversable t) => t Char -> Int
stringLength = sum . fmap charLength

-- | Return length of an individual 'Char'.
--
-- @since 9.7.0
charLength :: Char -> Int
charLength ch = if isWideChar ch then 2 else 1

-- | Determine whether the given 'Char' is “wide”, that is, whether it spans
-- 2 columns instead of one.
--
-- @since 9.7.0
isWideChar :: Char -> Bool
isWideChar c = go (bounds wideCharRanges)
  where
    go (lo, hi)
      | hi < lo = False
      | a <= n && n <= b = True
      | n < a = go (lo, pred mid)
      | otherwise = go (succ mid, hi)
      where
        mid = (lo + hi) `div` 2
        (a, b) = wideCharRanges ! mid
    n = ord c

-- | Wide character ranges.
wideCharRanges :: Array Int (Int, Int)
wideCharRanges =
  listArray
    (0, 118)
    [ (0x001100, 0x00115f),
      (0x00231a, 0x00231b),
      (0x002329, 0x00232a),
      (0x0023e9, 0x0023ec),
      (0x0023f0, 0x0023f0),
      (0x0023f3, 0x0023f3),
      (0x0025fd, 0x0025fe),
      (0x002614, 0x002615),
      (0x002648, 0x002653),
      (0x00267f, 0x00267f),
      (0x002693, 0x002693),
      (0x0026a1, 0x0026a1),
      (0x0026aa, 0x0026ab),
      (0x0026bd, 0x0026be),
      (0x0026c4, 0x0026c5),
      (0x0026ce, 0x0026ce),
      (0x0026d4, 0x0026d4),
      (0x0026ea, 0x0026ea),
      (0x0026f2, 0x0026f3),
      (0x0026f5, 0x0026f5),
      (0x0026fa, 0x0026fa),
      (0x0026fd, 0x0026fd),
      (0x002705, 0x002705),
      (0x00270a, 0x00270b),
      (0x002728, 0x002728),
      (0x00274c, 0x00274c),
      (0x00274e, 0x00274e),
      (0x002753, 0x002755),
      (0x002757, 0x002757),
      (0x002795, 0x002797),
      (0x0027b0, 0x0027b0),
      (0x0027bf, 0x0027bf),
      (0x002b1b, 0x002b1c),
      (0x002b50, 0x002b50),
      (0x002b55, 0x002b55),
      (0x002e80, 0x002e99),
      (0x002e9b, 0x002ef3),
      (0x002f00, 0x002fd5),
      (0x002ff0, 0x002ffb),
      (0x003000, 0x00303e),
      (0x003041, 0x003096),
      (0x003099, 0x0030ff),
      (0x003105, 0x00312f),
      (0x003131, 0x00318e),
      (0x003190, 0x0031ba),
      (0x0031c0, 0x0031e3),
      (0x0031f0, 0x00321e),
      (0x003220, 0x003247),
      (0x003250, 0x004db5),
      (0x004e00, 0x009fef),
      (0x00a000, 0x00a48c),
      (0x00a490, 0x00a4c6),
      (0x00a960, 0x00a97c),
      (0x00ac00, 0x00d7a3),
      (0x00f900, 0x00fa6d),
      (0x00fa70, 0x00fad9),
      (0x00fe10, 0x00fe19),
      (0x00fe30, 0x00fe52),
      (0x00fe54, 0x00fe66),
      (0x00fe68, 0x00fe6b),
      (0x00ff01, 0x00ff60),
      (0x00ffe0, 0x00ffe6),
      (0x016fe0, 0x016fe3),
      (0x017000, 0x0187f7),
      (0x018800, 0x018af2),
      (0x01b000, 0x01b11e),
      (0x01b150, 0x01b152),
      (0x01b164, 0x01b167),
      (0x01b170, 0x01b2fb),
      (0x01f004, 0x01f004),
      (0x01f0cf, 0x01f0cf),
      (0x01f18e, 0x01f18e),
      (0x01f191, 0x01f19a),
      (0x01f200, 0x01f202),
      (0x01f210, 0x01f23b),
      (0x01f240, 0x01f248),
      (0x01f250, 0x01f251),
      (0x01f260, 0x01f265),
      (0x01f300, 0x01f320),
      (0x01f32d, 0x01f335),
      (0x01f337, 0x01f37c),
      (0x01f37e, 0x01f393),
      (0x01f3a0, 0x01f3ca),
      (0x01f3cf, 0x01f3d3),
      (0x01f3e0, 0x01f3f0),
      (0x01f3f4, 0x01f3f4),
      (0x01f3f8, 0x01f43e),
      (0x01f440, 0x01f440),
      (0x01f442, 0x01f4fc),
      (0x01f4ff, 0x01f53d),
      (0x01f54b, 0x01f54e),
      (0x01f550, 0x01f567),
      (0x01f57a, 0x01f57a),
      (0x01f595, 0x01f596),
      (0x01f5a4, 0x01f5a4),
      (0x01f5fb, 0x01f64f),
      (0x01f680, 0x01f6c5),
      (0x01f6cc, 0x01f6cc),
      (0x01f6d0, 0x01f6d2),
      (0x01f6d5, 0x01f6d5),
      (0x01f6eb, 0x01f6ec),
      (0x01f6f4, 0x01f6fa),
      (0x01f7e0, 0x01f7eb),
      (0x01f90d, 0x01f971),
      (0x01f973, 0x01f976),
      (0x01f97a, 0x01f9a2),
      (0x01f9a5, 0x01f9aa),
      (0x01f9ae, 0x01f9ca),
      (0x01f9cd, 0x01f9ff),
      (0x01fa70, 0x01fa73),
      (0x01fa78, 0x01fa7a),
      (0x01fa80, 0x01fa82),
      (0x01fa90, 0x01fa95),
      (0x020000, 0x02a6d6),
      (0x02a700, 0x02b734),
      (0x02b740, 0x02b81d),
      (0x02b820, 0x02cea1),
      (0x02ceb0, 0x02ebe0),
      (0x02f800, 0x02fa1d)
    ]
{-# NOINLINE wideCharRanges #-}
