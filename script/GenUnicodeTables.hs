-- |
-- Module      :  Main
-- Copyright   :  © 2026–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Generate "Text.Megaparsec.Unicode.Tables" from the Unicode Character
-- Database. See the “Regenerating the Unicode tables” section of
-- @HACKING.md@ for usage.
--
-- The script only needs the packages that come with GHC.
module Main (main) where

import Control.Monad (forM, unless, when)
import Data.Char (isHexDigit, isSpace, ord)
import Data.List (foldl', intercalate, sort)
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import System.Process (callProcess)

----------------------------------------------------------------------------
-- Configuration

-- | The version of the Unicode Character Database to use by default.
defaultVersion :: String
defaultVersion = "17.0.0"

-- | The files we need and where they live relative to the UCD root.
ucdFiles :: [(FilePath, FilePath)]
ucdFiles =
  [ ("EastAsianWidth.txt", "EastAsianWidth.txt"),
    ("DerivedGeneralCategory.txt", "extracted/DerivedGeneralCategory.txt")
  ]

-- | The general categories whose characters take up no space at all. This
-- is the classic @wcwidth@ definition: control characters, non-spacing and
-- enclosing marks, and formatting characters (which includes the soft
-- hyphen, the zero width space, and the variation selectors).
zeroWidthCategories :: [String]
zeroWidthCategories = ["Cc", "Mn", "Me", "Cf"]

-- | Conjoining Hangul jamo of the medial (vowel) and final (trailing)
-- kinds. They combine with the preceding leading jamo instead of occupying
-- a cell of their own, but their general category does not say so.
extraZeroWidth :: [Range]
extraZeroWidth = [(0x1160, 0x11ff)]

----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  (version, mucdDir) <- parseArgs
  paths <- case mucdDir of
    Just dir -> forM ucdFiles $ \(name, _) -> do
      let path = dir ++ "/" ++ name
      exists <- doesFileExist path
      unless exists (die ("no such file: " ++ path))
      return (name, path)
    Nothing -> forM ucdFiles $ \(name, relPath) -> do
      let url =
            "https://www.unicode.org/Public/"
              ++ version
              ++ "/ucd/"
              ++ relPath
          path = "ucd-" ++ version ++ "-" ++ name
      exists <- doesFileExist path
      unless exists $ do
        hPutStrLn stderr ("Fetching " ++ url)
        callProcess "curl" ["--silent", "--show-error", "--fail", "--output", path, url]
      return (name, path)
  eaw <- parseUcd <$> readFile' (lookup' "EastAsianWidth.txt" paths)
  gc <- parseUcd <$> readFile' (lookup' "DerivedGeneralCategory.txt" paths)
  let zeroWidth =
        normalize (extraZeroWidth ++ selectValues zeroWidthCategories gc)
      -- A character that combines with its neighbour takes up no space even
      -- when its East Asian Width says it is wide. Removing those from the
      -- wide table leaves the two disjoint, which is what lets 'charLength'
      -- consult them in whichever order is faster.
      wide = normalize (selectValues ["W", "F"] eaw) `without` zeroWidth
  when (null zeroWidth || null wide) (die "the parsed tables are empty")
  limit <- either die return (simpleCharLimit wide zeroWidth)
  putStr (render version limit wide zeroWidth)
  hPutStrLn stderr $
    "Generated "
      ++ show (length wide)
      ++ " wide ranges and "
      ++ show (length zeroWidth)
      ++ " zero-width ranges from Unicode "
      ++ version
      ++ ", fast path limit "
      ++ showHex' limit
  where
    readFile' path = length <$> readFile path >> readFile path
    lookup' k xs = maybe (error ("impossible: " ++ k)) id (lookup k xs)

parseArgs :: IO (String, Maybe FilePath)
parseArgs = do
  args <- getArgs
  progName <- getProgName
  let usage =
        die $
          "usage: "
            ++ progName
            ++ " [VERSION] [--ucd-dir DIR]\n\n\
               \Writes Text/Megaparsec/Unicode/Tables.hs to stdout. Without\n\
               \--ucd-dir the required files are downloaded with curl and cached\n\
               \in the working directory. VERSION defaults to "
            ++ defaultVersion
            ++ "."
  case args of
    [] -> return (defaultVersion, Nothing)
    ["--help"] -> usage
    ["-h"] -> usage
    [v] | not ("-" `isPrefixOf'` v) -> return (v, Nothing)
    ["--ucd-dir", dir] -> return (defaultVersion, Just dir)
    [v, "--ucd-dir", dir] -> return (v, Just dir)
    _ -> usage
  where
    isPrefixOf' p s = take (length p) s == p

----------------------------------------------------------------------------
-- Parsing of the UCD files

-- | An inclusive range of code points.
type Range = (Int, Int)

-- | A property value together with the code points that have it.
type Assignment = (String, Range)

-- | Parse a UCD file in the common @CODE[..CODE] ; VALUE # comment@ format.
parseUcd :: String -> [Assignment]
parseUcd = concatMap (parseLine . takeWhile (/= '#')) . lines
  where
    parseLine ln =
      case break (== ';') ln of
        (codes, ';' : value) ->
          case parseRange (trim codes) of
            Just r -> [(trim value, r)]
            Nothing -> []
        _ -> []
    parseRange s =
      case break (== '.') s of
        (lo, '.' : '.' : hi)
          | isHex lo && isHex hi -> Just (readHex lo, readHex hi)
        (lo, "")
          | isHex lo -> Just (readHex lo, readHex lo)
        _ -> Nothing
    isHex s = not (null s) && all isHexDigit s

-- | Return the ranges whose property value is one of the given ones.
selectValues :: [String] -> [Assignment] -> [Range]
selectValues values as = [r | (v, r) <- as, v `elem` values]

-- | Sort the ranges and coalesce the ones that touch or overlap.
normalize :: [Range] -> [Range]
normalize = go . sort
  where
    go ((lo1, hi1) : (lo2, hi2) : rest)
      | lo2 <= hi1 + 1 = go ((lo1, max hi1 hi2) : rest)
      | otherwise = (lo1, hi1) : go ((lo2, hi2) : rest)
    go rs = rs

-- | Remove from the first collection of ranges everything that the second
-- one covers. Both are expected to be normalized.
without :: [Range] -> [Range] -> [Range]
without xs ys = normalize (concatMap sub xs)
  where
    sub r = foldl' (\acc y -> concatMap (`subtract1` y) acc) [r] ys
    subtract1 (lo, hi) (lo', hi')
      | hi < lo' || hi' < lo = [(lo, hi)] -- disjoint
      | otherwise =
          [(lo, lo' - 1) | lo < lo']
            ++ [(hi' + 1, hi) | hi' < hi]

----------------------------------------------------------------------------
-- The fast path

-- | Below the returned code point the width of a character is decided by a
-- handful of comparisons instead of a table lookup, see 'charLength'. Fail
-- if the data no longer supports the assumption baked into that code, in
-- which case both this function and 'charLength' need to be revisited.
simpleCharLimit :: [Range] -> [Range] -> Either String Int
simpleCharLimit wide zeroWidth = do
  -- The soft hyphen is the last thing below the limit that the fast path
  -- knows about, so the limit is whatever comes after it.
  let limit = minimum [lo | (lo, _) <- wide ++ zeroWidth, lo > 0xad]
      predicted n = n < 0x20 || (n >= 0x7f && n <= 0x9f) || n == 0xad
      actual n = any (\(lo, hi) -> lo <= n && n <= hi) zeroWidth
      wrong =
        [ n
        | n <- [0 .. limit - 1],
          predicted n /= actual n || any (\(lo, hi) -> lo <= n && n <= hi) wide
        ]
  unless (null wrong) $
    Left
      ( "the fast path in Text.Megaparsec.Unicode disagrees with the data \
        \for the following code points: "
          ++ intercalate ", " (map showHex' (take 10 wrong))
      )
  return limit

----------------------------------------------------------------------------
-- Rendering

render :: String -> Int -> [Range] -> [Range] -> String
render version limit wide zeroWidth =
  unlines
    [ "{-# LANGUAGE Safe #-}",
      "",
      "-- |",
      "-- Module      :  Text.Megaparsec.Unicode.Tables",
      "-- Copyright   :  © 2026–present Megaparsec contributors",
      "-- License     :  FreeBSD",
      "--",
      "-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>",
      "-- Stability   :  experimental",
      "-- Portability :  portable",
      "--",
      "-- Character width data extracted from the Unicode Character",
      "-- Database.",
      "--",
      "-- __This module is generated by @script\\/GenUnicodeTables.hs@, do not",
      "-- edit it by hand.__",
      "module Text.Megaparsec.Unicode.Tables",
      "  ( unicodeVersion,",
      "    simpleCharLimit,",
      "    wideCharRanges,",
      "    zeroWidthCharRanges,",
      "  )",
      "where",
      "",
      "import Data.Array (Array, listArray)",
      "",
      "-- | The version of the Unicode Character Database that the tables in",
      "-- this module were extracted from.",
      "unicodeVersion :: String",
      "unicodeVersion = " ++ show version,
      "",
      "-- | Below this code point a character is neither wide nor zero-width,",
      "-- with the exception of the C0 and C1 control characters and the soft",
      "-- hyphen. This is what allows the common case to avoid a table lookup",
      "-- altogether.",
      "simpleCharLimit :: Int",
      "simpleCharLimit = " ++ showHex' limit,
      "",
      "-- | Ranges of characters that span two columns, that is, those whose",
      "-- East Asian Width is Wide or Fullwidth. Zero-width characters are",
      "-- excluded, so the two tables are disjoint.",
      "wideCharRanges :: Array Int (Int, Int)",
      "wideCharRanges =",
      renderArray wide,
      "{-# NOINLINE wideCharRanges #-}",
      "",
      "-- | Ranges of characters that take up no space at all: control",
      "-- characters, non-spacing and enclosing marks, formatting characters,",
      "-- and conjoining Hangul jamo of the medial and final kinds.",
      "zeroWidthCharRanges :: Array Int (Int, Int)",
      "zeroWidthCharRanges =",
      renderArray zeroWidth
    ]
    ++ "{-# NOINLINE zeroWidthCharRanges #-}\n"
  where
    renderArray rs =
      intercalate
        "\n"
        ( [ "  listArray",
            "    (0, " ++ show (length rs - 1) ++ ")"
          ]
            ++ zipWith entry [0 ..] rs
            ++ ["    ]"]
        )
      where
        entry i (lo, hi) =
          (if i == (0 :: Int) then "    [ " else "      ")
            ++ "("
            ++ showHex' lo
            ++ ", "
            ++ showHex' hi
            ++ ")"
            ++ (if i == length rs - 1 then "" else ",")

----------------------------------------------------------------------------
-- Helpers

readHex :: String -> Int
readHex = foldl' (\acc c -> acc * 16 + digit c) 0
  where
    digit c
      | c >= '0' && c <= '9' = ord c - ord '0'
      | c >= 'a' && c <= 'f' = ord c - ord 'a' + 10
      | otherwise = ord c - ord 'A' + 10

-- | Render a code point the way the rest of the tables do, padded to at
-- least six digits so that the columns line up.
showHex' :: Int -> String
showHex' n = "0x" ++ pad (go n "")
  where
    go 0 acc = if null acc then "0" else acc
    go m acc = go (m `div` 16) (digits !! (m `mod` 16) : acc)
    digits = "0123456789abcdef"
    pad s = replicate (max 0 (6 - length s)) '0' ++ s

trim :: String -> String
trim = dropWhile isSpace . reverse . dropWhile isSpace . reverse
