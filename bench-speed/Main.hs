module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Text.Megaparsec
import Text.Megaparsec.String

main :: IO ()
main = defaultMain
  [ bparser "string"   manyAs (string . fst)
  , bparser "string'"  manyAs (string' . fst)
  , bparser "choice"   (const "b") (choice . fmap char . manyAsB . snd)
  , bparser "many"     manyAs (const $ many (char 'a'))
  , bparser "some"     manyAs (const $ some (char 'a'))
  , bparser "count"    manyAs (\(_,n) -> count n (char 'a'))
  , bparser "count'"   manyAs (\(_,n) -> count' 1 n (char 'a'))
  , bparser "endBy"    manyAbs' (const $ endBy (char 'a') (char 'b'))
  , bparser "endBy1"   manyAbs' (const $ endBy1 (char 'a') (char 'b'))
  , bparser "sepBy"    manyAbs (const $ sepBy (char 'a') (char 'b'))
  , bparser "sepBy1"   manyAbs (const $ sepBy1 (char 'a') (char 'b'))
  , bparser "sepEndBy"  manyAbs' (const $ sepEndBy (char 'a') (char 'b'))
  , bparser "sepEndBy1" manyAbs' (const $ sepEndBy1 (char 'a') (char 'b'))
  , bparser "manyTill" manyAsB (const $ manyTill (char 'a') (char 'b'))
  , bparser "someTill" manyAsB (const $ someTill (char 'a') (char 'b'))
  ]

-- | Perform a series to measurements with the same parser.

bparser :: NFData a
  => String            -- ^ Name of the benchmark group
  -> (Int -> String)   -- ^ How to construct input
  -> ((String, Int) -> Parser a) -- ^ The parser receiving its future input
  -> Benchmark         -- ^ The benchmark
bparser name f p = bgroup name (bs <$> stdSeries)
  where
    bs n = env (return (f n, n)) (bench (show n) . nf p')
    p' (s,n) = parse (p (s,n)) "" s

-- | The series of sizes to try as part of 'bparser'.

stdSeries :: [Int]
stdSeries = [500,1000,2000,4000]

----------------------------------------------------------------------------
-- Helpers

-- | Generate that many \'a\' characters.

manyAs :: Int -> String
manyAs n = replicate n 'a'

-- | Like 'manyAs', but with a \'b\' added to the end.

manyAsB :: Int -> String
manyAsB n = replicate n 'a' ++ "b"

-- | Like 'manyAs', but interspersed with \'b\'s and ends in a \'a\'.

manyAbs :: Int -> String
manyAbs n = take (if even n then n + 1 else n) (cycle "ab")

-- | Like 'manyAbs', but ends in a \'b\'.

manyAbs' :: Int -> String
manyAbs' n = take (if even n then n else n + 1) (cycle "ab")
