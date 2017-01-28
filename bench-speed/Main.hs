--
-- Criterion benchmarks for Megaparsec.
--
-- Copyright © 2015–2017 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

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
