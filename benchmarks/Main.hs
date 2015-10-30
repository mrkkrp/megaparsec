-- -*- Mode: Haskell; -*-
--
-- Criterion benchmarks for Megaparsec.
--
-- Copyright © 2015 Megaparsec contributors
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
-- * Neither the names of the copyright holders nor the names of
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
--
-- This software is provided by the copyright holders “as is” and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

{-# LANGUAGE CPP #-}

module Main (main) where

import Criterion.Main

import Text.Megaparsec

-- Configuration parameters.

-- To configure the benchmark, build the benchmarks e.g. like this:
-- $ cabal build --ghc-options="-DBENCHMARK_TYPE=3 -DBENCHMARK_STEPS=10"

-- BENCHMARK_TYPE options:
-- 0:  Text.Megaparsec.String
-- 1:  Text.Megaparsec.Text
-- 2:  Text.Megaparsec.Text.Lazy
-- 3:  Text.Megaparsec.ByteString
-- 4:  Text.Megaparsec.ByteString.Lazy

#ifndef BENCHMARK_TYPE
#define BENCHMARK_TYPE 0
#endif

#if BENCHMARK_TYPE == 0
import Text.Megaparsec.String (Parser)
pack :: String -> String
pack = id
#endif
#if BENCHMARK_TYPE == 1
import Text.Megaparsec.Text (Parser)
import Data.Text (pack)
#endif
#if BENCHMARK_TYPE == 2
import Text.Megaparsec.Text.Lazy (Parser)
import Data.Text.Lazy (pack)
#endif
#if BENCHMARK_TYPE == 3
import Text.Megaparsec.ByteString (Parser)
import Data.ByteString.Char8 (pack)
#endif
#if BENCHMARK_TYPE == 4
import Text.Megaparsec.ByteString.Lazy (Parser)
import Data.ByteString.Lazy.Char8 (pack)
#endif

-- benchSteps and benchSize control the benchmark test points

benchSteps :: Int
#if BENCHMARK_STEPS
benchSteps = BENCHMARK_STEPS
#else
benchSteps = 5
#endif
benchSize :: Int
#if BENCHMARK_SIZE
benchSize = BENCHMARK_SIZE
#else
benchSize = 1000
#endif

-- End of configuration parameters

main :: IO ()
main = defaultMain benchmarks

benchmarks :: [Benchmark]
benchmarks =
  [
  -- First the primitives
    bgroup "string" $ benchBunch $ \size str ->
      parse (string str :: Parser String) ""
        (pack $ replicate size 'a')
  , bgroup "try-string" $ benchBunch $ \size str ->
      parse (try $ string str :: Parser String) ""
        (pack $ replicate size 'a')
  , bgroup "lookahead-string" $ benchBunch $ \size str ->
      parse (lookAhead $ string str :: Parser String) ""
        (pack $ replicate size 'a')
  , bgroup "notfollowedby-string" $ benchBunch $ \size str ->
      parse (notFollowedBy $ string str :: Parser ()) ""
        (pack $ replicate size 'a')
  , bgroup "manual-string" benchManual

  -- Now for a few combinators
  -- Major class instance operators: return, >>=, <|>, mzero
  -- TODO
  -- I'm not really sure how to test these operators since I can't imagine what
  -- supraconstant complexity they might have.

  -- A few non-primitive combinators follow below
  , bgroup "choice"
      [ bgroup "match" $ benchBunchMatch $ \size _ ->
          parse
            (choice (replicate (size-1) (char 'b') ++ [char 'a']) :: Parser Char)
            ""
            (pack $ replicate size 'a')
      , bgroup "nomatch" $ benchBunchMatch $ \size _ ->
          parse
            (choice (replicate size (char 'b')) :: Parser Char)
            ""
            (pack $ replicate size 'a')
      ]
  , bgroup "count'" benchCount
  , bgroup "sepBy1" benchSepBy1
  , bgroup "manyTill" $ benchBunchNoMatchLate $ \size _ ->
      parse
        (manyTill (char 'a') (char 'b') :: Parser String)
        ""
        (pack $ replicate (size-1) 'a' ++ "b")
  ]


benchManual :: [Benchmark]
benchManual =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf
      (parse (sequence $ fmap char (replicate num 'a') :: Parser String) "")
      (pack $ replicate num 'a')

benchCount :: [Benchmark]
benchCount =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf
      (parse (count' size (size*2) (char 'a') :: Parser String) "")
      (pack $ replicate (num-1) 'a' ++ "b")
      where
        size = round ((0.7 :: Double) * fromIntegral num)

benchSepBy1 :: [Benchmark]
benchSepBy1 =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf
      (parse (sepBy1 (char 'a') (char 'b') :: Parser String) "")
      (pack $ genString num)
    genString 0 = "ac"
    genString i = 'a' : 'b' : genString (i-1)

benchBunch :: (Int -> String -> b) -> [Benchmark]
benchBunch f =
  [ bgroup "match" $ benchBunchMatch f
  , bgroup "nomatch_early" $ benchBunchNoMatchEarly f
  , bgroup "nomatch_late" $ benchBunchNoMatchLate f
  ]

benchBunchMatch :: (Int -> String -> b) -> [Benchmark]
benchBunchMatch f =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf (f num) (replicate num 'a')

benchBunchNoMatchEarly :: (Int -> String -> b) -> [Benchmark]
benchBunchNoMatchEarly f =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf (f num) (replicate num 'b')

benchBunchNoMatchLate :: (Int -> String -> b) -> [Benchmark]
benchBunchNoMatchLate f =
  map benchOne [benchSize,benchSize*2..benchSize*benchSteps]
  where
    benchOne num = bench (show num) $ whnf (f num) (replicate (num-1) 'a' ++ "b")
