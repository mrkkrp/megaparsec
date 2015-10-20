-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's generic parser combinators.
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
-- This software is provided by the copyright holders "as is" and any
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

module Combinator (tests) where

import Control.Applicative
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList, isNothing, fromJust)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator

import Util

tests :: Test
tests = testGroup "Generic parser combinators"
        [ testProperty "combinator between"   prop_between
        , testProperty "combinator choice"    prop_choice
        , testProperty "combinator count"     prop_count
        , testProperty "combinator count'"    prop_count'
        , testProperty "combinator endBy"     prop_endBy
        , testProperty "combinator endBy1"    prop_endBy1
        , testProperty "combinator manyTill"  prop_manyTill
        , testProperty "combinator someTill"  prop_someTill
        , testProperty "combinator option"    prop_option
        , testProperty "combinator sepBy"     prop_sepBy
        , testProperty "combinator sepBy1"    prop_sepBy1
        , testProperty "combinator sepEndBy"  prop_sepEndBy
        , testProperty "combinator sepEndBy1" prop_sepEndBy1
        , testProperty "combinator skipMany"  prop_skipMany
        , testProperty "combinator skipSome"  prop_skipSome ]

prop_between :: String -> Char -> NonNegative Int -> String -> Property
prop_between pre c n' post = checkParser p r s
  where p = between (string pre) (string post) (many (char c))
        n = getNonNegative n'
        b = length $ takeWhile (== c) post
        r | b > 0 = posErr (length pre + n + b) s $ exStr post :
                    if length post == b
                    then [uneEof]
                    else [uneCh (post !! b), exCh c]
          | otherwise = Right z
        z = replicate n c
        s = pre ++ z ++ post

prop_choice :: NonEmptyList Char -> Char -> Property
prop_choice cs' s' = checkParser p r s
  where cs = getNonEmpty cs'
        p = choice $ char <$> cs
        r | s' `elem` cs = Right s'
          | otherwise    = posErr 0 s $ uneCh s' : (exCh <$> cs)
        s = [s']

prop_count :: Int -> NonNegative Int -> Property
prop_count n x' = checkParser p r s
  where x = getNonNegative x'
        p = count n (char 'x')
        r = simpleParse (count' n n (char 'x')) s
        s = replicate x 'x'

prop_count' :: Int -> Int -> NonNegative Int -> Property
prop_count' m n x' = checkParser p r s
  where x = getNonNegative x'
        p = count' m n (char 'x')
        r | n <= 0 || m > n  =
              if x == 0
              then Right ""
              else posErr 0 s [uneCh 'x', exEof]
          | m <= x && x <= n = Right s
          | x < m            = posErr x s [uneEof, exCh 'x']
          | otherwise        = posErr n s [uneCh 'x', exEof]
        s = replicate x 'x'

prop_endBy :: NonNegative Int -> Char -> Property
prop_endBy n' c = checkParser p r s
  where n = getNonNegative n'
        p = endBy (char 'a') (char '-')
        r | c == 'a' && n == 0 = posErr 1 s [uneEof, exCh '-']
          | c == 'a'           = posErr (g n) s [uneCh 'a', exCh '-']
          | c == '-' && n == 0 = posErr 0 s [uneCh '-', exCh 'a', exEof]
          | c /= '-'           = posErr (g n) s $ uneCh c :
                                 (if n > 0 then exCh '-' else exEof) :
                                 [exCh 'a' | n == 0]
          | otherwise = Right (replicate n 'a')
        s = intersperse '-' (replicate n 'a') ++ [c]

prop_endBy1 :: NonNegative Int -> Char -> Property
prop_endBy1 n' c = checkParser p r s
  where n = getNonNegative n'
        p = endBy1 (char 'a') (char '-')
        r | c == 'a' && n == 0 = posErr 1 s [uneEof, exCh '-']
          | c == 'a'           = posErr (g n) s [uneCh 'a', exCh '-']
          | c == '-' && n == 0 = posErr 0 s [uneCh '-', exCh 'a']
          | c /= '-'           = posErr (g n) s $ uneCh c :
                                 [exCh '-' | n > 0] ++
                                 [exCh 'a' | n == 0]
          | otherwise = Right (replicate n 'a')
        s = intersperse '-' (replicate n 'a') ++ [c]

prop_manyTill :: NonNegative Int -> NonNegative Int
              -> NonNegative Int -> Property
prop_manyTill a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (,) <$> manyTill letterChar (char 'c') <*> many letterChar
        r | c == 0    = posErr (a + b) s [uneEof, exCh 'c', exSpec "letter"]
          | otherwise = let (pre, post) = break (== 'c') s
                        in Right (pre, drop 1 post)
        s = abcRow a b c

prop_someTill :: NonNegative Int -> NonNegative Int
              -> NonNegative Int -> Property
prop_someTill a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (,) <$> someTill letterChar (char 'c') <*> many letterChar
        r | null s    = posErr 0 s [uneEof, exSpec "letter"]
          | c == 0    = posErr (a + b) s [uneEof, exCh 'c', exSpec "letter"]
          | s == "c"  = posErr 1 s [uneEof, exCh 'c', exSpec "letter"]
          | head s == 'c' = Right ("c", drop 2 s)
          | otherwise = let (pre, post) = break (== 'c') s
                        in Right (pre, drop 1 post)
        s = abcRow a b c

prop_option :: String -> String -> String -> Property
prop_option d a s = checkParser p r s
  where p = option d (string a)
        r = simpleParse (fromMaybe d <$> optional (string a)) s

prop_sepBy :: NonNegative Int -> Maybe Char -> Property
prop_sepBy n' c' = checkParser p r s
  where n = getNonNegative n'
        c = fromJust c'
        p = sepBy (char 'a') (char '-')
        r | isNothing c' = Right (replicate n 'a')
          | c == 'a' && n == 0 = Right "a"
          | n == 0    = posErr 0 s [uneCh c, exCh 'a', exEof]
          | c == '-'  = posErr (length s) s [uneEof, exCh 'a']
          | otherwise = posErr (g n) s [uneCh c, exCh '-', exEof]
        s = intersperse '-' (replicate n 'a') ++ maybeToList c'

prop_sepBy1 :: NonNegative Int -> Maybe Char -> Property
prop_sepBy1 n' c' = checkParser p r s
  where n = getNonNegative n'
        c = fromJust c'
        p = sepBy1 (char 'a') (char '-')
        r | isNothing c' && n >= 1 = Right (replicate n 'a')
          | isNothing c' = posErr 0 s [uneEof, exCh 'a']
          | c == 'a' && n == 0 = Right "a"
          | n == 0    = posErr 0 s [uneCh c, exCh 'a']
          | c == '-'  = posErr (length s) s [uneEof, exCh 'a']
          | otherwise = posErr (g n) s [uneCh c, exCh '-', exEof]
        s = intersperse '-' (replicate n 'a') ++ maybeToList c'

prop_sepEndBy :: NonNegative Int -> Maybe Char -> Property
prop_sepEndBy n' c' = checkParser p r s
  where n = getNonNegative n'
        c = fromJust c'
        p = sepEndBy (char 'a') (char '-')
        a = Right $ replicate n 'a'
        r | isNothing c' = a
          | c == 'a' && n == 0 = Right "a"
          | n == 0    = posErr 0 s [uneCh c, exCh 'a', exEof]
          | c == '-'  = a
          | otherwise = posErr (g n) s [uneCh c, exCh '-', exEof]
        s = intersperse '-' (replicate n 'a') ++ maybeToList c'

prop_sepEndBy1 :: NonNegative Int -> Maybe Char -> Property
prop_sepEndBy1 n' c' = checkParser p r s
  where n = getNonNegative n'
        c = fromJust c'
        p = sepEndBy1 (char 'a') (char '-')
        a = Right $ replicate n 'a'
        r | isNothing c' && n >= 1 = a
          | isNothing c' = posErr 0 s [uneEof, exCh 'a']
          | c == 'a' && n == 0 = Right "a"
          | n == 0    = posErr 0 s [uneCh c, exCh 'a']
          | c == '-'  = a
          | otherwise = posErr (g n) s [uneCh c, exCh '-', exEof]
        s = intersperse '-' (replicate n 'a') ++ maybeToList c'

prop_skipMany :: Char -> NonNegative Int -> String -> Property
prop_skipMany c n' a = checkParser p r s
  where p = skipMany (char c) *> string a
        n = getNonNegative n'
        r = simpleParse (many (char c) >> string a) s
        s = replicate n c ++ a

prop_skipSome :: Char -> NonNegative Int -> String -> Property
prop_skipSome c n' a = checkParser p r s
  where p = skipSome (char c) *> string a
        n = getNonNegative n'
        r = simpleParse (some (char c) >> string a) s
        s = replicate n c ++ a

g :: Int -> Int
g x = x + if x > 0 then x - 1 else 0
