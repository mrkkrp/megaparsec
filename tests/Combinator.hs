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
import Data.Maybe (fromMaybe)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator

import Util

tests :: Test
tests = testGroup "Generic parser combinators"
        [ testProperty "combinator between"   prop_between
        , testProperty "combinator chainl"    prop_chainl
        , testProperty "combinator chainl1"   prop_chainl1
        , testProperty "combinator chainr"    prop_chainr
        , testProperty "combinator chainr1"   prop_chainr1
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

prop_chainl :: Property
prop_chainl = property True

prop_chainl1 :: Property
prop_chainl1 = property True

prop_chainr :: Property
prop_chainr = property True

prop_chainr1 :: Property
prop_chainr1 = property True

prop_choice :: NonEmptyList Char -> Char -> Property
prop_choice cs' s' = checkParser p r s
  where cs = getNonEmpty cs'
        p = choice $ char <$> cs
        r | s' `elem` cs = Right s'
          | otherwise    = posErr 0 s $ uneCh s' : (exCh <$> cs)
        s = [s']

prop_count :: Property
prop_count = property True

prop_count' :: Property
prop_count' = property True

prop_endBy :: Property
prop_endBy = property True

prop_endBy1 :: Property
prop_endBy1 = property True

prop_manyTill :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                 Property
prop_manyTill a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (,) <$> manyTill letterChar (char 'c') <*> many letterChar
        r | c == 0    = posErr (a + b) s [uneEof, exCh 'c', exSpec "letter"]
          | otherwise = let (pre, post) = break (== 'c') s
                        in Right (pre, drop 1 post)
        s = abcRow a b c

prop_someTill :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                 Property
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

prop_sepBy :: Property
prop_sepBy = property True

prop_sepBy1 :: Property
prop_sepBy1 = property True

prop_sepEndBy :: Property
prop_sepEndBy = property True

prop_sepEndBy1 :: Property
prop_sepEndBy1 = property True

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
