--
-- QuickCheck tests for Megaparsec's permutation phrases parsers.
--
-- Copyright © 2015–2016 Megaparsec contributors
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

module Perm (tests) where

import Control.Applicative
import Data.List (nub, elemIndices)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.Perm

import Util

tests :: Test
tests = testGroup "Permutation phrases parsers"
  [ testProperty "permutation parser pure" prop_pure
  , testProperty "permutation test 0"      prop_perm_0
  , testProperty "combinator (<$$>)"       prop_ddcomb ]

data CharRows = CharRows
  { getChars :: (Char, Char, Char)
  , getInput :: String }
  deriving (Eq, Show)

instance Arbitrary CharRows where
  arbitrary = do
    chars@(a,b,c) <- arbitrary `suchThat` different
    an            <- arbitrary
    bn            <- arbitrary
    cn            <- arbitrary
    input <- concat <$> shuffle
             [ replicate an a
             , replicate bn b
             , replicate cn c]
    return $ CharRows chars input
      where different (a,b,c) = let l = [a,b,c] in l == nub l

prop_pure :: Integer -> Property
prop_pure n = makePermParser p /=\ n
  where p = id <$?> (succ n, pure n)

prop_perm_0 :: String -> Char -> CharRows -> Property
prop_perm_0 a' c' v = checkParser (makePermParser p) r s
  where (a,b,c) = getChars v
        p = (,,) <$?> (a', some (char a))
                 <||> char b
                 <|?> (c', char c)
        r | length bis > 1 && (length cis <= 1 || head bis < head cis) =
              posErr (bis !! 1) s $ [uneCh b, exEof] ++
              [exCh a | a `notElem` preb] ++
              [exCh c | c `notElem` preb]
          | length cis > 1 =
            posErr (cis !! 1) s $ [uneCh c] ++
            [exCh a | a `notElem` prec] ++
            [if b `elem` prec then exEof else exCh b]
          | b `notElem` s = posErr (length s) s $ [uneEof, exCh b] ++
                            [exCh a | a `notElem` s || last s == a] ++
                            [exCh c | c `notElem` s]
          | otherwise = Right ( if a `elem` s then filter (== a) s else a'
                              , b
                              , if c `elem` s then c else c' )
        bis  = elemIndices b s
        preb = take (bis !! 1) s
        cis  = elemIndices c s
        prec = take (cis !! 1) s
        s    = getInput v

prop_ddcomb :: NonNegative Integer -> Property
prop_ddcomb n' = checkParser (makePermParser p) r s
  where p = succ <$$> integer
        r = Right (succ n)
        n = getNonNegative n'
        s = show n
