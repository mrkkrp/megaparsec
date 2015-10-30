-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's permutation phrases parsers.
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

module Perm (tests) where

import Control.Applicative
import Data.List (nub, elemIndices)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Perm

import Util

tests :: Test
tests = testGroup "Permutation phrases parsers"
        [ testProperty "permutation parser pure" prop_pure
        , testProperty "permutation test 0"      prop_perm_0 ]

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
