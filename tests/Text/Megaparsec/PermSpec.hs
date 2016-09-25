--
-- Tests for Megaparsec's permutation phrases parsers.
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

{-# LANGUAGE MultiWayIf #-}

module Text.Megaparsec.PermSpec (spec) where

import Control.Applicative
import Data.List (nub, elemIndices)
import Data.Monoid
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec.Char
import Text.Megaparsec.Lexer (integer)
import Text.Megaparsec.Perm

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

spec :: Spec
spec = do

  describe "(<$$>)" $ do
    context "when supplied parser succeeds" $
      it "returns value returned by the parser" $
        property $ \n -> do
          let p = makePermParser (succ <$$> pure (n :: Integer))
          prs p "" `shouldParse` succ n
    context "when supplied parser fails" $
      it "signals correct parse error" $ do
          let p = makePermParser (succ <$$> integer)
          prs p "" `shouldFailWith` err posI (ueof <> elabel "integer")

  describe "(<$?>)" $ do
    context "when supplied parser succeeds" $
      it "returns value returned by the parser" $
        property $ \n m -> do
          let p = makePermParser (succ <$?> (n :: Integer, pure (m :: Integer)))
          prs p "" `shouldParse` succ m
    context "when supplied parser fails" $
      it "returns the default value" $
        property $ \n -> do
          let p = makePermParser (succ <$?> (n :: Integer, fail "foo"))
          prs p "" `shouldParse` succ n
    context "when stream in empty" $
      it "returns the default value" $
        property $ \n -> do
          let p = makePermParser (succ <$?> (n :: Integer, integer))
          prs p "" `shouldParse` succ n

  describe "makeExprParser" $
    it "works" $
      property $ \a' c' v -> do
        let (a,b,c) = getChars v
            p = makePermParser
              ((,,) <$?> (a' :: String, some (char a))
                <||> char b
                <|?> (c', char c))
            bis  = elemIndices b s
            preb = take (bis !! 1) s
            cis  = elemIndices c s
            prec = take (cis !! 1) s
            s    = getInput v
        if | length bis > 1 && (length cis <= 1 || head bis < head cis) ->
               prs_ p s `shouldFailWith` err (posN (bis !! 1) s)
                 ( utok b <> eeof <>
                   (if a `elem` preb then mempty else etok a) <>
                   (if c `elem` preb then mempty else etok c) )
           | length cis > 1 ->
               prs_ p s `shouldFailWith` err (posN (cis !! 1) s)
                 ( utok c <>
                   (if a `elem` prec then mempty else etok a) <>
                   (if b `elem` prec then eeof   else etok b) )
           | b `notElem` s ->
               prs_ p s `shouldFailWith` err (posN (length s) s)
                 ( ueof <> etok b <>
                   (if a `notElem` s || last s == a then etok a else mempty) <>
                   (if c `elem` s then mempty else etok c) )
           | otherwise ->
               prs_ p s `shouldParse`
                 ( if a `elem` s then filter (== a) s else a'
                 , b
                 , if c `elem` s then c else c' )
