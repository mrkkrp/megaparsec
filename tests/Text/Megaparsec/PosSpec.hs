--
-- Tests for Megaparsec's textual source positions.
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

{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Megaparsec.PosSpec (spec) where

import Data.Function (on)
import Data.List (isInfixOf)
import Data.Semigroup ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

spec :: Spec
spec = do

  describe "mkPos" $ do
    context "when the argument is 0" $
      it "throws InvalidPosException" $
        mkPos (0 :: Word) `shouldThrow` (== InvalidPosException)
    context "when the argument is not 0" $
      it "returns Pos with the given value" $
        property $ \n ->
          (n > 0) ==> (mkPos n >>= shouldBe n . unPos)

  describe "unsafePos" $
    context "when the argument is a positive integer" $
      it "returns Pos with the given value" $
        property $ \n ->
          (n > 0) ==> (unPos (unsafePos n) === n)

  describe "Read and Show instances of Pos" $
    it "printed representation of Pos is isomorphic to its value" $
      property $ \x ->
        read (show x) === (x :: Pos)

  describe "Ord instance of Pos" $
    it "works just like Ord instance of underlying Word" $
      property $ \x y ->
        compare x y === (compare `on` unPos) x y

  describe "Semigroup instance of Pos" $
    it "works like addition" $
      property $ \x y ->
        x <> y === unsafePos (unPos x + unPos y) .&&.
        unPos (x <> y) === unPos x + unPos y

  describe "initialPos" $
    it "consturcts initial position correctly" $
      property $ \path ->
        let x = initialPos path
        in sourceName   x === path        .&&.
           sourceLine   x === unsafePos 1 .&&.
           sourceColumn x === unsafePos 1

  describe "Read and Show instances of SourcePos" $
    it "printed representation of SourcePos in isomorphic to its value" $
      property $ \x ->
        read (show x) === (x :: SourcePos)

  describe "sourcePosPretty" $ do
    it "displays file name" $
      property $ \x ->
        sourceName x `isInfixOf` sourcePosPretty x
    it "displays line number" $
      property $ \x ->
        (show . unPos . sourceLine) x `isInfixOf` sourcePosPretty x
    it "displays column number" $
      property $ \x ->
        (show . unPos . sourceColumn) x `isInfixOf` sourcePosPretty x

  describe "defaultUpdatePos" $ do
    it "returns actual position unchanged" $
      property $ \w pos ch ->
        fst (defaultUpdatePos w pos ch) === pos
    it "does not change file name" $
      property $ \w pos ch ->
        (sourceName . snd) (defaultUpdatePos w pos ch) === sourceName pos
    context "when given newline character" $
      it "increments line number" $
        property $ \w pos ->
          (sourceLine . snd) (defaultUpdatePos w pos '\n')
            === (sourceLine pos <> pos1)
    context "when given tab character" $
      it "shits column number to next tab position" $
        property $ \w pos ->
          let c  = sourceColumn pos
              c' = (sourceColumn . snd) (defaultUpdatePos w pos '\t')
          in c' > c .&&. (((unPos c' - 1) `rem` unPos w) == 0)
    context "when given character other than newline or tab" $
      it "increments column number by one" $
        property $ \w pos ch ->
          (ch /= '\n' && ch /= '\t') ==>
          (sourceColumn . snd) (defaultUpdatePos w pos ch)
            === (sourceColumn pos <> pos1)
