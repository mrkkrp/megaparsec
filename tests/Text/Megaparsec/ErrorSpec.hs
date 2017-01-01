--
-- Tests for Megaparsec's parse errors.
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

module Text.Megaparsec.ErrorSpec (spec) where

import Data.Char (isControl, isSpace)
import Data.Function (on)
import Data.List (isInfixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Set (Set)
import Test.Hspec
import Test.QuickCheck
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup     as S
import qualified Data.Set           as E

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, all)
import Prelude hiding (all)
#else
import Control.Exception (Exception (..))
#endif

type PE = ParseError Char Dec

spec :: Spec
spec = do

  describe "Semigroup instance of ParseError" $
    it "associativity" $
      property $ \x y z ->
        (x S.<> y) S.<> z === (x S.<> (y S.<> z) :: PE)

  describe "Monoid instance of ParseError" $ do
    it "left identity" $
      property $ \x ->
        mempty <> x === (x :: PE)
    it "right identity" $
      property $ \x ->
        x <> mempty === (x :: PE)
    it "associativity" $
      property $ \x y z ->
        (x <> y) <> z === (x <> (y <> z) :: PE)

  describe "Read and Show instances of ParseError" $
    it "printed representation of ParseError can be read back" $
      property $ \x ->
        read (show x) === (x :: PE)

  describe "error merging with (<>)" $ do
    it "selects greater source position" $
      property $ \x y ->
        errorPos (x <> y :: PE) === max (errorPos x) (errorPos y)
    it "merges unexpected items correctly" $
      property (checkMergedItems errorUnexpected)
    it "merges expected items correctly" $
      property (checkMergedItems errorExpected)
    it "merges custom items correctly" $
      property (checkMergedItems errorCustom)

  describe "showTokens (Char instance)" $ do
    let f x y = showTokens (NE.fromList x) `shouldBe` y
    it "shows CRLF newline correctly"
      (f "\r\n" "crlf newline")
    it "shows null byte correctly"
      (f "\NUL" "null (control character)")
    it "shows start of heading correctly"
      (f "\SOH" "start of heading (control character)")
    it "shows start of text correctly"
      (f "\STX" "start of text (control character)")
    it "shows end of text correctly"
      (f "\ETX" "end of text (control character)")
    it "shows end of transmission correctly"
      (f "\EOT" "end of transmission (control character)")
    it "shows enquiry correctly"
      (f "\ENQ" "enquiry (control character)")
    it "shows acknowledge correctly"
      (f "\ACK" "acknowledge (control character)")
    it "shows bell correctly"
      (f "\BEL" "bell (control character)")
    it "shows backspace correctly"
      (f "\BS" "backspace")
    it "shows tab correctly"
      (f "\t" "tab")
    it "shows newline correctly"
      (f "\n" "newline")
    it "shows vertical tab correctly"
      (f "\v" "vertical tab")
    it "shows form feed correctly"
      (f "\f" "form feed (control character)")
    it "shows carriage return correctly"
      (f "\r" "carriage return")
    it "shows shift out correctly"
      (f "\SO" "shift out (control character)")
    it "shows shift in correctly"
      (f "\SI" "shift in (control character)")
    it "shows data link escape correctly"
      (f "\DLE" "data link escape (control character)")
    it "shows device control one correctly"
      (f "\DC1" "device control one (control character)")
    it "shows device control two correctly"
      (f "\DC2" "device control two (control character)")
    it "shows device control three correctly"
      (f "\DC3" "device control three (control character)")
    it "shows device control four correctly"
      (f "\DC4" "device control four (control character)")
    it "shows negative acknowledge correctly"
      (f "\NAK" "negative acknowledge (control character)")
    it "shows synchronous idle correctly"
      (f "\SYN" "synchronous idle (control character)")
    it "shows end of transmission block correctly"
      (f "\ETB" "end of transmission block (control character)")
    it "shows cancel correctly"
      (f "\CAN" "cancel (control character)")
    it "shows end of medium correctly"
      (f "\EM"  "end of medium (control character)")
    it "shows substitute correctly"
      (f "\SUB" "substitute (control character)")
    it "shows escape correctly"
      (f "\ESC" "escape (control character)")
    it "shows file separator correctly"
      (f "\FS"  "file separator (control character)")
    it "shows group separator correctly"
      (f "\GS"  "group separator (control character)")
    it "shows record separator correctly"
      (f "\RS"  "record separator (control character)")
    it "shows unit separator correctly"
      (f "\US"  "unit separator (control character)")
    it "shows delete correctly"
      (f "\DEL" "delete (control character)")
    it "shows space correctly"
      (f " "    "space")
    it "shows non-breaking space correctly"
      (f "\160" "non-breaking space")
    it "shows other single characters in single quotes" $
      property $ \ch ->
        not (isControl ch) && not (isSpace ch) ==>
          showTokens (ch :| []) === ['\'',ch,'\'']
    it "shows strings in double quotes" $
      property $ \str ->
        (length str > 1) && (str /= "\r\n") ==>
          showTokens (NE.fromList str) === ("\"" ++ str ++"\"")

  describe "parseErrorPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorPretty (mempty :: PE) `shouldBe` "1:1:\nunknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorPretty (x :: PE) `shouldSatisfy` ("\n" `isSuffixOf`)
    it "result contains representation of source pos stack" $
      property (contains errorPos sourcePosPretty)
    it "result contains representation of unexpected items" $
      property (contains errorUnexpected showErrorComponent)
    it "result contains representation of expected items" $
      property (contains errorExpected showErrorComponent)
    it "result contains representation of custom items" $
      property (contains errorCustom showErrorComponent)

  describe "sourcePosStackPretty" $
    it "result never ends with a newline " $
      property $ \x ->
        let pos = errorPos (x :: PE)
        in sourcePosStackPretty pos `shouldNotSatisfy` ("\n" `isSuffixOf`)

  describe "parseErrorTextPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorTextPretty (mempty :: PE) `shouldBe` "unknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorTextPretty (x :: PE) `shouldSatisfy` ("\n" `isSuffixOf`)

#if MIN_VERSION_base(4,8,0)
  describe "displayException" $
    it "produces the same result as parseErrorPretty" $
      property $ \x ->
        displayException x `shouldBe` parseErrorPretty (x :: PE)
#endif

----------------------------------------------------------------------------
-- Helpers

checkMergedItems :: (Ord a, Show a) => (PE -> Set a) -> PE -> PE -> Property
checkMergedItems f e1 e2 = f (e1 <> e2) === r
  where r = case (compare `on` errorPos) e1 e2 of
              LT -> f e2
              EQ -> (E.union `on` f) e1 e2
              GT -> f e1

contains :: Foldable t => (PE -> t a) -> (a -> String) -> PE -> Property
contains g r e = property (all f (g e))
  where rendered = parseErrorPretty e
        f x = r x `isInfixOf` rendered
