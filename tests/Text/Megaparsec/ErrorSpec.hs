{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Text.Megaparsec.ErrorSpec (spec) where

import Data.Char (isControl, isSpace)
import Data.Function (on)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid
import Data.Set (Set)
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc ()
import Test.QuickCheck
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import qualified Data.List.NonEmpty     as NE
import qualified Data.Semigroup         as S
import qualified Data.Set               as E
import qualified Data.Text              as T
import qualified Data.Text.Lazy         as TL
import qualified Data.Text.Lazy.Builder as TB

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, all)
import Prelude hiding (all)
#else
import Control.Exception (Exception (..))
#endif

type PE = ParseError Char Void

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
      property (checkMergedItems errorFancy)

  describe "showTokens (Char instance)" $ do
    let f x y = showTokens (NE.fromList x) `shouldBe` y
    it "shows CRLF newline correctly"
      (f "\r\n" "crlf newline")
    it "shows null byte correctly"
      (f "\NUL" "null")
    it "shows start of heading correctly"
      (f "\SOH" "start of heading")
    it "shows start of text correctly"
      (f "\STX" "start of text")
    it "shows end of text correctly"
      (f "\ETX" "end of text")
    it "shows end of transmission correctly"
      (f "\EOT" "end of transmission")
    it "shows enquiry correctly"
      (f "\ENQ" "enquiry")
    it "shows acknowledge correctly"
      (f "\ACK" "acknowledge")
    it "shows bell correctly"
      (f "\BEL" "bell")
    it "shows backspace correctly"
      (f "\BS" "backspace")
    it "shows tab correctly"
      (f "\t" "tab")
    it "shows newline correctly"
      (f "\n" "newline")
    it "shows vertical tab correctly"
      (f "\v" "vertical tab")
    it "shows form feed correctly"
      (f "\f" "form feed")
    it "shows carriage return correctly"
      (f "\r" "carriage return")
    it "shows shift out correctly"
      (f "\SO" "shift out")
    it "shows shift in correctly"
      (f "\SI" "shift in")
    it "shows data link escape correctly"
      (f "\DLE" "data link escape")
    it "shows device control one correctly"
      (f "\DC1" "device control one")
    it "shows device control two correctly"
      (f "\DC2" "device control two")
    it "shows device control three correctly"
      (f "\DC3" "device control three")
    it "shows device control four correctly"
      (f "\DC4" "device control four")
    it "shows negative acknowledge correctly"
      (f "\NAK" "negative acknowledge")
    it "shows synchronous idle correctly"
      (f "\SYN" "synchronous idle")
    it "shows end of transmission block correctly"
      (f "\ETB" "end of transmission block")
    it "shows cancel correctly"
      (f "\CAN" "cancel")
    it "shows end of medium correctly"
      (f "\EM"  "end of medium")
    it "shows substitute correctly"
      (f "\SUB" "substitute")
    it "shows escape correctly"
      (f "\ESC" "escape")
    it "shows file separator correctly"
      (f "\FS"  "file separator")
    it "shows group separator correctly"
      (f "\GS"  "group separator")
    it "shows record separator correctly"
      (f "\RS"  "record separator")
    it "shows unit separator correctly"
      (f "\US"  "unit separator")
    it "shows delete correctly"
      (f "\DEL" "delete")
    it "shows space correctly"
      (f " "    "space")
    it "shows non-breaking space correctly"
      (f "\160" "non-breaking space")
    it "shows other single characters in single quotes" $
      property $ \ch ->
        not (isControl ch) && not (isSpace ch) ==>
          showTokens (ch :| []) === "\'" <> TB.singleton ch <> "\'"
    it "shows strings in double quotes" $
      property $ \str' ->
        let str = filter (not . g) str'
            g x = isControl x || x `elem` ['\160']
            toB = TB.fromText . T.pack
        in length str > 1 ==>
             showTokens (NE.fromList str) === ("\"" <> toB str <> "\"")
    it "shows control characters in long strings property"
      (f "{\n" "\"{<newline>\"")

  describe "parseErrorPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorPretty (mempty :: PE) `shouldBe` "1:1:\nunknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorPretty (x :: PE) `shouldSatisfy` ("\n" `T.isSuffixOf`)
    it "result contains representation of source pos stack" $
      property (contains errorPos sourcePosPretty)
    it "result contains representation of unexpected items" $
      property (contains errorUnexpected showErrorComponent)
    it "result contains representation of expected items" $
      property (contains errorExpected showErrorComponent)
    it "result contains representation of custom items" $
      property (contains errorFancy showErrorComponent)

  describe "sourcePosStackPretty" $
    it "result never ends with a newline " $
      property $ \x ->
        let pos = errorPos (x :: PE)
        in TB.toLazyText (sourcePosStackPretty pos)
           `shouldNotSatisfy` ("\n" `TL.isSuffixOf`)

  describe "parseErrorTextPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorTextPretty (mempty :: PE) `shouldBe` "unknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        TB.toLazyText (parseErrorTextPretty (x :: PE))
          `shouldSatisfy` ("\n" `TL.isSuffixOf`)

#if MIN_VERSION_base(4,8,0)
  describe "displayException" $
    it "produces the same result as parseErrorPretty" $
      property $ \x ->
        displayException x `shouldBe` T.unpack (parseErrorPretty (x :: PE))
#endif

----------------------------------------------------------------------------
-- Helpers

checkMergedItems :: (Ord a, Show a) => (PE -> Set a) -> PE -> PE -> Property
checkMergedItems f e1 e2 = f (e1 <> e2) === r
  where
    r = case (compare `on` errorPos) e1 e2 of
          LT -> f e2
          EQ -> (E.union `on` f) e1 e2
          GT -> f e1

contains :: Foldable t => (PE -> t a) -> (a -> TB.Builder) -> PE -> Property
contains g r e = property (all f (g e))
  where
    rendered = parseErrorPretty e
    f x = (TL.toStrict . TB.toLazyText . r) x `T.isInfixOf` rendered
