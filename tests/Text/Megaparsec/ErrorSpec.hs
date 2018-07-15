{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.ErrorSpec (spec) where

import Control.Exception (Exception (..))
import Control.Monad.Identity
import Data.ByteString (ByteString)
import Data.Char (isControl, isSpace)
import Data.List (isInfixOf, isSuffixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Void
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc ()
import Test.QuickCheck
import Text.Megaparsec.Error
import Text.Megaparsec.Error.Builder
import Text.Megaparsec.Pos
import qualified Data.ByteString    as B
import qualified Data.List.NonEmpty as NE
import qualified Data.Semigroup     as S
import qualified Data.Set           as E

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

type PE = ParseError Char Void
type PW = ParseError Word8 Void

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
    context "when combining two trivial parse errors at the same position" $
      it "merges their unexpected and expected items" $ do
        let n Nothing  Nothing = Nothing
            n (Just x) Nothing = Just x
            n Nothing (Just y) = Just y
            n (Just x) (Just y) = Just (max x y)
        property $ \pos us0 ps0 us1 ps1 ->
          TrivialError pos us0 ps0 <> TrivialError pos us1 ps1 `shouldBe`
            (TrivialError pos (n us0 us1) (E.union ps0 ps1) :: PE)
    context "when combining two fancy parse errors at the same position" $
      it "merges their custom items" $
        property $ \pos xs0 xs1 ->
          FancyError pos xs0 <> FancyError pos xs1 `shouldBe`
            (FancyError pos (E.union xs0 xs1) :: PE)
    context "when combining trivial error with fancy error" $ do
      it "fancy has precedence (left)" $
        property $ \pos us ps xs ->
          FancyError pos xs <> TrivialError pos us ps `shouldBe`
            (FancyError pos xs :: PE)
      it "fancy has precedence (right)" $
        property $ \pos us ps xs ->
          TrivialError pos us ps <> FancyError pos xs `shouldBe`
            (FancyError pos xs :: PE)

  describe "errorPos" $
    it "returns error position" $
      property $ \e ->
        errorPos e `shouldBe`
          (case e :: PE of
            TrivialError pos _ _ -> pos
            FancyError   pos _   -> pos)

  describe "showTokens (Char instance)" $ do
    let f :: String -> String -> Expectation
        f x y = showTokens (NE.fromList x) `shouldBe` y
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
          showTokens (ch :| []) === "\'" <> [ch] <> "\'"
    it "shows strings in double quotes" $
      property $ \str' ->
        let str = filter (not . g) str'
            g x = isControl x || x == '\160'
        in length str > 1 ==>
             showTokens (NE.fromList str) === ("\"" <> str <> "\"")
    it "shows control characters in long strings property"
      (f "{\n" "\"{<newline>\"")

  describe "showTokens (Word8 instance)" $
    it "basically works" $ do
      -- NOTE Currently the Word8 instance is defined via Char intance, so
      -- the testing is rather shallow.
      let ts = NE.fromList [10,48,49,50] :: NonEmpty Word8
      showTokens ts `shouldBe` "\"<newline>012\""

  describe "errorBundlePretty" $ do
    context "with Char tokens" $ do
      it "shows empty line correctly" $ do
        let s = "" :: String
        mkBundlePE s (mempty :: PE) `shouldBe`
          "1:1:\n  |\n1 | <empty line>\n  | ^\nunknown parse error\n"
      it "shows position on first line correctly" $ do
        let s = "abc" :: String
            pe = err (posN 1 s) (utok 'b' <> etok 'd') :: PE
        mkBundlePE s pe `shouldBe`
          "1:2:\n  |\n1 | abc\n  |  ^\nunexpected 'b'\nexpecting 'd'\n"
      it "skips to second line correctly" $ do
        let s = "one\ntwo\n" :: String
            pe = err (posN 4 s) (utok 't' <> etok 'x') :: PE
        mkBundlePE s pe `shouldBe`
          "2:1:\n  |\n2 | two\n  | ^\nunexpected 't'\nexpecting 'x'\n"
      it "shows position on 1000 line correctly" $ do
        let s = replicate 999 '\n' ++ "abc"
            pe = err (posN 999 s) (utok 'a' <> etok 'd') :: PE
        mkBundlePE s pe `shouldBe`
          "1000:1:\n     |\n1000 | abc\n     | ^\nunexpected 'a'\nexpecting 'd'\n"
      it "shows offending line in the presence of tabs correctly" $ do
        let s = "\tsomething" :: String
            pe = err (posN 1 s) (utok 's' <> etok 'x') :: PE
        mkBundlePE s pe `shouldBe`
          "1:9:\n  |\n1 |         something\n  |         ^\nunexpected 's'\nexpecting 'x'\n"
      it "uses continuous highlighting properly" $ do
        let s = "\tfoobar" :: String
            pe = err (posN 1 s) (utoks "foo" <> utoks "rar") :: PE
        mkBundlePE s pe `shouldBe`
          "1:9:\n  |\n1 |         foobar\n  |         ^^^\nunexpected \"rar\"\n"
      context "stream of insufficient size is provided in the bundle" $
        it "handles the situation reasonably" $ do
          let s = "" :: String
              s' = "foobar" :: String
              pe = err (posN 3 s') (ueof <> etok 'x') :: PE
          mkBundlePE s pe `shouldBe`
            "1:4:\n  |\n1 | <empty line>\n  |    ^\nunexpected end of input\nexpecting 'x'\n"
      context "starting column in bundle is greater than 1" $ do
        context "and less than parse error column" $
          it "is rendered correctly" $ do
            let s = "foo" :: String
                s' = "blahfoo" :: String
                pe = err (posN 5 s') (utok 'o' <> etok 'x') :: PE
                bundle = ParseErrorBundle
                  { bundleErrors = pe :| []
                  , bundleTabWidth = defaultTabWidth
                  , bundleInput = s
                  , bundleSourcePos =
                      SourcePos "" pos1 (S.stimes (5 :: Int) pos1)
                  }
            errorBundlePretty bundle `shouldBe`
              "1:6:\n  |\n1 | ????foo\n  |      ^\nunexpected 'o'\nexpecting 'x'\n"
        context "and greater than parse error column" $
          it "is rendered correctly" $ do
            let s = "foo" :: String
                s' = "blahfoo" :: String
                pe = err (posN 5 s') (utok 'o' <> etok 'x') :: PE
                bundle = ParseErrorBundle
                  { bundleErrors = pe :| []
                  , bundleTabWidth = defaultTabWidth
                  , bundleInput = s
                  , bundleSourcePos =
                      SourcePos "" pos1 (S.stimes (10 :: Int) pos1)
                  }
            errorBundlePretty bundle `shouldBe`
              "1:6:\n  |\n1 | ?????????foo\n  |      ^\nunexpected 'o'\nexpecting 'x'\n"
    context "with Word8 tokens" $ do
      it "shows empty line correctly" $ do
        let s = "" :: ByteString
        mkBundlePW s (mempty :: PW) `shouldBe`
          "1:1:\n  |\n1 | <empty line>\n  | ^\nunknown parse error\n"
      it "shows position on first line correctly" $ do
        let s = "abc" :: ByteString
            pe = err (posN 1 s) (utok 98 <> etok 100) :: PW
        mkBundlePW s pe `shouldBe`
          "1:2:\n  |\n1 | abc\n  |  ^\nunexpected 'b'\nexpecting 'd'\n"
      it "skips to second line correctly" $ do
        let s = "one\ntwo\n" :: ByteString
            pe = err (posN 4 s) (utok 116 <> etok 120) :: PW
        mkBundlePW s pe `shouldBe`
          "2:1:\n  |\n2 | two\n  | ^\nunexpected 't'\nexpecting 'x'\n"
      it "shows position on 1000 line correctly" $ do
        let s = B.replicate 999 10 <> "abc"
            pe = err (posN 999 s) (utok 97 <> etok 100) :: PW
        mkBundlePW s pe `shouldBe`
          "1000:1:\n     |\n1000 | abc\n     | ^\nunexpected 'a'\nexpecting 'd'\n"
      it "shows offending line in the presence of tabs correctly" $ do
        let s = "\tsomething" :: ByteString
            pe = err (posN 1 s) (utok 115 <> etok 120) :: PW
        mkBundlePW s pe `shouldBe`
          "1:9:\n  |\n1 |         something\n  |         ^\nunexpected 's'\nexpecting 'x'\n"
      it "uses continuous highlighting properly" $ do
        let s = "\tfoobar" :: ByteString
            pe = err (posN 1 s) (utoks (B.unpack "foo") <> utoks (B.unpack "rar")) :: PW
        mkBundlePW s pe `shouldBe`
          "1:9:\n  |\n1 |         foobar\n  |         ^^^\nunexpected \"rar\"\n"
      context "stream of insufficient size is provided in the bundle" $
        it "handles the situation reasonably" $ do
          let s = "" :: ByteString
              s' = "foobar" :: ByteString
              pe = err (posN 3 s') (ueof <> etok 120) :: PW
          mkBundlePW s pe `shouldBe`
            "1:4:\n  |\n1 | <empty line>\n  |    ^\nunexpected end of input\nexpecting 'x'\n"
      context "starting column in bundle is greater than 1" $ do
        context "and less than parse error column" $
          it "is rendered correctly" $ do
            let s = "foo" :: ByteString
                s' = "blahfoo" :: ByteString
                pe = err (posN 5 s') (utok 111 <> etok 120) :: PW
                bundle = ParseErrorBundle
                  { bundleErrors = pe :| []
                  , bundleTabWidth = defaultTabWidth
                  , bundleInput = s
                  , bundleSourcePos =
                      SourcePos "" pos1 (S.stimes (5 :: Int) pos1)
                  }
            errorBundlePretty bundle `shouldBe`
              "1:6:\n  |\n1 | ????foo\n  |      ^\nunexpected 'o'\nexpecting 'x'\n"
        context "and greater than parse error column" $
          it "is rendered correctly" $ do
            let s = "foo" :: ByteString
                s' = "blahfoo" :: ByteString
                pe = err (posN 5 s') (utok 111 <> etok 120) :: PW
                bundle = ParseErrorBundle
                  { bundleErrors = pe :| []
                  , bundleTabWidth = defaultTabWidth
                  , bundleInput = s
                  , bundleSourcePos =
                      SourcePos "" pos1 (S.stimes (10 :: Int) pos1)
                  }
            errorBundlePretty bundle `shouldBe`
              "1:6:\n  |\n1 | ?????????foo\n  |      ^\nunexpected 'o'\nexpecting 'x'\n"
    it "takes tab width into account correctly" $
      property $ \w' -> do
        let s  = "\tsomething\t" :: String
            pe = err (posN 1 s) (utok 's' <> etok 'x') :: PE
            bundle = ParseErrorBundle
              { bundleErrors = pe :| []
              , bundleTabWidth = w'
              , bundleInput = s
              , bundleSourcePos = initialPos ""
              }
            w  = unPos w'
        errorBundlePretty bundle `shouldBe`
          ("1:9:\n  |\n1 | " ++ replicate w ' ' ++ "something" ++ replicate w ' '
           ++ "\n  |         ^\nunexpected 's'\nexpecting 'x'\n")
    it "displays multi-error bundle correctly" $ do
      let s = "something\ngood\n" :: String
          pe0 = err (posN 0 s) (utok 's' <> etok 'x') :: PE
          pe1 = err (posN 10 s) (utok 'g' <> etok 'y') :: PE
          bundle = ParseErrorBundle
            { bundleErrors = pe0 :| [pe1]
            , bundleTabWidth = defaultTabWidth
            , bundleInput = s
            , bundleSourcePos = initialPos ""
            }
      errorBundlePretty bundle `shouldBe`
        "1:1:\n  |\n1 | something\n  | ^\nunexpected 's'\nexpecting 'x'\n\n2:1:\n  |\n2 | good\n  | ^\nunexpected 'g'\nexpecting 'y'\n"

  describe "parseErrorPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorPretty (mempty :: PE) `shouldBe` "1:1:\nunknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorPretty (x :: PE) `shouldSatisfy` ("\n" `isSuffixOf`)
    it "result contains representation of source pos stack" $
      property (contains (Identity . errorPos) sourcePosPretty)
    it "result contains representation of unexpected items" $ do
      let f (TrivialError _ us _) = us
          f _                     = Nothing
      property (contains f showErrorComponent)
    it "result contains representation of expected items" $ do
      let f (TrivialError _ _ ps) = ps
          f _                     = E.empty
      property (contains f showErrorComponent)
    it "result contains representation of custom items" $ do
      let f (FancyError _ xs) = xs
          f _                 = E.empty
      property (contains f showErrorComponent)
    it "several fancy errors look not so bad" $ do
      let pe :: PE
          pe = errFancy posI $
            mempty <> fancy (ErrorFail "foo") <> fancy (ErrorFail "bar")
      parseErrorPretty pe `shouldBe` "1:1:\nbar\nfoo\n"

  describe "parseErrorTextPretty" $ do
    it "shows trivial unknown ParseError correctly" $
      parseErrorTextPretty (mempty :: PE)
        `shouldBe` "unknown parse error\n"
    it "shows fancy unknown ParseError correctly" $
      parseErrorTextPretty (FancyError posI E.empty :: PE)
        `shouldBe` "unknown fancy parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorTextPretty (x :: PE)
          `shouldSatisfy` ("\n" `isSuffixOf`)

  describe "displayException" $
    it "produces the same result as parseErrorPretty" $
      property $ \x ->
        displayException x `shouldBe` parseErrorPretty (x :: PE)

----------------------------------------------------------------------------
-- Helpers

contains :: Foldable t => (PE -> t a) -> (a -> String) -> PE -> Property
contains g r e = property (all f (g e))
  where
    rendered = parseErrorPretty e
    f x = r x `isInfixOf` rendered

mkBundlePE :: String -> PE -> String
mkBundlePE s e = errorBundlePretty $ ParseErrorBundle
  { bundleErrors = e :| []
  , bundleTabWidth = defaultTabWidth
  , bundleInput = s
  , bundleSourcePos = initialPos ""
  }

mkBundlePW :: ByteString -> PW -> String
mkBundlePW s e = errorBundlePretty $ ParseErrorBundle
  { bundleErrors = e :| []
  , bundleTabWidth = defaultTabWidth
  , bundleInput = s
  , bundleSourcePos = initialPos ""
  }
