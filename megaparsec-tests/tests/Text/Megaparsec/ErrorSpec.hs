{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.ErrorSpec (spec) where

import Control.Exception (Exception (..))
import Data.Functor.Identity
import Data.List (isInfixOf, isSuffixOf, sort)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as E
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc ()
import Test.QuickCheck
import Text.Megaparsec

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

spec :: Spec
spec = do
  describe "Semigroup instance of ParseError" $
    it "associativity" $
      property $ \x y z ->
        (x <> y) <> z === (x <> (y <> z) :: PE)

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

  describe "error merging with (<>)" $ do
    it "selects greater offset" $
      property $ \x y ->
        errorOffset (x <> y :: PE) === max (errorOffset x) (errorOffset y)
    context "when combining two trivial parse errors at the same position" $
      it "merges their unexpected and expected items" $
        do
          let n Nothing Nothing = Nothing
              n (Just x) Nothing = Just x
              n Nothing (Just y) = Just y
              n (Just x) (Just y) = Just (max x y)
          property $ \pos us0 ps0 us1 ps1 ->
            TrivialError pos us0 ps0 <> TrivialError pos us1 ps1
              `shouldBe` (TrivialError pos (n us0 us1) (E.union ps0 ps1) :: PE)
    context "when combining two fancy parse errors at the same position" $
      it "merges their custom items" $
        property $ \pos xs0 xs1 ->
          FancyError pos xs0 <> FancyError pos xs1
            `shouldBe` (FancyError pos (E.union xs0 xs1) :: PE)
    context "when combining trivial error with fancy error" $ do
      it "fancy has precedence (left)" $
        property $ \pos us ps xs ->
          FancyError pos xs <> TrivialError pos us ps
            `shouldBe` (FancyError pos xs :: PE)
      it "fancy has precedence (right)" $
        property $ \pos us ps xs ->
          TrivialError pos us ps <> FancyError pos xs
            `shouldBe` (FancyError pos xs :: PE)

  -- NOTE 'errorOffset' and 'setErrorOffset' are trivial.

  describe "attachSourcePos" $
    it "attaches the positions correctly" $
      property $ \xs' s -> do
        let xs = sort $ getSmall . getPositive <$> xs'
            pst = initialPosState (s :: String)
            pst' =
              if null xs
                then pst
                else reachOffsetNoLine (last xs) pst
            rs = f <$> xs
            f x = (x, pstateSourcePos (reachOffsetNoLine x pst))
        attachSourcePos id (xs :: [Int]) pst `shouldBe` (rs, pst')

  describe "errorBundlePretty" $ do
    it "shows empty line correctly" $ do
      let s = "" :: String
      mkBundlePE s (mempty :: PE)
        `shouldBe` "1:1:\n  |\n1 | <empty line>\n  | ^\nunknown parse error\n"
    it "shows position on first line correctly" $ do
      let s = "abc" :: String
          pe = err 1 (utok 'b' <> etok 'd') :: PE
      mkBundlePE s pe
        `shouldBe` "1:2:\n  |\n1 | abc\n  |  ^\nunexpected 'b'\nexpecting 'd'\n"
    it "skips to second line correctly" $ do
      let s = "one\ntwo\n" :: String
          pe = err 4 (utok 't' <> etok 'x') :: PE
      mkBundlePE s pe
        `shouldBe` "2:1:\n  |\n2 | two\n  | ^\nunexpected 't'\nexpecting 'x'\n"
    it "shows position on 1000 line correctly" $ do
      let s = replicate 999 '\n' ++ "abc"
          pe = err 999 (utok 'a' <> etok 'd') :: PE
      mkBundlePE s pe
        `shouldBe` "1000:1:\n     |\n1000 | abc\n     | ^\nunexpected 'a'\nexpecting 'd'\n"
    it "shows offending line in the presence of tabs correctly" $ do
      let s = "\tsomething" :: String
          pe = err 1 (utok 's' <> etok 'x') :: PE
      mkBundlePE s pe
        `shouldBe` "1:9:\n  |\n1 |         something\n  |         ^\nunexpected 's'\nexpecting 'x'\n"
    it "uses continuous highlighting properly (trivial)" $ do
      let s = "\tfoobar" :: String
          pe = err 1 (utoks "foo" <> utoks "rar") :: PE
      mkBundlePE s pe
        `shouldBe` "1:9:\n  |\n1 |         foobar\n  |         ^^^\nunexpected \"rar\"\n"
    it "uses continuous highlighting properly (fancy)" $ do
      let s = "\tfoobar" :: String
          pe =
            errFancy
              1
              (fancy $ ErrorCustom (CustomErr 5)) ::
              ParseError String CustomErr
      mkBundlePE s pe
        `shouldBe` "1:9:\n  |\n1 |         foobar\n  |         ^^^^^\ncustom thing\n"
    it "adjusts continuous highlighting so it doesn't get too long" $ do
      let s = "foobar\n" :: String
          pe = err 4 (utoks "foobar" <> etoks "foobar") :: PE
      mkBundlePE s pe
        `shouldBe` "1:5:\n  |\n1 | foobar\n  |     ^^^\nunexpected \"foobar\"\nexpecting \"foobar\"\n"
    context "stream of insufficient size is provided in the bundle" $
      it "handles the situation reasonably" $
        do
          let s = "" :: String
              pe = err 3 (ueof <> etok 'x') :: PE
          mkBundlePE s pe
            `shouldBe` "1:1:\n  |\n1 | <empty line>\n  | ^\nunexpected end of input\nexpecting 'x'\n"
    context "starting column in bundle is greater than 1" $ do
      context "and less than parse error column" $
        it "is rendered correctly" $
          do
            let s = "foo" :: String
                pe = err 5 (utok 'o' <> etok 'x') :: PE
                bundle =
                  ParseErrorBundle
                    { bundleErrors = pe :| [],
                      bundlePosState =
                        PosState
                          { pstateInput = s,
                            pstateOffset = 4,
                            pstateSourcePos = SourcePos "" pos1 (mkPos 5),
                            pstateTabWidth = defaultTabWidth,
                            pstateLinePrefix = ""
                          }
                    }
            errorBundlePretty bundle
              `shouldBe` "1:6:\n  |\n1 | foo\n  | \nunexpected 'o'\nexpecting 'x'\n"
      context "and greater than parse error column" $
        it "is rendered correctly" $
          do
            let s = "foo" :: String
                pe = err 5 (utok 'o' <> etok 'x') :: PE
                bundle =
                  ParseErrorBundle
                    { bundleErrors = pe :| [],
                      bundlePosState =
                        PosState
                          { pstateInput = s,
                            pstateOffset = 9,
                            pstateSourcePos = SourcePos "" pos1 (mkPos 10),
                            pstateTabWidth = defaultTabWidth,
                            pstateLinePrefix = ""
                          }
                    }
            errorBundlePretty bundle
              `shouldBe` "1:10:\n  |\n1 | foo\n  | \nunexpected 'o'\nexpecting 'x'\n"
    it "takes tab width into account correctly" $
      property $ \w' -> do
        let s = "\tsomething\t" :: String
            pe = err 1 (utok 's' <> etok 'x') :: PE
            bundle =
              ParseErrorBundle
                { bundleErrors = pe :| [],
                  bundlePosState =
                    PosState
                      { pstateInput = s,
                        pstateOffset = 0,
                        pstateSourcePos = initialPos "",
                        pstateTabWidth = w',
                        pstateLinePrefix = ""
                      }
                }
            w = unPos w'
            tabRep = replicate w ' '
        errorBundlePretty bundle
          `shouldBe` ( "1:" ++ show (w + 1) ++ ":\n  |\n1 | " ++ tabRep
                         ++ "something"
                         ++ tabRep
                         ++ "\n  | "
                         ++ tabRep
                         ++ "^\nunexpected 's'\nexpecting 'x'\n"
                     )
    it "displays multi-error bundle correctly" $ do
      let s = "something\ngood\n" :: String
          pe0 = err 2 (utok 'm' <> etok 'x') :: PE
          pe1 = err 10 (utok 'g' <> etok 'y') :: PE
          bundle =
            ParseErrorBundle
              { bundleErrors = pe0 :| [pe1],
                bundlePosState =
                  PosState
                    { pstateInput = s,
                      pstateOffset = 0,
                      pstateSourcePos = initialPos "",
                      pstateTabWidth = defaultTabWidth,
                      pstateLinePrefix = ""
                    }
              }
      errorBundlePretty bundle
        `shouldBe` "1:3:\n  |\n1 | something\n  |   ^\nunexpected 'm'\nexpecting 'x'\n\n2:1:\n  |\n2 | good\n  | ^\nunexpected 'g'\nexpecting 'y'\n"

  describe "parseErrorPretty" $ do
    it "shows unknown ParseError correctly" $
      parseErrorPretty (mempty :: PE) `shouldBe` "offset=0:\nunknown parse error\n"
    it "result always ends with a newline" $
      property $ \x ->
        parseErrorPretty (x :: PE) `shouldSatisfy` ("\n" `isSuffixOf`)
    it "result contains representation of offset" $
      property (contains (Identity . errorOffset) show)
    it "result contains unexpected/expected items" $ do
      let e = err 0 (utoks "foo" <> etoks "bar" <> etoks "baz") :: PE
      parseErrorPretty e `shouldBe` "offset=0:\nunexpected \"foo\"\nexpecting \"bar\" or \"baz\"\n"
    it "result contains representation of custom items" $ do
      let e = errFancy 0 (fancy (ErrorFail "Ooops!")) :: PE
      parseErrorPretty e `shouldBe` "offset=0:\nOoops!\n"
    it "several fancy errors look not so bad" $ do
      let pe :: PE
          pe =
            errFancy 0 $
              mempty <> fancy (ErrorFail "foo") <> fancy (ErrorFail "bar")
      parseErrorPretty pe `shouldBe` "offset=0:\nbar\nfoo\n"

  describe "parseErrorTextPretty" $ do
    it "shows trivial unknown ParseError correctly" $
      parseErrorTextPretty (mempty :: PE)
        `shouldBe` "unknown parse error\n"
    it "shows fancy unknown ParseError correctly" $
      parseErrorTextPretty (FancyError 0 E.empty :: PE)
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

-- | Custom error component to test continuous highlighting for custom
-- components.
newtype CustomErr = CustomErr Int
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomErr where
  showErrorComponent _ = "custom thing"
  errorComponentLen (CustomErr n) = n

type PE = ParseError String Void

contains :: Foldable t => (PE -> t a) -> (a -> String) -> PE -> Property
contains g r e = property (all f (g e))
  where
    rendered = parseErrorPretty e
    f x = r x `isInfixOf` rendered

mkBundlePE ::
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  s ->
  ParseError s e ->
  String
mkBundlePE s e =
  errorBundlePretty $
    ParseErrorBundle
      { bundleErrors = e :| [],
        bundlePosState =
          PosState
            { pstateInput = s,
              pstateOffset = 0,
              pstateSourcePos = initialPos "",
              pstateTabWidth = defaultTabWidth,
              pstateLinePrefix = ""
            }
      }
