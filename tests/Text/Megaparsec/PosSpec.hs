{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Megaparsec.PosSpec (spec) where

import Control.Exception (evaluate)
import Data.Function (on)
import Data.Semigroup ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream (defaultUpdatePos)
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB

#if !MIN_VERSION_base(4,8,0)
import Data.Word (Word)
#endif

spec :: Spec
spec = do

  describe "mkPos" $ do
    context "when the argument is 0" $
      it "throws InvalidPosException" $
        evaluate (mkPos 0) `shouldThrow` (== InvalidPosException)
    context "when the argument is not 0" $
      it "returns Pos with the given value" $
        property $ \n ->
          (n > 0) ==> (unPos (mkPos n) `shouldBe` n)

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
        x <> y === mkPos (unPos x + unPos y) .&&.
        unPos (x <> y) === unPos x + unPos y

  describe "initialPos" $
    it "constructs initial position correctly" $
      property $ \path ->
        let x = initialPos path
        in sourceName   x === path    .&&.
           sourceLine   x === mkPos 1 .&&.
           sourceColumn x === mkPos 1

  describe "Read and Show instances of SourcePos" $
    it "printed representation of SourcePos in isomorphic to its value" $
      property $ \x ->
        read (show x) === (x :: SourcePos)

  describe "sourcePosPretty" $ do
    it "displays file name" $
      property $ \x ->
        TL.pack (sourceName x) `TL.isInfixOf`
          TB.toLazyText (sourcePosPretty x)
    it "displays line number" $
      property $ \x ->
        (TB.toLazyText . TB.decimal . unPos . sourceLine) x `TL.isInfixOf`
          TB.toLazyText (sourcePosPretty x)
    it "displays column number" $
      property $ \x ->
        (TB.toLazyText . TB.decimal . unPos . sourceColumn) x `TL.isInfixOf`
          TB.toLazyText (sourcePosPretty x)

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
