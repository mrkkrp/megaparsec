{-# LANGUAGE CPP #-}

module Text.Megaparsec.PosSpec (spec) where

import Control.Exception (evaluate)
import Data.Function (on)
import Data.List (isInfixOf)
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc ()
import Test.QuickCheck
import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

spec :: Spec
spec = do

  describe "mkPos" $ do
    context "when the argument is a non-positive number" $
      it "throws InvalidPosException" $
        property $ \n -> n <= 0 ==>
          evaluate (mkPos n) `shouldThrow` (== InvalidPosException n)
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
        sourceName x `isInfixOf` sourcePosPretty x
    it "displays line number" $
      property $ \x ->
        (show . unPos . sourceLine) x `isInfixOf` sourcePosPretty x
    it "displays column number" $
      property $ \x ->
        (show . unPos . sourceColumn) x `isInfixOf` sourcePosPretty x
