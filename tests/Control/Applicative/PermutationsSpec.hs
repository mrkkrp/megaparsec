{-# LANGUAGE TypeFamilies #-}

module Control.Applicative.PermutationsSpec (spec) where

import Control.Applicative.Permutations
import Control.Monad
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do
  describe "runPermutation & Permutation" $ do
    describe "Functor instance" $ do
      it "obeys identity law" $
        property $ \n ->
          prsp (fmap id (pure (n :: Int))) "" ===
          prsp (id (pure n)) ""
      it "obeys composition law" $
        property $ \n m t ->
          let f = (+ m)
              g = (* t)
          in prs (fmap (f . g) (pure (n :: Int))) "" ===
             prs ((fmap f . fmap g) (pure n)) ""
    describe "Applicative instance" $ do
      it "obeys identity law" $
        property $ \n ->
          prsp (pure id <*> pure (n :: Int)) "" ===
          prsp (pure n) ""
      it "obeys composition law" $
        property $ \n m t ->
          let u = pure (+ m)
              v = pure (* t)
              w = pure (n :: Int)
          in prsp (pure (.) <*> u <*> v <*> w) "" ===
             prsp (u <*> (v <*> w)) ""
      it "obeys homomorphism law" $
        property $ \x m ->
          let f = (+ m)
          in prsp (pure f <*> pure (x :: Int)) "" ===
             prsp (pure (f x)) ""
      it "obeys interchange law" $
        property $ \n y ->
          let u = pure (+ n)
          in prsp (u <*> pure (y :: Int)) "" ===
             prsp (pure ($ y) <*> u) ""
  describe "toPermutation" $
    it "works" $
      property $ \xs s' -> forAll (shuffle xs) $ \ys -> do
        let s = ys ++ s'
            p = foldr f (pure []) xs
            f x p' = (:) <$> toPermutation (char x) <*> p'
        prsp p s `shouldParse` xs
        prsp' p s `succeedsLeaving` s'
  describe "toPermutationWithDefault" $ do
    let testCases =
          [ ("abc", "abc", "")
          , ("cba", "abc", "")
          , ("bbc", "xbz", "bc")
          , ("aaa", "ayz", "aa")
          , ("",    "xyz", "")
          ]
    forM_ testCases $ \(i, o, r) ->
      it ("parses \"" ++ i ++ "\" as \"" ++ o ++ "\" leaving \"" ++ r ++ "\"") $ do
        prsp testPermParser i `shouldParse` o
        prsp' testPermParser i `succeedsLeaving` r
  describe "intercalateEffect" $ do
    let p = intercalateEffect (char ',') testPermParser
        testCases =
          [ ("a,b,c", "abc", "")
          , ("c,b,a", "abc", "")
          , ("b,b,c", "xbz", "b,c")
          , ("a,a,a", "ayz", "a,a")
          , (",",     "xyz", ",")
          ]
    forM_ testCases $ \(i, o, r) ->
      it ("parses \"" ++ i ++ "\" as \"" ++ o ++ "\" leaving \"" ++ r ++ "\"") $ do
        prs p i `shouldParse` o
        prs' p i `succeedsLeaving` r

prsp
  :: Permutation Parser a
  -> String
  -> Either (ParseError Char Void) a
prsp p = prs (runPermutation p)

prsp'
  :: Permutation Parser a
  -> String
  -> (State String, Either (ParseError Char Void) a)
prsp' p = prs' (runPermutation p)

testPermParser :: Permutation Parser String
testPermParser =
  f <$> toPermutationWithDefault 'x' (char 'a')
    <*> toPermutationWithDefault 'y' (char 'b')
    <*> toPermutationWithDefault 'z' (char 'c')
  where
    f a b c = [a,b,c]
