{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.DebugSpec (spec) where

import Control.Monad
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

spec :: Spec
spec = do
  describe "dbg" $ do
    -- NOTE We don't test properties here to avoid a flood of debugging
    -- output when the test runs.
    context "when inner parser succeeds consuming input" $ do
      it "has no effect on how parser works" $ do
        let p = dbg "char" (char 'a')
            s = "ab"
        prs p s `shouldParse` 'a'
        prs' p s `succeedsLeaving` "b"
      it "its hints are preserved" $ do
        let p = dbg "many chars" (many (char 'a')) <* empty
            s = "abcd"
        prs p s `shouldFailWith` err 1 (etok 'a')
        prs' p s `failsLeaving` "bcd"
    context "when inner parser fails consuming input" $
      it "has no effect on how parser works" $ do
        let p = dbg "chars" (char 'a' *> char 'c')
            s = "abc"
        prs p s `shouldFailWith` err 1 (utok 'b' <> etok 'c')
        prs' p s `failsLeaving` "bc"
    context "when inner parser succeeds without consuming" $ do
      it "has no effect on how parser works" $ do
        let p = dbg "return" (return 'a')
            s = "abc"
        prs p s `shouldParse` 'a'
        prs' p s `succeedsLeaving` s
      it "its hints are preserved" $ do
        let p = dbg "many chars" (many (char 'a')) <* empty
            s = "bcd"
        prs p s `shouldFailWith` err 0 (etok 'a')
        prs' p s `failsLeaving` "bcd"
    context "when inner parser fails without consuming" $
      it "has no effect on how parser works" $ do
        let p = dbg "empty" (void empty)
            s = "abc"
        prs p s `shouldFailWith` err 0 mempty
        prs' p s `failsLeaving` s
