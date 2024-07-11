module Text.Megaparsec.UnicodeSpec (spec) where

import Test.Hspec
import qualified Text.Megaparsec.Unicode as Unicode

spec :: Spec
spec = do
  describe "stringLength" $
    it "computes correct length in the presense of wide chars" $
      Unicode.stringLength "123 구구 이면" `shouldBe` 13
  describe "charLength" $ do
    it "returns 1 for non-wide chars" $
      Unicode.charLength 'a' `shouldBe` 1
    it "returns 2 for wide chars" $
      Unicode.charLength '구' `shouldBe` 2
  describe "isWideChar" $ do
    it "returns False for non-wide chars" $
      Unicode.isWideChar 'a' `shouldBe` False
    it "returns True for wide chars" $
      Unicode.isWideChar '구' `shouldBe` True
