module Text.Megaparsec.StreamSpec (spec) where

-- import Data.Proxy
-- import Data.Void
import Test.Hspec
-- import Test.Hspec.Megaparsec
-- import Test.Hspec.Megaparsec.AdHoc
-- import Test.QuickCheck
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import qualified Data.ByteString      as B
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Text            as T
-- import qualified Data.Text.Lazy       as TL

spec :: Spec
spec = do

  -- TODO This should be moved in some form to Text.Megaparsec.StreamSpec

  return ()

  -- describe "non-String instances of Stream" $ do
  --   context "lazy ByteString" $ do
  --     it "unconses correctly" $
  --       property $ \ch' n -> do
  --         let p  = many (char ch) :: Parsec Void BL.ByteString String
  --             s  = replicate (getNonNegative n) ch
  --             ch = byteToChar ch'
  --         parse p "" (BL.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy BL.ByteString) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "strict ByteString" $ do
  --     it "unconses correctly" $
  --       property $ \ch' n -> do
  --         let p  = many (char ch) :: Parsec Void B.ByteString String
  --             s  = replicate (getNonNegative n) ch
  --             ch = byteToChar ch'
  --         parse p "" (B.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy B.ByteString) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "lazy Text" $ do
  --     it "unconses correctly" $
  --       property $ \ch n -> do
  --         let p = many (char ch) :: Parsec Void TL.Text String
  --             s = replicate (getNonNegative n) ch
  --         parse p "" (TL.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy TL.Text) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "strict Text" $ do
  --     it "unconses correctly" $
  --       property $ \ch n -> do
  --         let p = many (char ch) :: Parsec Void T.Text String
  --             s = replicate (getNonNegative n) ch
  --         parse p "" (T.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy T.Text) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
