{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Text.Megaparsec.Byte.BinarySpec (spec) where

import qualified Data.ByteString.Builder as BB
import qualified Data.ByteString.Lazy as BL
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Byte.Binary

spec :: Spec
spec = do
  describe "word8" $
    testParser word8 BB.word8
  describe "word16le" $
    testParser word16le BB.word16LE
  describe "word16be" $
    testParser word16be BB.word16BE
  describe "word32le" $
    testParser word32le BB.word32LE
  describe "word32be" $
    testParser word32be BB.word32BE
  describe "word64le" $
    testParser word64le BB.word64LE
  describe "word64be" $
    testParser word64be BB.word64BE
  describe "int8" $
    testParser int8 BB.int8
  describe "int16le" $
    testParser int16le BB.int16LE
  describe "int16be" $
    testParser int16be BB.int16BE
  describe "int32le" $ do
    testParser int32le BB.int32LE
  describe "int32be" $ do
    testParser int32be BB.int32BE
  describe "int64le" $ do
    testParser int64le BB.int64LE
  describe "int64be" $ do
    testParser int64be BB.int64BE

----------------------------------------------------------------------------
-- Helpers

-- | Test a binary parser.
testParser ::
  (Arbitrary a, Show a, Eq a) =>
  -- | The parser to test
  (forall s. (Stream s, BinaryChunk (Tokens s)) => Parsec Void s a) ->
  -- | Builder for the values that the parer consumes
  (a -> BB.Builder) ->
  SpecWith ()
testParser parser serializer = do
  it "works with strict ByteString" $
    property $ \x -> do
      let rendered = (BL.toStrict . BB.toLazyByteString . serializer) x
      parse (parser <* eof) "" rendered `shouldParse` x
  it "works with lazy ByteString" $
    property $ \x -> do
      let rendered = (BB.toLazyByteString . serializer) x
      parse (parser <* eof) "" rendered `shouldParse` x
