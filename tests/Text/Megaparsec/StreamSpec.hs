{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.StreamSpec (spec) where

import Data.Char (isLetter, chr)
import Data.Proxy
import Data.Semigroup ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL

spec :: Spec
spec = do

  describe "String instance of Stream" $ do
    describe "tokenToChunk" $
      it "produces the same result as singleton with tokensToChunk" $
        property $ \ch ->
          tokenToChunk sproxy ch === tokensToChunk sproxy [ch]
    describe "tokensToChunk" $
      it "list of tokens is isomorphic to chunk" $
        property $ \ts ->
          chunkToTokens sproxy (tokensToChunk sproxy ts) === ts
    describe "chunkToTokens" $
      it "chunk is isomorphic to list of tokens" $
        property $ \chunk ->
          tokensToChunk sproxy (chunkToTokens sproxy chunk) === chunk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chunk ->
          chunkLength sproxy chunk === length chunk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chunk ->
          chunkEmpty sproxy chunk === (chunkLength sproxy chunk <= 0)
    describe "positionAt1" $
      it "just returns the given position" $
        property $ \pos t ->
          positionAt1 sproxy pos t === pos
    describe "positionAtN" $
      it "just returns the given position" $
        property $ \pos chunk ->
          positionAtN sproxy pos chunk === pos
    describe "advance1" $ do
      context "when given newline" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l _) ->
            advance1 sproxy w pos '\n' === SourcePos n (l <> pos1) pos1
      context "when given tab" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l c) ->
            advance1 sproxy w pos '\t' === SourcePos n l (toNextTab w c)
      context "when given other character" $
        it "works correctly" $
          property $ \ch w pos@(SourcePos n l c) ->
            (ch /= '\n' && ch /= '\t') ==>
              advance1 sproxy w pos ch === SourcePos n l (c <> pos1)
    describe "advanceN" $
      it "works correctly" $
        advanceN sproxy defaultTabWidth (initialPos "") "something\n\tfoo"
          === SourcePos "" (mkPos 2) (mkPos 12)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: String) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s -> not (null s) ==>
            take1_ (s :: String) === Just (head s, tail s)
    describe "takeN_" $ do
      context "requested length is 0" $
        it "returns Just empty chunk and original stream" $
          property $ \s ->
            takeN_ 0 (s :: String) === Just ("", s)
      context "requested length is greater than 0" $ do
        context "stream is empty" $
          it "returns Nothing" $
            property $ \(Positive n) ->
              takeN_ n ("" :: String) === Nothing
        context "stream is not empty" $
          it "returns a chunk of correct length and rest of the stream" $
            property $ \(Positive n) s -> not (null s) ==>
              takeN_ n (s :: String) === Just (splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === span isLetter s

  describe "ByteString instance of Stream" $ do
    describe "tokenToChunk" $
      it "produces the same result as singleton with tokensToChunk" $
        property $ \ch ->
          tokenToChunk bproxy ch === tokensToChunk bproxy [ch]
    describe "tokensToChunk" $
      it "list of tokens is isomorphic to chunk" $
        property $ \ts ->
          chunkToTokens bproxy (tokensToChunk bproxy ts) === ts
    describe "chunkToTokens" $
      it "chunk is isomorphic to list of tokens" $
        property $ \chunk ->
          tokensToChunk bproxy (chunkToTokens bproxy chunk) === chunk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chunk ->
          chunkLength bproxy chunk === B.length chunk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chunk ->
          chunkEmpty bproxy chunk === (chunkLength bproxy chunk <= 0)
    describe "positionAt1" $
      it "just returns the given position" $
        property $ \pos t ->
          positionAt1 bproxy pos t === pos
    describe "positionAtN" $
      it "just returns the given position" $
        property $ \pos chunk ->
          positionAtN bproxy pos chunk === pos
    describe "advance1" $ do
      context "when given newline" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l _) ->
            advance1 bproxy w pos 10 === SourcePos n (l <> pos1) pos1
      context "when given tab" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l c) ->
            advance1 bproxy w pos 9 === SourcePos n l (toNextTab w c)
      context "when given other character" $
        it "works correctly" $
          property $ \ch w pos@(SourcePos n l c) ->
            (ch /= 10 && ch /= 9) ==>
              advance1 bproxy w pos ch === SourcePos n l (c <> pos1)
    describe "advanceN" $
      it "works correctly" $
        advanceN bproxy defaultTabWidth (initialPos "") "something\n\tfoo"
          === SourcePos "" (mkPos 2) (mkPos 12)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: B.ByteString) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s -> not (B.null s) ==>
            take1_ (s :: B.ByteString) === B.uncons s
    describe "takeN_" $ do
      context "requested length is 0" $
        it "returns Just empty chunk and original stream" $
          property $ \s ->
            takeN_ 0 (s :: B.ByteString) === Just ("", s)
      context "requested length is greater than 0" $ do
        context "stream is empty" $
          it "returns Nothing" $
            property $ \(Positive n) ->
              takeN_ n ("" :: B.ByteString) === Nothing
        context "stream is not empty" $
          it "returns a chunk of correct length and rest of the stream" $
            property $ \(Positive n) s -> not (B.null s) ==>
              takeN_ n (s :: B.ByteString) === Just (B.splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          let f = isLetter . chr . fromIntegral
          in takeWhile_ f s === B.span f s

  describe "Lazy ByteString instance of Stream" $ do
    describe "tokenToChunk" $
      it "produces the same result as singleton with tokensToChunk" $
        property $ \ch ->
          tokenToChunk blproxy ch === tokensToChunk blproxy [ch]
    describe "tokensToChunk" $
      it "list of tokens is isomorphic to chunk" $
        property $ \ts ->
          chunkToTokens blproxy (tokensToChunk blproxy ts) === ts
    describe "chunkToTokens" $
      it "chunk is isomorphic to list of tokens" $
        property $ \chunk ->
          tokensToChunk blproxy (chunkToTokens blproxy chunk) === chunk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chunk ->
          chunkLength blproxy chunk === fromIntegral (BL.length chunk)
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chunk ->
          chunkEmpty blproxy chunk === (chunkLength blproxy chunk <= 0)
    describe "positionAt1" $
      it "just returns the given position" $
        property $ \pos t ->
          positionAt1 blproxy pos t === pos
    describe "positionAtN" $
      it "just returns the given position" $
        property $ \pos chunk ->
          positionAtN blproxy pos chunk === pos
    describe "advance1" $ do
      context "when given newline" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l _) ->
            advance1 blproxy w pos 10 === SourcePos n (l <> pos1) pos1
      context "when given tab" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l c) ->
            advance1 blproxy w pos 9 === SourcePos n l (toNextTab w c)
      context "when given other character" $
        it "works correctly" $
          property $ \ch w pos@(SourcePos n l c) ->
            (ch /= 10 && ch /= 9) ==>
              advance1 blproxy w pos ch === SourcePos n l (c <> pos1)
    describe "advanceN" $
      it "works correctly" $
        advanceN blproxy defaultTabWidth (initialPos "") "something\n\tfoo"
          === SourcePos "" (mkPos 2) (mkPos 12)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: BL.ByteString) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s -> not (BL.null s) ==>
            take1_ (s :: BL.ByteString) === BL.uncons s
    describe "takeN_" $ do
      context "requested length is 0" $
        it "returns Just empty chunk and original stream" $
          property $ \s ->
            takeN_ 0 (s :: BL.ByteString) === Just ("", s)
      context "requested length is greater than 0" $ do
        context "stream is empty" $
          it "returns Nothing" $
            property $ \(Positive n) ->
              takeN_ n ("" :: BL.ByteString) === Nothing
        context "stream is not empty" $
          it "returns a chunk of correct length and rest of the stream" $
            property $ \(Positive n) s -> not (BL.null s) ==>
              takeN_ n (s :: BL.ByteString) === Just (BL.splitAt (fromIntegral n) s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          let f = isLetter . chr . fromIntegral
          in takeWhile_ f s === BL.span f s

  describe "Text instance of Stream" $ do
    describe "tokenToChunk" $
      it "produces the same result as singleton with tokensToChunk" $
        property $ \ch ->
          tokenToChunk tproxy ch === tokensToChunk tproxy [ch]
    describe "tokensToChunk" $
      it "list of tokens is isomorphic to chunk" $
        property $ \ts ->
          chunkToTokens tproxy (tokensToChunk tproxy ts) === ts
    describe "chunkToTokens" $
      it "chunk is isomorphic to list of tokens" $
        property $ \chunk ->
          tokensToChunk tproxy (chunkToTokens tproxy chunk) === chunk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chunk ->
          chunkLength tproxy chunk === T.length chunk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chunk ->
          chunkEmpty tproxy chunk === (chunkLength tproxy chunk <= 0)
    describe "positionAt1" $
      it "just returns the given position" $
        property $ \pos t ->
          positionAt1 tproxy pos t === pos
    describe "positionAtN" $
      it "just returns the given position" $
        property $ \pos chunk ->
          positionAtN tproxy pos chunk === pos
    describe "advance1" $ do
      context "when given newline" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l _) ->
            advance1 tproxy w pos '\n' === SourcePos n (l <> pos1) pos1
      context "when given tab" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l c) ->
            advance1 tproxy w pos '\t' === SourcePos n l (toNextTab w c)
      context "when given other character" $
        it "works correctly" $
          property $ \ch w pos@(SourcePos n l c) ->
            (ch /= '\n' && ch /= '\t') ==>
              advance1 tproxy w pos ch === SourcePos n l (c <> pos1)
    describe "advanceN" $
      it "works correctly" $
        advanceN tproxy defaultTabWidth (initialPos "") "something\n\tfoo"
          === SourcePos "" (mkPos 2) (mkPos 12)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: T.Text) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s -> not (T.null s) ==>
            take1_ (s :: T.Text) === T.uncons s
    describe "takeN_" $ do
      context "requested length is 0" $
        it "returns Just empty chunk and original stream" $
          property $ \s ->
            takeN_ 0 (s :: T.Text) === Just ("", s)
      context "requested length is greater than 0" $ do
        context "stream is empty" $
          it "returns Nothing" $
            property $ \(Positive n) ->
              takeN_ n ("" :: T.Text) === Nothing
        context "stream is not empty" $
          it "returns a chunk of correct length and rest of the stream" $
            property $ \(Positive n) s -> not (T.null s) ==>
              takeN_ n (s :: T.Text) === Just (T.splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === T.span isLetter s

  describe "Lazy Text instance of Stream" $ do
    describe "tokenToChunk" $
      it "produces the same result as singleton with tokensToChunk" $
        property $ \ch ->
          tokenToChunk tlproxy ch === tokensToChunk tlproxy [ch]
    describe "tokensToChunk" $
      it "list of tokens is isomorphic to chunk" $
        property $ \ts ->
          chunkToTokens tlproxy (tokensToChunk tlproxy ts) === ts
    describe "chunkToTokens" $
      it "chunk is isomorphic to list of tokens" $
        property $ \chunk ->
          tokensToChunk tlproxy (chunkToTokens tlproxy chunk) === chunk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chunk ->
          chunkLength tlproxy chunk === fromIntegral (TL.length chunk)
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chunk ->
          chunkEmpty tlproxy chunk === (chunkLength tlproxy chunk <= 0)
    describe "positionAt1" $
      it "just returns the given position" $
        property $ \pos t ->
          positionAt1 tlproxy pos t === pos
    describe "positionAtN" $
      it "just returns the given position" $
        property $ \pos chunk ->
          positionAtN tlproxy pos chunk === pos
    describe "advance1" $ do
      context "when given newline" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l _) ->
            advance1 tlproxy w pos '\n' === SourcePos n (l <> pos1) pos1
      context "when given tab" $
        it "works correctly" $
          property $ \w pos@(SourcePos n l c) ->
            advance1 tlproxy w pos '\t' === SourcePos n l (toNextTab w c)
      context "when given other character" $
        it "works correctly" $
          property $ \ch w pos@(SourcePos n l c) ->
            (ch /= '\n' && ch /= '\t') ==>
              advance1 tlproxy w pos ch === SourcePos n l (c <> pos1)
    describe "advanceN" $
      it "works correctly" $
        advanceN tlproxy defaultTabWidth (initialPos "") "something\n\tfoo"
          === SourcePos "" (mkPos 2) (mkPos 12)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: TL.Text) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s -> not (TL.null s) ==>
            take1_ (s :: TL.Text) === TL.uncons s
    describe "takeN_" $ do
      context "requested length is 0" $
        it "returns Just empty chunk and original stream" $
          property $ \s ->
            takeN_ 0 (s :: TL.Text) === Just ("", s)
      context "requested length is greater than 0" $ do
        context "stream is empty" $
          it "returns Nothing" $
            property $ \(Positive n) ->
              takeN_ n ("" :: TL.Text) === Nothing
        context "stream is not empty" $
          it "returns a chunk of correct length and rest of the stream" $
            property $ \(Positive n) s -> not (TL.null s) ==>
              takeN_ n (s :: TL.Text) === Just (TL.splitAt (fromIntegral n) s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === TL.span isLetter s

----------------------------------------------------------------------------
-- Helpers

toNextTab :: Pos -> Pos -> Pos
toNextTab w' c' = mkPos $ c + w - ((c - 1) `rem` w)
  where
    w = unPos w'
    c = unPos c'

sproxy :: Proxy String
sproxy = Proxy

bproxy :: Proxy B.ByteString
bproxy = Proxy

blproxy :: Proxy BL.ByteString
blproxy = Proxy

tproxy :: Proxy T.Text
tproxy = Proxy

tlproxy :: Proxy TL.Text
tlproxy = Proxy
