{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Text.Megaparsec.StreamSpec (spec) where

import Control.Monad
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.Char (chr, isControl, isLetter, isSpace)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.String (IsString)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

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
        property $ \chk ->
          tokensToChunk sproxy (chunkToTokens sproxy chk) === chk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chk ->
          chunkLength sproxy chk === length chk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chk ->
          chunkEmpty sproxy chk === (chunkLength sproxy chk <= 0)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: String) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s ->
            not (null s)
              ==> take1_ (s :: String) === Just (head s, tail s)
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
            property $ \(Positive n) s ->
              not (null s)
                ==> takeN_ n (s :: String) === Just (splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === span isLetter s
    describeShowTokens sproxy quotedCharGen
    describeReachOffset sproxy
    describeReachOffsetNoLine sproxy

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
        property $ \chk ->
          tokensToChunk bproxy (chunkToTokens bproxy chk) === chk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chk ->
          chunkLength bproxy chk === B.length chk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chk ->
          chunkEmpty bproxy chk === (chunkLength bproxy chk <= 0)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: B.ByteString) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s ->
            not (B.null s)
              ==> take1_ (s :: B.ByteString) === B.uncons s
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
            property $ \(Positive n) s ->
              not (B.null s)
                ==> takeN_ n (s :: B.ByteString) === Just (B.splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          let f = isLetter . chr . fromIntegral
           in takeWhile_ f s === B.span f s
    describeShowTokens bproxy quotedWordGen
    describeReachOffset bproxy
    describeReachOffsetNoLine bproxy

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
        property $ \chk ->
          tokensToChunk blproxy (chunkToTokens blproxy chk) === chk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chk ->
          chunkLength blproxy chk === fromIntegral (BL.length chk)
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chk ->
          chunkEmpty blproxy chk === (chunkLength blproxy chk <= 0)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: BL.ByteString) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s ->
            not (BL.null s)
              ==> take1_ (s :: BL.ByteString) === BL.uncons s
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
            property $ \(Positive n) s ->
              not (BL.null s)
                ==> takeN_ n (s :: BL.ByteString) === Just (BL.splitAt (fromIntegral n) s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          let f = isLetter . chr . fromIntegral
           in takeWhile_ f s === BL.span f s
    describeShowTokens blproxy quotedWordGen
    describeReachOffset blproxy
    describeReachOffsetNoLine blproxy

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
        property $ \chk ->
          tokensToChunk tproxy (chunkToTokens tproxy chk) === chk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chk ->
          chunkLength tproxy chk === T.length chk
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chk ->
          chunkEmpty tproxy chk === (chunkLength tproxy chk <= 0)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: T.Text) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s ->
            not (T.null s)
              ==> take1_ (s :: T.Text) === T.uncons s
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
            property $ \(Positive n) s ->
              not (T.null s)
                ==> takeN_ n (s :: T.Text) === Just (T.splitAt n s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === T.span isLetter s
    describeShowTokens tproxy quotedCharGen
    describeReachOffset tproxy
    describeReachOffsetNoLine tproxy

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
        property $ \chk ->
          tokensToChunk tlproxy (chunkToTokens tlproxy chk) === chk
    describe "chunkLength" $
      it "returns correct length of given chunk" $
        property $ \chk ->
          chunkLength tlproxy chk === fromIntegral (TL.length chk)
    describe "chunkEmpty" $
      it "only true when chunkLength returns 0" $
        property $ \chk ->
          chunkEmpty tlproxy chk === (chunkLength tlproxy chk <= 0)
    describe "take1_" $ do
      context "when input in empty" $
        it "returns Nothing" $
          take1_ ("" :: TL.Text) === Nothing
      context "when input is not empty" $
        it "unconses a token" $
          property $ \s ->
            not (TL.null s)
              ==> take1_ (s :: TL.Text) === TL.uncons s
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
            property $ \(Positive n) s ->
              not (TL.null s)
                ==> takeN_ n (s :: TL.Text) === Just (TL.splitAt (fromIntegral n) s)
    describe "takeWhile_" $
      it "extracts a chunk that is a prefix consisting of matching tokens" $
        property $ \s ->
          takeWhile_ isLetter s === TL.span isLetter s
    describeShowTokens tlproxy quotedCharGen
    describeReachOffset tlproxy
    describeReachOffsetNoLine tlproxy

----------------------------------------------------------------------------
-- Helpers

-- | Generic block of tests for the 'showTokens' method.
describeShowTokens ::
  forall s.
  ( Stream s,
    IsString (Tokens s),
    Show (Token s),
    Arbitrary (Token s)
  ) =>
  -- | 'Proxy' that clarifies the type of stream
  Proxy s ->
  -- | Generator of tokens that should be simply quoted
  Gen (Token s) ->
  Spec
describeShowTokens pxy quotedTokGen =
  describe "showTokens" $ do
    let f :: Tokens s -> String -> Expectation
        f x y = showTokens pxy (NE.fromList $ chunkToTokens pxy x) `shouldBe` y
    it
      "shows CRLF newline correctly"
      (f "\r\n" "crlf newline")
    it
      "shows null byte correctly"
      (f "\NUL" "null")
    it
      "shows start of heading correctly"
      (f "\SOH" "start of heading")
    it
      "shows start of text correctly"
      (f "\STX" "start of text")
    it
      "shows end of text correctly"
      (f "\ETX" "end of text")
    it
      "shows end of transmission correctly"
      (f "\EOT" "end of transmission")
    it
      "shows enquiry correctly"
      (f "\ENQ" "enquiry")
    it
      "shows acknowledge correctly"
      (f "\ACK" "acknowledge")
    it
      "shows bell correctly"
      (f "\BEL" "bell")
    it
      "shows backspace correctly"
      (f "\BS" "backspace")
    it
      "shows tab correctly"
      (f "\t" "tab")
    it
      "shows newline correctly"
      (f "\n" "newline")
    it
      "shows vertical tab correctly"
      (f "\v" "vertical tab")
    it
      "shows form feed correctly"
      (f "\f" "form feed")
    it
      "shows carriage return correctly"
      (f "\r" "carriage return")
    it
      "shows shift out correctly"
      (f "\SO" "shift out")
    it
      "shows shift in correctly"
      (f "\SI" "shift in")
    it
      "shows data link escape correctly"
      (f "\DLE" "data link escape")
    it
      "shows device control one correctly"
      (f "\DC1" "device control one")
    it
      "shows device control two correctly"
      (f "\DC2" "device control two")
    it
      "shows device control three correctly"
      (f "\DC3" "device control three")
    it
      "shows device control four correctly"
      (f "\DC4" "device control four")
    it
      "shows negative acknowledge correctly"
      (f "\NAK" "negative acknowledge")
    it
      "shows synchronous idle correctly"
      (f "\SYN" "synchronous idle")
    it
      "shows end of transmission block correctly"
      (f "\ETB" "end of transmission block")
    it
      "shows cancel correctly"
      (f "\CAN" "cancel")
    it
      "shows end of medium correctly"
      (f "\EM" "end of medium")
    it
      "shows substitute correctly"
      (f "\SUB" "substitute")
    it
      "shows escape correctly"
      (f "\ESC" "escape")
    it
      "shows file separator correctly"
      (f "\FS" "file separator")
    it
      "shows group separator correctly"
      (f "\GS" "group separator")
    it
      "shows record separator correctly"
      (f "\RS" "record separator")
    it
      "shows unit separator correctly"
      (f "\US" "unit separator")
    it
      "shows delete correctly"
      (f "\DEL" "delete")
    it
      "shows space correctly"
      (f " " "space")
    it
      "shows non-breaking space correctly"
      (f "\160" "non-breaking space")
    it "shows other single characters in single quotes" $
      property $
        forAll quotedTokGen $ \x -> do
          let r = showTokens pxy (x :| [])
          head r `shouldBe` '\''
          last r `shouldBe` '\''
    it "shows strings in double quotes" $
      property $ \x (NonEmpty xs) -> do
        let r = showTokens pxy (x :| xs)
        when (r == "crlf newline") discard
        head r `shouldBe` '\"'
        last r `shouldBe` '\"'
    it
      "shows control characters in long strings property"
      (f "{\n" "\"{<newline>\"")

-- | Generic block of tests for the 'reachOffset' method.
describeReachOffset ::
  forall s.
  ( Stream s,
    IsString s,
    Show s,
    Arbitrary s
  ) =>
  -- | 'Proxy' that clarifies the type of stream
  Proxy s ->
  Spec
describeReachOffset Proxy =
  describe "reachOffset" $ do
    it "returns correct SourcePos (newline)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "\n" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos . snd $ reachOffset o pst
            SourcePos n l _ = pstateSourcePos pst
        r `shouldBe` SourcePos n (l <> pos1) pos1
    it "returns correct SourcePos (tab)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "\t" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos . snd $ reachOffset o pst
            SourcePos n l c = pstateSourcePos pst
            w = pstateTabWidth pst
        r `shouldBe` SourcePos n l (toNextTab w c)
    it "returns correct SourcePos (other)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "a" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos . snd $ reachOffset o pst
            SourcePos n l c = pstateSourcePos pst
        r `shouldBe` SourcePos n l (c <> pos1)
    it "replaces empty line with <empty line>" $
      property $ \o pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "" :: s,
                  pstateLinePrefix = ""
                }
            (r, _) = reachOffset o pst
        r `shouldBe` "<empty line>"
    it "replaces tabs with spaces in returned line" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "\ta\t" :: s,
                  pstateLinePrefix = "\t"
                }
            (r, _) = reachOffset 2 pst
            w = unPos (pstateTabWidth pst)
            r' = replicate (w * 2) ' ' ++ "a" ++ replicate w ' '
        r `shouldBe` r'
    it "returns correct line (with line prefix)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "foo\nbar\nbaz" :: s,
                  pstateLinePrefix = "123"
                }
            (r, _) = reachOffset 0 pst
        r `shouldBe` "123foo"
    it "returns correct line (without line prefix)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "foo\nbar\nbaz" :: s,
                  pstateOffset = 0
                }
            (r, _) = reachOffset 4 pst
        r `shouldBe` "bar"
    it "works incrementally" $
      property $ \os' (NonNegative d) s -> do
        let os = getNonNegative <$> os'
            s' :: PosState String
            s' = foldl' f s os
            o' = case os of
              [] -> d
              xs -> maximum xs + d
            f pst o =
              let (_, pst') = reachOffset o pst
               in pst'
        reachOffset o' s `shouldBe` reachOffset o' s'

-- | Generic block of tests for the 'reachOffsetNoLine' method.
describeReachOffsetNoLine ::
  forall s.
  ( Stream s,
    IsString s,
    Show s,
    Arbitrary s
  ) =>
  -- | 'Proxy' that clarifies the type of stream
  Proxy s ->
  Spec
describeReachOffsetNoLine Proxy =
  describe "reachOffsetNoLine" $ do
    it "returns correct SourcePos (newline)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "\n" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos (reachOffsetNoLine o pst)
            SourcePos n l _ = pstateSourcePos pst
        r `shouldBe` SourcePos n (l <> pos1) pos1
    it "returns correct SourcePos (tab)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "\t" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos (reachOffsetNoLine o pst)
            SourcePos n l c = pstateSourcePos pst
            w = pstateTabWidth pst
        r `shouldBe` SourcePos n l (toNextTab w c)
    it "returns correct SourcePos (other)" $
      property $ \pst' -> do
        let pst =
              (pst' :: PosState s)
                { pstateInput = "a" :: s
                }
            o = pstateOffset pst + 1
            r = pstateSourcePos (reachOffsetNoLine o pst)
            SourcePos n l c = pstateSourcePos pst
        r `shouldBe` SourcePos n l (c <> pos1)
    it "works incrementally" $
      property $ \os' (NonNegative d) s -> do
        let os = getNonNegative <$> os'
            s' :: PosState String
            s' = foldl' f s os
            o' = case os of
              [] -> d
              xs -> maximum xs + d
            f pst o =
              let pst' = reachOffsetNoLine o pst
               in pst'
        reachOffsetNoLine o' s `shouldBe` reachOffsetNoLine o' s'

-- | Get next tab position given tab width and current column.
toNextTab ::
  -- | Tab width
  Pos ->
  -- | Current column
  Pos ->
  -- | Column of next tab position
  Pos
toNextTab w' c' = mkPos $ c + w - ((c - 1) `rem` w)
  where
    w = unPos w'
    c = unPos c'

quotedCharGen :: Gen Char
quotedCharGen = arbitrary `suchThat` isQuotedChar

quotedWordGen :: Gen Word8
quotedWordGen = arbitrary `suchThat` (isQuotedChar . toChar)

-- | Return 'True' if the 'Char' should be simply quoted by the 'showTokens'
-- method, i.e. it's not a character with a special representation.
isQuotedChar :: Char -> Bool
isQuotedChar x = not (isControl x) && not (isSpace x)
