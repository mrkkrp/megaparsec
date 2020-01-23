{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.ByteSpec
  ( spec,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char
import Data.Maybe (fromMaybe)
import Data.Void
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc hiding (Parser, prs, prs')
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Byte

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

type Parser = Parsec Void ByteString

spec :: Spec
spec = do
  describe "newline" $
    checkStrLit "newline" "\n" (tokenToChunk bproxy <$> newline)
  describe "csrf" $
    checkStrLit "crlf newline" "\r\n" crlf
  describe "eol" $ do
    context "when stream begins with a newline"
      $ it "succeeds returning the newline"
      $ property
      $ \s -> do
        let s' = "\n" <> s
        prs eol s' `shouldParse` "\n"
        prs' eol s' `succeedsLeaving` s
    context "when stream begins with CRLF sequence"
      $ it "parses the CRLF sequence"
      $ property
      $ \s -> do
        let s' = "\r\n" <> s
        prs eol s' `shouldParse` "\r\n"
        prs' eol s' `succeedsLeaving` s
    context "when stream begins with '\\r', but it's not followed by '\\n'"
      $ it "signals correct parse error"
      $ property
      $ \ch -> ch /= 10 ==> do
        let s = "\r" <> B.singleton ch
        prs eol s
          `shouldFailWith` err 0 (utoks s <> elabel "end of line")
    context "when input stream is '\\r'"
      $ it "signals correct parse error"
      $ prs eol "\r"
        `shouldFailWith` err
          0
          (utok 13 <> elabel "end of line")
    context "when stream does not begin with newline or CRLF sequence"
      $ it "signals correct parse error"
      $ property
      $ \ch s -> (ch /= 13 && ch /= 10) ==> do
        let s' = B.singleton ch <> s
        prs eol s'
          `shouldFailWith` err
            0
            (utoks (B.take 2 s') <> elabel "end of line")
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs eol ""
        `shouldFailWith` err
          0
          (ueof <> elabel "end of line")
  describe "tab" $
    checkStrLit "tab" "\t" (tokenToChunk bproxy <$> tab)
  describe "space"
    $ it "consumes space up to first non-space character"
    $ property
    $ \s' -> do
      let (s0, s1) = B.partition isSpace' s'
          s = s0 <> s1
      prs space s `shouldParse` ()
      prs' space s `succeedsLeaving` s1
  describe "space1" $ do
    context "when stream does not start with a space character"
      $ it "signals correct parse error"
      $ property
      $ \ch s' -> not (isSpace' ch) ==> do
        let (s0, s1) = B.partition isSpace' s'
            s = B.singleton ch <> s0 <> s1
        prs space1 s `shouldFailWith` err 0 (utok ch <> elabel "white space")
        prs' space1 s `failsLeaving` s
    context "when stream starts with a space character"
      $ it "consumes space up to first non-space character"
      $ property
      $ \s' -> do
        let (s0, s1) = B.partition isSpace' s'
            s = " " <> s0 <> s1
        prs space1 s `shouldParse` ()
        prs' space1 s `succeedsLeaving` s1
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs space1 "" `shouldFailWith` err 0 (ueof <> elabel "white space")
  describe "controlChar" $
    checkCharPred "control character" (isControl . toChar) controlChar
  describe "spaceChar" $
    checkCharRange "white space" [9, 10, 11, 12, 13, 32, 160] spaceChar
  describe "alphaNumChar" $
    checkCharPred "alphanumeric character" (isAlphaNum . toChar) alphaNumChar
  describe "printChar" $
    checkCharPred "printable character" (isPrint . toChar) printChar
  describe "digitChar" $
    checkCharRange "digit" [48 .. 57] digitChar
  describe "binDigitChar" $
    checkCharRange "binary digit" [48 .. 49] binDigitChar
  describe "octDigitChar" $
    checkCharRange "octal digit" [48 .. 55] octDigitChar
  describe "hexDigitChar" $
    checkCharRange "hexadecimal digit" ([48 .. 57] ++ [97 .. 102] ++ [65 .. 70]) hexDigitChar
  describe "char'" $ do
    context "when stream begins with the character specified as argument"
      $ it "parses the character"
      $ property
      $ \ch s -> do
        let sl = B.cons (liftChar toLower ch) s
            su = B.cons (liftChar toUpper ch) s
            st = B.cons (liftChar toTitle ch) s
        prs (char' ch) sl `shouldParse` liftChar toLower ch
        prs (char' ch) su `shouldParse` liftChar toUpper ch
        prs (char' ch) st `shouldParse` liftChar toTitle ch
        prs' (char' ch) sl `succeedsLeaving` s
        prs' (char' ch) su `succeedsLeaving` s
        prs' (char' ch) st `succeedsLeaving` s
    context "when stream does not begin with the character specified as argument"
      $ it "signals correct parse error"
      $ property
      $ \ch ch' s -> not (casei ch ch') ==> do
        let s' = B.cons ch' s
            ms = utok ch' <> etok (liftChar toLower ch) <> etok (liftChar toUpper ch)
        prs (char' ch) s' `shouldFailWith` err 0 ms
        prs' (char' ch) s' `failsLeaving` s'
    context "when stream is empty"
      $ it "signals correct parse error"
      $ property
      $ \ch -> do
        let ms = ueof <> etok (liftChar toLower ch) <> etok (liftChar toUpper ch)
        prs (char' ch) "" `shouldFailWith` err 0 ms

----------------------------------------------------------------------------
-- Helpers

checkStrLit :: String -> ByteString -> Parser ByteString -> SpecWith ()
checkStrLit name ts p = do
  context ("when stream begins with " ++ name)
    $ it ("parses the " ++ name)
    $ property
    $ \s -> do
      let s' = ts <> s
      prs p s' `shouldParse` ts
      prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name)
    $ it "signals correct parse error"
    $ property
    $ \ch s -> ch /= B.head ts ==> do
      let s' = B.cons ch s
          us = B.take (B.length ts) s'
      prs p s' `shouldFailWith` err 0 (utoks us <> etoks ts)
      prs' p s' `failsLeaving` s'
  context "when stream is empty"
    $ it "signals correct parse error"
    $ prs p "" `shouldFailWith` err 0 (ueof <> etoks ts)

checkCharPred :: String -> (Word8 -> Bool) -> Parser Word8 -> SpecWith ()
checkCharPred name f p = do
  context ("when stream begins with " ++ name)
    $ it ("parses the " ++ name)
    $ property
    $ \ch s -> f ch ==> do
      let s' = B.singleton ch <> s
      prs p s' `shouldParse` ch
      prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name)
    $ it "signals correct parse error"
    $ property
    $ \ch s -> not (f ch) ==> do
      let s' = B.singleton ch <> s
      prs p s' `shouldFailWith` err 0 (utok ch <> elabel name)
      prs' p s' `failsLeaving` s'
  context "when stream is empty"
    $ it "signals correct parse error"
    $ prs p "" `shouldFailWith` err 0 (ueof <> elabel name)

checkCharRange :: String -> [Word8] -> Parser Word8 -> SpecWith ()
checkCharRange name tchs p = do
  forM_ tchs $ \tch ->
    context ("when stream begins with " ++ showTokens bproxy (nes tch))
      $ it ("parses the " ++ showTokens bproxy (nes tch))
      $ property
      $ \s -> do
        let s' = B.singleton tch <> s
        prs p s' `shouldParse` tch
        prs' p s' `succeedsLeaving` s
  context "when stream is empty"
    $ it "signals correct parse error"
    $ prs p "" `shouldFailWith` err 0 (ueof <> elabel name)

prs ::
  -- | Parser to run
  Parser a ->
  -- | Input for the parser
  ByteString ->
  -- | Result of parsing
  Either (ParseErrorBundle ByteString Void) a
prs p = parse p ""

prs' ::
  -- | Parser to run
  Parser a ->
  -- | Input for the parser
  ByteString ->
  -- | Result of parsing
  (State ByteString Void, Either (ParseErrorBundle ByteString Void) a)
prs' p s = runParser' p (initialState s)

-- | 'Word8'-specialized version of 'isSpace'.
isSpace' :: Word8 -> Bool
isSpace' x
  | x >= 9 && x <= 13 = True
  | x == 32 = True
  | x == 160 = True
  | otherwise = False

-- | Lift char transformation to byte transformation.
liftChar :: (Char -> Char) -> Word8 -> Word8
liftChar f x = (fromMaybe x . fromChar . f . toChar) x

-- | Compare two characters case-insensitively.
casei :: Word8 -> Word8 -> Bool
casei x y =
  x == liftChar toLower y
    || x == liftChar toUpper y
    || x == liftChar toTitle y
