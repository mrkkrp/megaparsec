{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.Byte.LexerSpec
  ( spec,
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char (intToDigit, toUpper)
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Void
import Data.Word (Word8)
import Numeric (showFFloatAlt, showHex, showInt, showIntAtBase, showOct)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import qualified Text.Megaparsec.Byte as B
import Text.Megaparsec.Byte.Lexer

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

type Parser = Parsec Void ByteString

spec :: Spec
spec = do
  describe "skipLineComment" $ do
    context "when there is no newline at the end of line"
      $ it "is picked up successfully"
      $ do
        let p = space B.space1 (skipLineComment "//") empty <* eof
            s = "  // this line comment doesn't have a newline at the end "
        prs p s `shouldParse` ()
        prs' p s `succeedsLeaving` ""
    it "inner characters are labelled properly" $ do
      let p = skipLineComment "//" <* empty
          s = "// here we go"
      prs p s `shouldFailWith` err (B.length s) (elabel "character")
      prs' p s `failsLeaving` ""

  describe "skipBlockComment"
    $ it "skips a simple block comment"
    $ do
      let p = skipBlockComment "/*" "*/"
          s = "/* here we go */foo!"
      prs p s `shouldParse` ()
      prs' p s `succeedsLeaving` "foo!"

  describe "skipBlockCommentNested"
    $ context "when it runs into nested block comments"
    $ it "parses them all right"
    $ do
      let p =
            space
              B.space1
              empty
              (skipBlockCommentNested "/*" "*/")
              <* eof
          s = " /* foo bar /* baz */ quux */ "
      prs p s `shouldParse` ()
      prs' p s `succeedsLeaving` ""

  describe "decimal" $ do
    context "when stream begins with decimal digits"
      $ it "they are parsed as an integer"
      $ property
      $ \n' -> do
        let p = decimal :: Parser Integer
            n = getNonNegative n'
            s = B8.pack (showInt n "")
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream does not begin with decimal digits"
      $ it "signals correct parse error"
      $ property
      $ \a as -> not (isDigit a) ==> do
        let p = decimal :: Parser Integer
            s = B.pack (a : as)
        prs p s `shouldFailWith` err 0 (utok a <> elabel "integer")
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (decimal :: Parser Integer) ""
        `shouldFailWith` err 0 (ueof <> elabel "integer")

  describe "binary" $ do
    context "when stream begins with binary digits"
      $ it "they are parsed as an integer"
      $ property
      $ \n' -> do
        let p = binary :: Parser Integer
            n = getNonNegative n'
            s = B8.pack (showIntAtBase 2 intToDigit n "")
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream does not begin with binary digits"
      $ it "signals correct parse error"
      $ property
      $ \a as -> a /= 48 && a /= 49 ==> do
        let p = binary :: Parser Integer
            s = B.pack (a : as)
        prs p s
          `shouldFailWith` err 0 (utok a <> elabel "binary integer")
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (binary :: Parser Integer) ""
        `shouldFailWith` err 0 (ueof <> elabel "binary integer")

  describe "octal" $ do
    context "when stream begins with octal digits"
      $ it "they are parsed as an integer"
      $ property
      $ \n' -> do
        let p = octal :: Parser Integer
            n = getNonNegative n'
            s = B8.pack (showOct n "")
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream does not begin with octal digits"
      $ it "signals correct parse error"
      $ property
      $ \a as -> not (isOctDigit a) ==> do
        let p = octal :: Parser Integer
            s = B.pack (a : as)
        prs p s
          `shouldFailWith` err 0 (utok a <> elabel "octal integer")
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (octal :: Parser Integer) ""
        `shouldFailWith` err 0 (ueof <> elabel "octal integer")

  describe "hexadecimal" $ do
    context "when stream begins with hexadecimal digits"
      $ it "they are parsed as an integer"
      $ property
      $ \n' -> do
        let p = hexadecimal :: Parser Integer
            n = getNonNegative n'
            s = B8.pack (showHex n "")
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream begins with hexadecimal digits (uppercase)"
      $ it "they are parsed as an integer"
      $ property
      $ \n' -> do
        let p = hexadecimal :: Parser Integer
            n = getNonNegative n'
            s = B8.pack (toUpper <$> showHex n "")
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream does not begin with hexadecimal digits"
      $ it "signals correct parse error"
      $ property
      $ \a as -> not (isHexDigit a) ==> do
        let p = hexadecimal :: Parser Integer
            s = B.pack (a : as)
        prs p s
          `shouldFailWith` err 0 (utok a <> elabel "hexadecimal integer")
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (hexadecimal :: Parser Integer) ""
        `shouldFailWith` err 0 (ueof <> elabel "hexadecimal integer")

  describe "scientific" $ do
    context "when stream begins with a number"
      $ it "parses it"
      $ property
      $ \n' -> do
        let p = scientific :: Parser Scientific
            s =
              B8.pack $
                either
                  (show . getNonNegative)
                  (show . getNonNegative)
                  (n' :: Either (NonNegative Integer) (NonNegative Double))
        prs p s `shouldParse` case n' of
          Left x -> fromIntegral (getNonNegative x)
          Right x -> fromFloatDigits (getNonNegative x)
        prs' p s `succeedsLeaving` ""
    context "when fractional part is interrupted"
      $ it "signals correct parse error"
      $ property
      $ \(NonNegative n) -> do
        let p = scientific <* empty :: Parser Scientific
            s = B8.pack (showFFloatAlt Nothing (n :: Double) "")
        prs p s
          `shouldFailWith` err
            (B.length s)
            (etok 69 <> etok 101 <> elabel "digit")
        prs' p s `failsLeaving` ""
    context "when whole part is followed by a dot without valid fractional part"
      $ it "parsing of fractional part is backtracked correctly"
      $ property
      $ \(NonNegative n) -> do
        let p = scientific :: Parser Scientific
            s = B8.pack $ showInt (n :: Integer) ".err"
        prs p s `shouldParse` fromIntegral n
        prs' p s `succeedsLeaving` ".err"
    context "when number is followed by something starting with 'e'"
      $ it "parsing of exponent part is backtracked correctly"
      $ property
      $ \(NonNegative n) -> do
        let p = scientific :: Parser Scientific
            s = B8.pack $ showFFloatAlt Nothing (n :: Double) "err!"
        prs p s `shouldParse` fromFloatDigits n
        prs' p s `succeedsLeaving` "err!"
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (scientific :: Parser Scientific) ""
        `shouldFailWith` err 0 (ueof <> elabel "digit")

  describe "float" $ do
    context "when stream begins with a float"
      $ it "parses it"
      $ property
      $ \n' -> do
        let p = float :: Parser Double
            n = getNonNegative n'
            s = B8.pack (show n)
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when stream does not begin with a float"
      $ it "signals correct parse error"
      $ property
      $ \a as -> not (isDigit a) ==> do
        let p = float :: Parser Double
            s = B.pack (a : as)
        prs p s
          `shouldFailWith` err 0 (utok a <> elabel "digit")
        prs' p s `failsLeaving` s
    context "when stream begins with an integer (decimal)"
      $ it "signals correct parse error"
      $ property
      $ \n' -> do
        let p = float :: Parser Double
            n = getNonNegative n'
            s = B8.pack $ show (n :: Integer)
        prs p s
          `shouldFailWith` err
            (B.length s)
            (ueof <> etok 46 <> etok 69 <> etok 101 <> elabel "digit")
        prs' p s `failsLeaving` ""
    context "when number is followed by something starting with 'e'"
      $ it "parsing of exponent part is backtracked correctly"
      $ property
      $ \(NonNegative n) -> do
        let p = float :: Parser Double
            s = B8.pack $ showFFloatAlt Nothing (n :: Double) "err!"
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` "err!"
    context "when stream is empty"
      $ it "signals correct parse error"
      $ prs (float :: Parser Double) ""
        `shouldFailWith` err 0 (ueof <> elabel "digit")
    context "when there is float with just exponent"
      $ it "parses it all right"
      $ do
        let p = float :: Parser Double
        prs p "123e3" `shouldParse` 123e3
        prs' p "123e3" `succeedsLeaving` ""
        prs p "123e+3" `shouldParse` 123e+3
        prs' p "123e+3" `succeedsLeaving` ""
        prs p "123e-3" `shouldParse` 123e-3
        prs' p "123e-3" `succeedsLeaving` ""

  describe "signed" $ do
    context "with integer"
      $ it "parses signed integers"
      $ property
      $ \n -> do
        let p :: Parser Integer
            p = signed (hidden B.space) decimal
            s = B8.pack (show n)
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "with float"
      $ it "parses signed floats"
      $ property
      $ \n -> do
        let p :: Parser Double
            p = signed (hidden B.space) float
            s = B8.pack (show n)
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "with scientific"
      $ it "parses singed scientific numbers"
      $ property
      $ \n -> do
        let p = signed (hidden B.space) scientific
            s = B8.pack $ either show show (n :: Either Integer Double)
        prs p s `shouldParse` case n of
          Left x -> fromIntegral x
          Right x -> fromFloatDigits x
    context "when number is prefixed with plus sign"
      $ it "parses the number"
      $ property
      $ \n' -> do
        let p :: Parser Integer
            p = signed (hidden B.space) decimal
            n = getNonNegative n'
            s = B8.pack ('+' : show n)
        prs p s `shouldParse` n
        prs' p s `succeedsLeaving` ""
    context "when number is prefixed with white space"
      $ it "signals correct parse error"
      $ property
      $ \n -> do
        let p :: Parser Integer
            p = signed (hidden B.space) decimal
            s = B8.pack (' ' : show (n :: Integer))
        prs p s
          `shouldFailWith` err
            0
            (utok 32 <> etok 43 <> etok 45 <> elabel "integer")
        prs' p s `failsLeaving` s
    context "when there is white space between sign and digits"
      $ it "parses it all right"
      $ do
        let p :: Parser Integer
            p = signed (hidden B.space) decimal
            s = "- 123"
        prs p s `shouldParse` (-123)
        prs' p s `succeedsLeaving` ""

----------------------------------------------------------------------------
-- Helpers

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

isDigit :: Word8 -> Bool
isDigit w = w - 48 < 10

isOctDigit :: Word8 -> Bool
isOctDigit w = w - 48 < 8

isHexDigit :: Word8 -> Bool
isHexDigit w =
  (w >= 48 && w <= 57)
    || (w >= 97 && w <= 102)
    || (w >= 65 && w <= 70)
