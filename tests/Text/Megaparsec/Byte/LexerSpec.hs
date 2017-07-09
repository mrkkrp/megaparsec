{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.Byte.LexerSpec (spec) where

import Control.Applicative
import Data.ByteString (ByteString)
import Data.Char (toUpper)
import Data.Monoid ((<>))
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Void
import Data.Word (Word8)
import Numeric (showInt, showHex, showOct, showFFloatAlt)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Byte.Lexer
import qualified Data.ByteString       as B
import qualified Data.ByteString.Char8 as B8
import qualified Text.Megaparsec.Byte  as B

type Parser = Parsec Void ByteString

spec :: Spec
spec = do

  describe "skipLineComment" $ do
    context "when there is no newline at the end of line" $
      it "is picked up successfully" $ do
        let p = space B.space1 (skipLineComment "//") empty <* eof
            s = "  // this line comment doesn't have a newline at the end "
        prs  p s `shouldParse` ()
        prs' p s `succeedsLeaving` ""
    it "inner characters are labelled properly" $ do
      let p = skipLineComment "//" <* empty
          s = "// here we go"
      prs  p s `shouldFailWith` err (posN (B.length s) s) (elabel "character")
      prs' p s `failsLeaving` ""

  describe "skipBlockComment" $
    it "skips a simple block comment" $ do
      let p = skipBlockComment "/*" "*/"
          s = "/* here we go */foo!"
      prs  p s `shouldParse` ()
      prs' p s `succeedsLeaving` "foo!"

  describe "skipBlockCommentNested" $
    context "when it runs into nested block comments" $
      it "parses them all right" $ do
        let p = space B.space1 empty
              (skipBlockCommentNested "/*" "*/") <* eof
            s = " /* foo bar /* baz */ quux */ "
        prs  p s `shouldParse` ()
        prs' p s `succeedsLeaving` ""

  describe "decimal" $ do
    context "when stream begins with decimal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = decimal :: Parser Integer
              n = getNonNegative n'
              s = B8.pack (showInt n "")
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with decimal digits" $
      it "signals correct parse error" $
        property $ \a as -> not (isDigit a) ==> do
          let p = decimal :: Parser Integer
              s = B.pack (a : as)
          prs  p s `shouldFailWith` err posI (utok a <> elabel "integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (decimal :: Parser Integer) "" `shouldFailWith`
          err posI (ueof <> elabel "integer")

  describe "octal" $ do
    context "when stream begins with octal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = octal :: Parser Integer
              n = getNonNegative n'
              s = B8.pack (showOct n "")
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with octal digits" $
      it "signals correct parse error" $
        property $ \a as -> not (isOctDigit a) ==> do
          let p = octal :: Parser Integer
              s = B.pack (a : as)
          prs  p s `shouldFailWith`
            err posI (utok a <> elabel "octal integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (octal :: Parser Integer) "" `shouldFailWith`
          err posI (ueof <> elabel "octal integer")

  describe "hexadecimal" $ do
    context "when stream begins with hexadecimal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = hexadecimal :: Parser Integer
              n = getNonNegative n'
              s = B8.pack (showHex n "")
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream begins with hexadecimal digits (uppercase)" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = hexadecimal :: Parser Integer
              n = getNonNegative n'
              s = B8.pack (toUpper <$> showHex n "")
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with hexadecimal digits" $
      it "signals correct parse error" $
        property $ \a as -> not (isHexDigit a) ==> do
          let p = hexadecimal :: Parser Integer
              s = B.pack (a : as)
          prs  p s `shouldFailWith`
            err posI (utok a <> elabel "hexadecimal integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (hexadecimal :: Parser Integer) "" `shouldFailWith`
          err posI (ueof <> elabel "hexadecimal integer")

  describe "float" $ do
    context "when stream begins with a float" $
      it "parses it" $
        property $ \n' -> do
          let p = float :: Parser Double
              n = getNonNegative n'
              s = B8.pack (show n)
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with a float" $
      it "signals correct parse error" $
        property $ \a as -> not (isDigit a) ==> do
          let p = float :: Parser Double
              s = B.pack (a : as)
          prs  p s `shouldFailWith`
            err posI (utok a <> elabel "floating point number")
          prs' p s `failsLeaving` s
    context "when stream begins with a decimal number" $
      it "parses it" $
        property $ \n' -> do
          let p = float :: Parser Double
              n = getNonNegative n'
              s = B8.pack $ show (n :: Integer)
          prs  p s `shouldParse` fromIntegral n
          prs' p s `succeedsLeaving` ""
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (float :: Parser Double) "" `shouldFailWith`
          err posI (ueof <> elabel "floating point number")
    context "when there is float with exponent without explicit sign" $
      it "parses it all right" $ do
        let p = float :: Parser Double
            s = "123e3"
        prs  p s `shouldParse` 123e3
        prs' p s `succeedsLeaving` ""

  describe "scientific" $ do
    context "when stream begins with a number" $
      it "parses it" $
        property $ \n' -> do
          let p = scientific :: Parser Scientific
              s = B8.pack $ either (show . getNonNegative) (show . getNonNegative)
                (n' :: Either (NonNegative Integer) (NonNegative Double))
          prs p s `shouldParse` case n' of
            Left  x -> fromIntegral    (getNonNegative x)
            Right x -> fromFloatDigits (getNonNegative x)
          prs' p s `succeedsLeaving` ""
    context "when fractional part is interrupted" $
      it "signals correct parse error" $
        property $ \(NonNegative n) -> do
          let p = scientific <* empty :: Parser Scientific
              s = B8.pack (showFFloatAlt Nothing (n :: Double) "")
          prs p s `shouldFailWith` err (posN (B.length s) s)
            (etok 69 <> etok 101 <> elabel "digit")
          prs' p s `failsLeaving` ""
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (scientific :: Parser Scientific) "" `shouldFailWith`
          err posI (ueof <> elabel "digit")

  describe "signed" $ do
    context "with integer" $
      it "parses signed integers" $
        property $ \n -> do
          let p :: Parser Integer
              p = signed (hidden B.space) decimal
              s = B8.pack (show n)
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "with float" $
      it "parses signed floats" $
        property $ \n -> do
          let p :: Parser Double
              p = signed (hidden B.space) float
              s = B8.pack (show n)
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "with scientific" $
      it "parses singed scientific numbers" $
        property $ \n -> do
          let p = signed (hidden B.space) scientific
              s = B8.pack $ either show show (n :: Either Integer Double)
          prs p s `shouldParse` case n of
            Left  x -> fromIntegral    x
            Right x -> fromFloatDigits x
    context "when number is prefixed with plus sign" $
      it "parses the number" $
        property $ \n' -> do
          let p :: Parser Integer
              p = signed (hidden B.space) decimal
              n = getNonNegative n'
              s = B8.pack ('+' : show n)
          prs  p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when number is prefixed with white space" $
      it "signals correct parse error" $
        property $ \n -> do
          let p :: Parser Integer
              p = signed (hidden B.space) decimal
              s = B8.pack (' ' : show (n :: Integer))
          prs  p s `shouldFailWith` err posI
            (utok 32 <> etok 43 <> etok 45 <> elabel "integer")
          prs' p s `failsLeaving` s
    context "when there is white space between sign and digits" $
      it "parses it all right" $ do
        let p :: Parser Integer
            p = signed (hidden B.space) decimal
            s = "- 123"
        prs  p s `shouldParse` (-123)
        prs' p s `succeedsLeaving` ""

----------------------------------------------------------------------------
-- Helpers

prs
  :: Parser a          -- ^ Parser to run
  -> ByteString        -- ^ Input for the parser
  -> Either (ParseError Word8 Void) a -- ^ Result of parsing
prs p = parse p ""

prs'
  :: Parser a          -- ^ Parser to run
  -> ByteString        -- ^ Input for the parser
  -> (State ByteString, Either (ParseError Word8 Void) a) -- ^ Result of parsing
prs' p s = runParser' p (initialState s)

isDigit :: Word8 -> Bool
isDigit w = w - 48 < 10

isOctDigit :: Word8 -> Bool
isOctDigit w = w - 48 < 8

isHexDigit :: Word8 -> Bool
isHexDigit w =
  (w >= 48 && w <= 57)  ||
  (w >= 97 && w <= 102) ||
  (w >= 65 && w <= 70)
