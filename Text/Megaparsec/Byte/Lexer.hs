-- |
-- Module      :  Text.Megaparsec.Byte.Lexer
-- Copyright   :  © 2015–2018 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Stripped-down version of "Text.Megaparsec.Char.Lexer" for streams of
-- bytes.
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Megaparsec.Byte.Lexer as L

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Byte.Lexer
  ( -- * White space
    C.space
  , C.lexeme
  , C.symbol
  , C.symbol'
  , skipLineComment
  , skipBlockComment
  , skipBlockCommentNested
    -- * Numbers
  , decimal
  , binary
  , octal
  , hexadecimal
  , scientific
  , float
  , signed )
where

import Control.Applicative
import Data.Functor (void)
import Data.List (foldl')
import Data.Proxy
import Data.Scientific (Scientific)
import Data.Word (Word8)
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Data.Scientific            as Sci
import qualified Text.Megaparsec.Char.Lexer as C

----------------------------------------------------------------------------
-- White space

-- | Given comment prefix this function returns a parser that skips line
-- comments. Note that it stops just before the newline character but
-- doesn't consume the newline. Newline is either supposed to be consumed by
-- 'space' parser or picked up manually.

skipLineComment :: (MonadParsec e s m, Token s ~ Word8)
  => Tokens s          -- ^ Line comment prefix
  -> m ()
skipLineComment prefix =
  string prefix *> void (takeWhileP (Just "character") (/= 10))
{-# INLINEABLE skipLineComment #-}

-- | @'skipBlockComment' start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: (MonadParsec e s m, Token s ~ Word8)
  => Tokens s          -- ^ Start of block comment
  -> Tokens s          -- ^ End of block comment
  -> m ()
skipBlockComment start end = p >> void (manyTill anySingle n)
  where
    p = string start
    n = string end
{-# INLINEABLE skipBlockComment #-}

-- | @'skipBlockCommentNested' start end@ skips possibly nested block
-- comment starting with @start@ and ending with @end@.
--
-- @since 5.0.0

skipBlockCommentNested :: (MonadParsec e s m, Token s ~ Word8)
  => Tokens s          -- ^ Start of block comment
  -> Tokens s          -- ^ End of block comment
  -> m ()
skipBlockCommentNested start end = p >> void (manyTill e n)
  where
    e = skipBlockCommentNested start end <|> void anySingle
    p = string start
    n = string end
{-# INLINEABLE skipBlockCommentNested #-}

----------------------------------------------------------------------------
-- Numbers

-- | Parse an integer in decimal representation according to the format of
-- integer literals described in the Haskell report.
--
-- If you need to parse signed integers, see 'signed' combinator.

decimal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Word8, Integral a)
  => m a
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.

decimal_
  :: forall e s m a. (MonadParsec e s m, Token s ~ Word8, Integral a)
  => m a
decimal_ = mkNum <$> takeWhile1P (Just "digit") isDigit
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a w = a * 10 + fromIntegral (w - 48)
{-# INLINE decimal_ #-}

-- | Parse an integer in binary representation. Representation of binary
-- number is expected to follow the rule @0 (b | B) (0 | 1) {0 | 1}@ except
-- for the fact that this parser doesn't parse “0b” or “0B” prefix. It is
-- a responsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to the rule mentioned above like this:
--
-- > binary = char '0' >> char' 'b' >> L.binary

binary
  :: forall e s m a. (MonadParsec e s m, Token s ~ Word8, Integral a)
  => m a
binary = mkNum
  <$> takeWhile1P Nothing isBinDigit
  <?> "binary integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a w     = a * 2 + fromIntegral (w - 48)
    isBinDigit w = w - 48 < 2
{-# INLINEABLE binary #-}

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to the Haskell report except for the
-- fact that this parser doesn't parse “0o” or “0O” prefix. It is a
-- responsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > octal = char '0' >> char' 'o' >> L.octal

octal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Word8, Integral a)
  => m a
octal = mkNum
  <$> takeWhile1P Nothing isOctDigit
  <?> "octal integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a w     = a * 8 + fromIntegral (w - 48)
    isOctDigit w = w - 48 < 8
{-# INLINEABLE octal #-}

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to the Haskell report
-- except for the fact that this parser doesn't parse “0x” or “0X” prefix.
-- It is a responsibility of the programmer to parse correct prefix before
-- parsing the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

hexadecimal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Word8, Integral a)
  => m a
hexadecimal = mkNum
  <$> takeWhile1P Nothing isHexDigit
  <?> "hexadecimal integer"
  where
    mkNum        = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a w
      | w >= 48 && w <= 57 = a * 16 + fromIntegral (w - 48)
      | w >= 97            = a * 16 + fromIntegral (w - 87)
      | otherwise          = a * 16 + fromIntegral (w - 55)
    isHexDigit w =
      (w >= 48 && w <= 57)  ||
      (w >= 97 && w <= 102) ||
      (w >= 65 && w <= 70)
{-# INLINEABLE hexadecimal #-}

-- | Parse a floating point value as a 'Scientific' number. 'Scientific' is
-- great for parsing of arbitrary precision numbers coming from an untrusted
-- source. See documentation in "Data.Scientific" for more information.
--
-- The parser can be used to parse integers or floating point values. Use
-- functions like 'Data.Scientific.floatingOrInteger' from "Data.Scientific"
-- to test and extract integer or real values.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.

scientific
  :: forall e s m. (MonadParsec e s m, Token s ~ Word8)
  => m Scientific
scientific = do
  c'      <- decimal_
  SP c e' <- option (SP c' 0) (try $ dotDecimal_ (Proxy :: Proxy s) c')
  e       <- option e' (try $ exponent_ e')
  return (Sci.scientific c e)
{-# INLINEABLE scientific #-}

data SP = SP !Integer {-# UNPACK #-} !Int

-- | Parse a floating point number according to the syntax for floating
-- point literals described in the Haskell report.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.
--
-- __Note__: in versions 6.0.0–6.1.1 this function accepted plain integers.

float :: (MonadParsec e s m, Token s ~ Word8, RealFloat a) => m a
float = do
  c' <- decimal_
  Sci.toRealFloat <$>
    ((do SP c e' <- dotDecimal_ (Proxy :: Proxy s) c'
         e       <- option e' (try $ exponent_ e')
         return (Sci.scientific c e))
     <|> (Sci.scientific c' <$> exponent_ 0))
{-# INLINEABLE float #-}

dotDecimal_ :: (MonadParsec e s m, Token s ~ Word8)
  => Proxy s
  -> Integer
  -> m SP
dotDecimal_ pxy c' = do
  void (char 46)
  let mkNum    = foldl' step (SP c' 0) . chunkToTokens pxy
      step (SP a e') w = SP
        (a * 10 + fromIntegral (w - 48))
        (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") isDigit
{-# INLINE dotDecimal_ #-}

exponent_ :: (MonadParsec e s m, Token s ~ Word8)
  => Int
  -> m Int
exponent_ e' = do
  void (char' 101)
  (+ e') <$> signed (return ()) decimal_
{-# INLINE exponent_ #-}

-- | @'signed' space p@ parser parses an optional sign character (“+” or
-- “-”), then if there is a sign it consumes optional white space (using
-- @space@ parser), then it runs parser @p@ which should return a number.
-- Sign of the number is changed according to the previously parsed sign
-- character.
--
-- For example, to parse signed integer you can write:
--
-- > lexeme        = L.lexeme spaceConsumer
-- > integer       = lexeme L.decimal
-- > signedInteger = L.signed spaceConsumer integer

signed :: (MonadParsec e s m, Token s ~ Word8, Num a)
  => m ()              -- ^ How to consume white space after the sign
  -> m a               -- ^ How to parse the number itself
  -> m a               -- ^ Parser for signed numbers
signed spc p = option id (C.lexeme spc sign) <*> p
  where
    sign = (id <$ char 43) <|> (negate <$ char 45)
{-# INLINEABLE signed #-}

----------------------------------------------------------------------------
-- Helpers

-- | A fast predicate to check if given 'Word8' is a digit in ASCII.

isDigit :: Word8 -> Bool
isDigit w = w - 48 < 10
{-# INLINE isDigit #-}
