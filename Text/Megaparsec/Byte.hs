-- |
-- Module      :  Text.Megaparsec.Byte
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used binary parsers.
--
-- @since 6.0.0

{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Byte
  ( -- * Simple parsers
    newline
  , crlf
  , eol
  , tab
  , space
  , space1
    -- * Categories of characters
  , controlChar
  , spaceChar
  , upperChar
  , lowerChar
  , letterChar
  , alphaNumChar
  , printChar
  , digitChar
  , octDigitChar
  , hexDigitChar
  , asciiChar
    -- * More general parsers
  , C.char
  , char'
  , C.anyChar
  , C.notChar
  , C.oneOf
  , C.noneOf
  , C.satisfy
    -- * Sequence of bytes
  , C.string
  , C.string' )
where

import Control.Applicative
import Data.Char
import Data.Proxy
import Data.Word (Word8)
import qualified Text.Megaparsec.Char as C

import Text.Megaparsec

----------------------------------------------------------------------------
-- Simple parsers

-- | Parse a newline byte.

newline :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
newline = C.char 10
{-# INLINE newline #-}

-- | Parse a carriage return character followed by a newline character.
-- Return the sequence of characters parsed.

crlf :: forall e s m. (MonadParsec e s m, Token s ~ Word8) => m (Tokens s)
crlf = C.string (tokensToChunk (Proxy :: Proxy s) [13,10])
{-# INLINE crlf #-}

-- | Parse a CRLF (see 'crlf') or LF (see 'newline') end of line. Return the
-- sequence of characters parsed.

eol :: forall e s m. (MonadParsec e s m, Token s ~ Word8) => m (Tokens s)
eol = (tokenToChunk (Proxy :: Proxy s) <$> newline)
  <|> crlf
  <?> "end of line"
{-# INLINE eol #-}

-- | Parse a tab character.

tab :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
tab = C.char 9
{-# INLINE tab #-}

-- | Skip /zero/ or more white space characters.
--
-- See also: 'skipMany' and 'spaceChar'.

space :: (MonadParsec e s m, Token s ~ Word8) => m ()
space = skipWhileP (Just "white space") isSpace'
{-# INLINE space #-}

-- | Skip /one/ or more white space characters.
--
-- See also: 'skipSome' and 'spaceChar'.

space1 :: (MonadParsec e s m, Token s ~ Word8) => m ()
space1 = skipWhile1P (Just "white space") isSpace'
{-# INLINE space1 #-}

----------------------------------------------------------------------------
-- Categories of characters

-- | Parse a control character (a non-printing character of the Latin-1
-- subset of Unicode).

controlChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
controlChar = C.satisfy (isControl . toChar) <?> "control character"
{-# INLINE controlChar #-}

-- | Parse a space character, and the control characters: tab, newline,
-- carriage return, form feed, and vertical tab.

spaceChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
spaceChar = C.satisfy isSpace' <?> "white space"
{-# INLINE spaceChar #-}

-- | Parse an upper-case character.

upperChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
upperChar = C.satisfy (isUpper . toChar) <?> "uppercase letter"
{-# INLINE upperChar #-}

-- | Parse a lower-case alphabetic character.

lowerChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
lowerChar = C.satisfy (isLower . toChar) <?> "lowercase letter"
{-# INLINE lowerChar #-}

-- | Parse an alphabetic character: lower-case or upper-case.

letterChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
letterChar = C.satisfy (isLetter . toChar) <?> "letter"
{-# INLINE letterChar #-}

-- | Parse an alphabetic or numeric digit Unicode characters.
--
-- Note that the numeric digits outside the ASCII range are parsed by this
-- parser but not by 'digitChar'. Such digits may be part of identifiers but
-- are not used by the printer and reader to represent numbers.

alphaNumChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
alphaNumChar = C.satisfy (isAlphaNum . toChar) <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

-- | Parse a printable character: letter, number, mark, punctuation, symbol
-- or space.

printChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
printChar = C.satisfy (isPrint . toChar) <?> "printable character"
{-# INLINE printChar #-}

-- | Parse an ASCII digit, i.e between “0” and “9”.

digitChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
digitChar = C.satisfy isDigit' <?> "digit"
  where
    isDigit' x = x >= 48 && x <= 57
{-# INLINE digitChar #-}

-- | Parse an octal digit, i.e. between “0” and “7”.

octDigitChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
octDigitChar = C.satisfy isOctDigit' <?> "octal digit"
  where
    isOctDigit' x = x >= 48 && x <= 55
{-# INLINE octDigitChar #-}

-- | Parse a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”, or
-- “A” and “F”.

hexDigitChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
hexDigitChar = C.satisfy (isHexDigit . toChar) <?> "hexadecimal digit"
{-# INLINE hexDigitChar #-}

-- | Parse a character from the first 128 characters of the Unicode
-- character set, corresponding to the ASCII character set.

asciiChar :: (MonadParsec e s m, Token s ~ Word8) => m (Token s)
asciiChar = C.satisfy (< 128) <?> "ASCII character"
{-# INLINE asciiChar #-}

----------------------------------------------------------------------------
-- More general parsers

-- | The same as 'char' but case-insensitive. This parser returns the
-- actually parsed character preserving its case.
--
-- >>> parseTest (char' 'e') "E"
-- 'E'
-- >>> parseTest (char' 'e') "G"
-- 1:1:
-- unexpected 'G'
-- expecting 'E' or 'e'

char' :: (MonadParsec e s m, Token s ~ Word8) => Token s -> m (Token s)
char' c = choice [C.char c, C.char (swapCase c)]
  where
    swapCase x
      | isUpper g = fromChar (toLower g)
      | isLower g = fromChar (toUpper g)
      | otherwise = x
      where
        g = toChar x
{-# INLINE char' #-}

----------------------------------------------------------------------------
-- Helpers

-- | 'Word8'-specialized version of 'isSpace'.

isSpace' :: Word8 -> Bool
isSpace' x
  | x >= 9 && x <= 13 = True
  | x == 32           = True
  | x == 160          = True
  | otherwise         = False
{-# INLINE isSpace' #-}

-- | Convert a byte to char.

toChar :: Word8 -> Char
toChar = chr . fromIntegral
{-# INLINE toChar #-}

-- | Convert a char to byte.

fromChar :: Char -> Word8
fromChar = fromIntegral . ord
{-# INLINE fromChar #-}
