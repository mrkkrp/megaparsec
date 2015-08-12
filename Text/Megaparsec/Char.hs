-- |
-- Module      :  Text.Megaparsec.Char
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used character parsers.

module Text.Megaparsec.Char
    ( oneOf
    , noneOf
    , spaces
    , space
    , newline
    , crlf
    , eol
    , tab
    , letter
    , upper
    , lower
    , digit
    , hexDigit
    , octDigit
    , alphaNum
    , char
    , anyChar
    , satisfy
    , string )
where

import Control.Applicative ((<|>))
import Data.Char

import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- > vowel = oneOf "aeiou" <?> "vowel"

oneOf :: Stream s m Char => String -> ParsecT s u m Char
oneOf cs = satisfy (`elem` cs)

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- > consonant = noneOf "aeiou" <?> "consonant"

noneOf :: Stream s m Char => String -> ParsecT s u m Char
noneOf cs = satisfy (`notElem` cs)

-- | Skips /zero/ or more white space characters. See also 'skipMany'.

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany space

-- | Parses a white space character (any character which satisfies 'isSpace').
-- Returns the parsed character.

space :: Stream s m Char => ParsecT s u m Char
space = satisfy isSpace <?> "white space"

-- | Parses a newline character.

newline :: Stream s m Char => ParsecT s u m Char
newline = char '\n' <?> "newline"

-- | Parses a carriage return character followed by a newline
-- character. Returns sequence of characters parsed.

crlf :: Stream s m Char => ParsecT s u m String
crlf = string "\r\n"

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end of line.
-- Returns the sequence of characters parsed.
--
-- > eol = (pure <$> newline) <|> crlf

eol :: Stream s m Char => ParsecT s u m String
eol = (pure <$> newline) <|> crlf <?> "end of line"

-- | Parses a tab character.

tab :: Stream s m Char => ParsecT s u m Char
tab = char '\t' <?> "tab"

-- | Parses a letter (an upper case or lower case character).

letter :: Stream s m Char => ParsecT s u m Char
letter = satisfy isAlpha <?> "letter"

-- | Parses an upper case letter.

upper :: Stream s m Char => ParsecT s u m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parses a lower case character.

lower :: Stream s m Char => ParsecT s u m Char
lower = satisfy isLower <?> "lowercase letter"

-- | Parses a digit.

digit :: Stream s m Char => ParsecT s u m Char
digit = satisfy isDigit <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between “a” and
-- “f” or “A” and “F”).

hexDigit :: Stream s m Char => ParsecT s u m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

-- | Parses an octal digit (a character between “0” and “7”).

octDigit :: Stream s m Char => ParsecT s u m Char
octDigit = satisfy isOctDigit <?> "octal digit"

-- | Parses a letter or digit.

alphaNum :: Stream s m Char => ParsecT s u m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

-- | @char c@ parses a single character @c@.
--
-- > semicolon = char ';'

char :: Stream s m Char => Char -> ParsecT s u m Char
char c = satisfy (== c) <?> showToken c

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: Stream s m Char => ParsecT s u m Char
anyChar = satisfy (const True) <?> "character"

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- > digit    = satisfy isDigit
-- > oneOf cs = satisfy (`elem` cs)

satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
satisfy f = tokenPrim nextPos testChar
    where nextPos pos x _ = updatePosChar pos x
          testChar x      = if f x then Just x else Nothing

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- > divOrMod = string "div" <|> string "mod"

string :: Stream s m Char => String -> ParsecT s u m String
string = tokens updatePosString
