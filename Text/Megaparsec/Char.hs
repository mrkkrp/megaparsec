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
    , endOfLine
    , tab
    , upper
    , lower
    , alphaNum
    , letter
    , digit
    , hexDigit
    , octDigit
    , char
    , anyChar
    , satisfy
    , string )
where

import Control.Applicative ((<|>))
import Data.Char

import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. See also
-- 'satisfy'.
--
-- > vowel = oneOf "aeiou" <?> "a vowel"

oneOf :: Stream s m Char => String -> ParsecT s u m Char
oneOf cs = satisfy (`elem` cs)

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.
--
-- > consonant = noneOf "aeiou" <?> "a consonant"

noneOf :: Stream s m Char => String -> ParsecT s u m Char
noneOf cs = satisfy (`notElem` cs)

-- | Skips /zero/ or more white space characters. See also 'skipMany'.

spaces :: Stream s m Char => ParsecT s u m ()
spaces = skipMany space

-- | Parses a white space character (any character which satisfies 'isSpace')
-- Returns the parsed character.

space :: Stream s m Char => ParsecT s u m Char
space = satisfy isSpace <?> "white space"

-- | Parses a newline character (\'\\n\'). Returns a newline character.

newline :: Stream s m Char => ParsecT s u m Char
newline = char '\n' <?> "lf newline"

-- | Parses a carriage return character (\'\\r\') followed by a newline
-- character (\'\\n\'). Returns a newline character.

crlf :: Stream s m Char => ParsecT s u m Char
crlf = char '\r' *> char '\n' <?> "crlf newline"

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end-of-line.
-- Returns a newline character (\'\\n\').
--
-- > endOfLine = newline <|> crlf

endOfLine :: Stream s m Char => ParsecT s u m Char
endOfLine = newline <|> crlf <?> "newline"

-- | Parses a tab character (\'\\t\').

tab :: Stream s m Char => ParsecT s u m Char
tab = char '\t' <?> "tab"

-- | Parses an upper case letter (a character between \'A\' and \'Z\').

upper :: Stream s m Char => ParsecT s u m Char
upper = satisfy isUpper <?> "uppercase letter"

-- | Parses a lower case character (a character between \'a\' and \'z\').

lower :: Stream s m Char => ParsecT s u m Char
lower = satisfy isLower <?> "lowercase letter"

-- | Parses a letter or digit (a character between \'0\' and \'9\').

alphaNum :: Stream s m Char => ParsecT s u m Char
alphaNum = satisfy isAlphaNum <?> "letter or digit"

-- | Parses a letter (an upper case or lower case character).

letter :: Stream s m Char => ParsecT s u m Char
letter = satisfy isAlpha <?> "letter"

-- | Parses a digit.

digit :: Stream s m Char => ParsecT s u m Char
digit = satisfy isDigit <?> "digit"

-- | Parses a hexadecimal digit (a digit or a letter between \'a\' and
-- \'f\' or \'A\' and \'F\').

hexDigit :: Stream s m Char => ParsecT s u m Char
hexDigit = satisfy isHexDigit <?> "hexadecimal digit"

-- | Parses an octal digit (a character between \'0\' and \'7\').

octDigit :: Stream s m Char => ParsecT s u m Char
octDigit = satisfy isOctDigit <?> "octal digit"

-- | @char c@ parses a single character @c@.
--
-- > semiColon = char ';'

char :: Stream s m Char => Char -> ParsecT s u m Char
char c = satisfy (== c) <?> show [c]

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: Stream s m Char => ParsecT s u m Char
anyChar = satisfy (const True)

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
