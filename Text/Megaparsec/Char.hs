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
  ( newline
  , crlf
  , eol
  , tab
  , space
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
  , markChar
  , numberChar
  , punctuationChar
  , symbolChar
  , separatorChar
  , asciiChar
  , latin1Char
  , charCategory
  , categoryName
  , char
  , anyChar
  , oneOf
  , noneOf
  , satisfy
  , string )
where

import Control.Applicative ((<|>))
import Data.Char
import Data.Maybe (fromJust)

import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken

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

-- | Skips /zero/ or more white space characters. See also 'skipMany' and
-- 'spaceChar'.

space :: Stream s m Char => ParsecT s u m ()
space = skipMany spaceChar

-- | Parses control characters, which are the non-printing characters of the
-- Latin-1 subset of Unicode.

controlChar :: Stream s m Char => ParsecT s u m Char
controlChar = satisfy isControl <?> "control character"

-- | Parses a Unicode space character, and the control characters: tab,
-- newline, carriage return, form feed, and vertical tab.

spaceChar :: Stream s m Char => ParsecT s u m Char
spaceChar = satisfy isSpace <?> "white space"

-- | Parses an upper-case or title-case alphabetic Unicode character. Title
-- case is used by a small number of letter ligatures like the
-- single-character form of Lj.

upperChar :: Stream s m Char => ParsecT s u m Char
upperChar = satisfy isUpper <?> "uppercase letter"

-- | Parses a lower-case alphabetic Unicode character.

lowerChar :: Stream s m Char => ParsecT s u m Char
lowerChar = satisfy isLower <?> "lowercase letter"

-- | Parses alphabetic Unicode characters: lower-case, upper-case and
-- title-case letters, plus letters of case-less scripts and modifiers
-- letters.

letterChar :: Stream s m Char => ParsecT s u m Char
letterChar = satisfy isLetter <?> "letter"

-- | Parses alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are parsed by this
-- parser but not by 'digitChar'. Such digits may be part of identifiers but
-- are not used by the printer and reader to represent numbers.

alphaNumChar :: Stream s m Char => ParsecT s u m Char
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"

-- | Parses printable Unicode characters: letters, numbers, marks,
-- punctuation, symbols and spaces.

printChar :: Stream s m Char => ParsecT s u m Char
printChar = satisfy isPrint <?> "printable character"

-- | Parses an ASCII digit, i.e between “0” and “9”.

digitChar :: Stream s m Char => ParsecT s u m Char
digitChar = satisfy isDigit <?> "digit"

-- | Parses an octal digit, i.e. between “0” and “7”.

octDigitChar :: Stream s m Char => ParsecT s u m Char
octDigitChar = satisfy isOctDigit <?> "octal digit"

-- | Parses a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”,
-- or “A” and “F”.

hexDigitChar :: Stream s m Char => ParsecT s u m Char
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"

-- | Parses Unicode mark characters, for example accents and the like, which
-- combine with preceding characters.

markChar :: Stream s m Char => ParsecT s u m Char
markChar = satisfy isMark <?> "mark character"

-- | Parses Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.

numberChar :: Stream s m Char => ParsecT s u m Char
numberChar = satisfy isNumber <?> "numeric character"

-- | Parses Unicode punctuation characters, including various kinds of
-- connectors, brackets and quotes.

punctuationChar :: Stream s m Char => ParsecT s u m Char
punctuationChar = satisfy isPunctuation <?> "punctuation"

-- | Parses Unicode symbol characters, including mathematical and currency
-- symbols.

symbolChar :: Stream s m Char => ParsecT s u m Char
symbolChar = satisfy isSymbol <?> "symbol"

-- | Parses Unicode space and separator characters.

separatorChar :: Stream s m Char => ParsecT s u m Char
separatorChar = satisfy isSeparator <?> "separator"

-- | Parses a character from the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.

asciiChar :: Stream s m Char => ParsecT s u m Char
asciiChar = satisfy isAscii <?> "ASCII character"

-- | Parses a character from the first 256 characters of the Unicode
-- character set, corresponding to the ISO 8859-1 (Latin-1) character set.

latin1Char :: Stream s m Char => ParsecT s u m Char
latin1Char = satisfy isLatin1 <?> "Latin-1 character"

-- | @charCategory cat@ Parses character in Unicode General Category @cat@,
-- see 'Data.Char.GeneralCategory'.

charCategory :: Stream s m Char => GeneralCategory -> ParsecT s u m Char
charCategory cat = satisfy ((== cat) . generalCategory) <?> categoryName cat

-- | Returns human-readable name of Unicode General Category.

categoryName :: GeneralCategory -> String
categoryName cat =
  fromJust $ lookup cat
  [ (UppercaseLetter     , "uppercase letter")
  , (LowercaseLetter     , "lowercase letter")
  , (TitlecaseLetter     , "titlecase letter")
  , (ModifierLetter      , "modifier letter")
  , (OtherLetter         , "other letter")
  , (NonSpacingMark      , "non-spacing mark")
  , (SpacingCombiningMark, "spacing combining mark")
  , (EnclosingMark       , "enclosing mark")
  , (DecimalNumber       , "decimal number character")
  , (LetterNumber        , "letter number character")
  , (OtherNumber         , "other number character")
  , (ConnectorPunctuation, "connector punctuation")
  , (DashPunctuation     , "dash punctuation")
  , (OpenPunctuation     , "open punctuation")
  , (ClosePunctuation    , "close punctuation")
  , (InitialQuote        , "initial quote")
  , (FinalQuote          , "final quote")
  , (OtherPunctuation    , "other punctuation")
  , (MathSymbol          , "math symbol")
  , (CurrencySymbol      , "currency symbol")
  , (ModifierSymbol      , "modifier symbol")
  , (OtherSymbol         , "other symbol")
  , (Space               , "white space")
  , (LineSeparator       , "line separator")
  , (ParagraphSeparator  , "paragraph separator")
  , (Control             , "control character")
  , (Format              , "format character")
  , (Surrogate           , "surrogate character")
  , (PrivateUse          , "private-use Unicode character")
  , (NotAssigned         , "non-assigned Unicode character") ]

-- | @char c@ parses a single character @c@.
--
-- > semicolon = char ';'

char :: Stream s m Char => Char -> ParsecT s u m Char
char c = satisfy (== c) <?> showToken c

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: Stream s m Char => ParsecT s u m Char
anyChar = satisfy (const True) <?> "character"

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

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- > digit    = satisfy isDigit
-- > oneOf cs = satisfy (`elem` cs)

satisfy :: Stream s m Char => (Char -> Bool) -> ParsecT s u m Char
satisfy f = token nextPos testChar
  where nextPos pos x _ = updatePosChar pos x
        testChar x      = if f x then Just x else Nothing

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- > divOrMod = string "div" <|> string "mod"

string :: Stream s m Char => String -> ParsecT s u m String
string = tokens updatePosString
