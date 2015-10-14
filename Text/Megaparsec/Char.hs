-- |
-- Module      :  Text.Megaparsec.Char
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Commonly used character parsers.

module Text.Megaparsec.Char
  ( -- * Simple parsers
    newline
  , crlf
  , eol
  , tab
  , space
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
  , markChar
  , numberChar
  , punctuationChar
  , symbolChar
  , separatorChar
  , asciiChar
  , latin1Char
  , charCategory
  , categoryName
    -- * More general parsers
  , char
  , char'
  , anyChar
  , oneOf
  , oneOf'
  , noneOf
  , noneOf'
  , satisfy
    -- * Sequence of characters
  , string
  , string' )
where

import Control.Applicative ((<|>))
import Data.Char
import Data.List (nub)
import Data.Maybe (fromJust)

import Text.Megaparsec.Combinator
import Text.Megaparsec.Error (Message (..))
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
#endif

-- | Parses a newline character.

newline :: MonadParsec s m Char => m Char
newline = char '\n' <?> "newline"

-- | Parses a carriage return character followed by a newline
-- character. Returns sequence of characters parsed.

crlf :: MonadParsec s m Char => m String
crlf = string "\r\n"

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end of line.
-- Returns the sequence of characters parsed.
--
-- > eol = (pure <$> newline) <|> crlf

eol :: MonadParsec s m Char => m String
eol = (pure <$> newline) <|> crlf <?> "end of line"

-- | Parses a tab character.

tab :: MonadParsec s m Char => m Char
tab = char '\t' <?> "tab"

-- | Skips /zero/ or more white space characters.
--
-- See also: 'skipMany' and 'spaceChar'.

space :: MonadParsec s m Char => m ()
space = skipMany spaceChar

-- | Parses control characters, which are the non-printing characters of the
-- Latin-1 subset of Unicode.

controlChar :: MonadParsec s m Char => m Char
controlChar = satisfy isControl <?> "control character"

-- | Parses a Unicode space character, and the control characters: tab,
-- newline, carriage return, form feed, and vertical tab.

spaceChar :: MonadParsec s m Char => m Char
spaceChar = satisfy isSpace <?> "white space"

-- | Parses an upper-case or title-case alphabetic Unicode character. Title
-- case is used by a small number of letter ligatures like the
-- single-character form of Lj.

upperChar :: MonadParsec s m Char => m Char
upperChar = satisfy isUpper <?> "uppercase letter"

-- | Parses a lower-case alphabetic Unicode character.

lowerChar :: MonadParsec s m Char => m Char
lowerChar = satisfy isLower <?> "lowercase letter"

-- | Parses alphabetic Unicode characters: lower-case, upper-case and
-- title-case letters, plus letters of case-less scripts and modifiers
-- letters.

letterChar :: MonadParsec s m Char => m Char
letterChar = satisfy isLetter <?> "letter"

-- | Parses alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are parsed by this
-- parser but not by 'digitChar'. Such digits may be part of identifiers but
-- are not used by the printer and reader to represent numbers.

alphaNumChar :: MonadParsec s m Char => m Char
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"

-- | Parses printable Unicode characters: letters, numbers, marks,
-- punctuation, symbols and spaces.

printChar :: MonadParsec s m Char => m Char
printChar = satisfy isPrint <?> "printable character"

-- | Parses an ASCII digit, i.e between “0” and “9”.

digitChar :: MonadParsec s m Char => m Char
digitChar = satisfy isDigit <?> "digit"

-- | Parses an octal digit, i.e. between “0” and “7”.

octDigitChar :: MonadParsec s m Char => m Char
octDigitChar = satisfy isOctDigit <?> "octal digit"

-- | Parses a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”,
-- or “A” and “F”.

hexDigitChar :: MonadParsec s m Char => m Char
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"

-- | Parses Unicode mark characters, for example accents and the like, which
-- combine with preceding characters.

markChar :: MonadParsec s m Char => m Char
markChar = satisfy isMark <?> "mark character"

-- | Parses Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.

numberChar :: MonadParsec s m Char => m Char
numberChar = satisfy isNumber <?> "numeric character"

-- | Parses Unicode punctuation characters, including various kinds of
-- connectors, brackets and quotes.

punctuationChar :: MonadParsec s m Char => m Char
punctuationChar = satisfy isPunctuation <?> "punctuation"

-- | Parses Unicode symbol characters, including mathematical and currency
-- symbols.

symbolChar :: MonadParsec s m Char => m Char
symbolChar = satisfy isSymbol <?> "symbol"

-- | Parses Unicode space and separator characters.

separatorChar :: MonadParsec s m Char => m Char
separatorChar = satisfy isSeparator <?> "separator"

-- | Parses a character from the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.

asciiChar :: MonadParsec s m Char => m Char
asciiChar = satisfy isAscii <?> "ASCII character"

-- | Parses a character from the first 256 characters of the Unicode
-- character set, corresponding to the ISO 8859-1 (Latin-1) character set.

latin1Char :: MonadParsec s m Char => m Char
latin1Char = satisfy isLatin1 <?> "Latin-1 character"

-- | @charCategory cat@ Parses character in Unicode General Category @cat@,
-- see 'Data.Char.GeneralCategory'.

charCategory :: MonadParsec s m Char => GeneralCategory -> m Char
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

char :: MonadParsec s m Char => Char -> m Char
char c = satisfy (== c) <?> showToken c

-- | The same as 'char' but case-insensitive. This parser returns actually
-- parsed character preserving its case.
--
-- >>> parseTest (char' 'e') "E"
-- 'E'
-- >>> parseTest (char' 'e') "G"
-- 1:1:
--   unexpected 'G'
--   expecting 'E' or 'e'

char' :: MonadParsec s m Char => Char -> m Char
char' = choice . fmap char . extendi . pure

-- | Extends given list of characters adding uppercase version of every
-- lowercase characters and vice versa. Resulting list is guaranteed to have
-- no duplicates.

extendi :: String -> String
extendi cs = nub (cs >>= f)
  where f c | isLower c = [c, toUpper c]
            | isUpper c = [c, toLower c]
            | otherwise = [c]

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: MonadParsec s m Char => m Char
anyChar = satisfy (const True) <?> "character"

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. Note that this
-- parser doesn't automatically generate “expected” component of error
-- message, so usually you should label it manually with 'label' or
-- ('<?>').
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"

oneOf :: MonadParsec s m Char => String -> m Char
oneOf cs = satisfy (`elem` cs)

-- | The same as 'oneOf', but case-insensitive. Returns the parsed character
-- preserving its case.
--
-- > vowel = oneOf' "aeiou" <?> "vowel"

oneOf' :: MonadParsec s m Char => String -> m Char
oneOf' = oneOf . extendi

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.

noneOf :: MonadParsec s m Char => String -> m Char
noneOf cs = satisfy (`notElem` cs)

-- | The same as 'noneOf', but case-insensitive.
--
-- > consonant = noneOf' "aeiou" <?> "consonant"

noneOf' :: MonadParsec s m Char => String -> m Char
noneOf' = noneOf . extendi

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)

satisfy :: MonadParsec s m Char => (Char -> Bool) -> m Char
satisfy f = token updatePosChar testChar
  where testChar x = if f x
                     then Right x
                     else Left . pure . Unexpected . showToken $ x

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- > divOrMod = string "div" <|> string "mod"

string :: MonadParsec s m Char => String -> m String
string = tokens updatePosString (==)

-- | The same as 'string', but case-insensitive. On success returns string
-- cased as actually parsed input.
--
-- >>> parseTest (string' "foobar") "foObAr"
-- "foObAr"

string' :: MonadParsec s m Char => String -> m String
string' = tokens updatePosString test
  where test x y = toLower x == toLower y
