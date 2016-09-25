-- |
-- Module      :  Text.Megaparsec.Char
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Commonly used character parsers.

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import qualified Data.Set as E

import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Prim

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), pure)
import Data.Foldable (Foldable (), any, elem, notElem)
import Prelude hiding (any, elem, notElem)
#endif

----------------------------------------------------------------------------
-- Simple parsers

-- | Parses a newline character.

newline :: (MonadParsec e s m, Token s ~ Char) => m Char
newline = char '\n'
{-# INLINE newline #-}

-- | Parses a carriage return character followed by a newline character.
-- Returns sequence of characters parsed.

crlf :: (MonadParsec e s m, Token s ~ Char) => m String
crlf = string "\r\n"
{-# INLINE crlf #-}

-- | Parses a CRLF (see 'crlf') or LF (see 'newline') end of line. Returns
-- the sequence of characters parsed.
--
-- > eol = (pure <$> newline) <|> crlf

eol :: (MonadParsec e s m, Token s ~ Char) => m String
eol = (pure <$> newline) <|> crlf <?> "end of line"
{-# INLINE eol #-}

-- | Parses a tab character.

tab :: (MonadParsec e s m, Token s ~ Char) => m Char
tab = char '\t'
{-# INLINE tab #-}

-- | Skips /zero/ or more white space characters.
--
-- See also: 'skipMany' and 'spaceChar'.

space :: (MonadParsec e s m, Token s ~ Char) => m ()
space = skipMany spaceChar
{-# INLINE space #-}

----------------------------------------------------------------------------
-- Categories of characters

-- | Parses control characters, which are the non-printing characters of the
-- Latin-1 subset of Unicode.

controlChar :: (MonadParsec e s m, Token s ~ Char) => m Char
controlChar = satisfy isControl <?> "control character"
{-# INLINE controlChar #-}

-- | Parses a Unicode space character, and the control characters: tab,
-- newline, carriage return, form feed, and vertical tab.

spaceChar :: (MonadParsec e s m, Token s ~ Char) => m Char
spaceChar = satisfy isSpace <?> "white space"
{-# INLINE spaceChar #-}

-- | Parses an upper-case or title-case alphabetic Unicode character. Title
-- case is used by a small number of letter ligatures like the
-- single-character form of Lj.

upperChar :: (MonadParsec e s m, Token s ~ Char) => m Char
upperChar = satisfy isUpper <?> "uppercase letter"
{-# INLINE upperChar #-}

-- | Parses a lower-case alphabetic Unicode character.

lowerChar :: (MonadParsec e s m, Token s ~ Char) => m Char
lowerChar = satisfy isLower <?> "lowercase letter"
{-# INLINE lowerChar #-}

-- | Parses alphabetic Unicode characters: lower-case, upper-case and
-- title-case letters, plus letters of case-less scripts and modifiers
-- letters.

letterChar :: (MonadParsec e s m, Token s ~ Char) => m Char
letterChar = satisfy isLetter <?> "letter"
{-# INLINE letterChar #-}

-- | Parses alphabetic or numeric digit Unicode characters.
--
-- Note that numeric digits outside the ASCII range are parsed by this
-- parser but not by 'digitChar'. Such digits may be part of identifiers but
-- are not used by the printer and reader to represent numbers.

alphaNumChar :: (MonadParsec e s m, Token s ~ Char) => m Char
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

-- | Parses printable Unicode characters: letters, numbers, marks,
-- punctuation, symbols and spaces.

printChar :: (MonadParsec e s m, Token s ~ Char) => m Char
printChar = satisfy isPrint <?> "printable character"
{-# INLINE printChar #-}

-- | Parses an ASCII digit, i.e between “0” and “9”.

digitChar :: (MonadParsec e s m, Token s ~ Char) => m Char
digitChar = satisfy isDigit <?> "digit"
{-# INLINE digitChar #-}

-- | Parses an octal digit, i.e. between “0” and “7”.

octDigitChar :: (MonadParsec e s m, Token s ~ Char) => m Char
octDigitChar = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigitChar #-}

-- | Parses a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”,
-- or “A” and “F”.

hexDigitChar :: (MonadParsec e s m, Token s ~ Char) => m Char
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigitChar #-}

-- | Parses Unicode mark characters, for example accents and the like, which
-- combine with preceding characters.

markChar :: (MonadParsec e s m, Token s ~ Char) => m Char
markChar = satisfy isMark <?> "mark character"
{-# INLINE markChar #-}

-- | Parses Unicode numeric characters, including digits from various
-- scripts, Roman numerals, et cetera.

numberChar :: (MonadParsec e s m, Token s ~ Char) => m Char
numberChar = satisfy isNumber <?> "numeric character"
{-# INLINE numberChar #-}

-- | Parses Unicode punctuation characters, including various kinds of
-- connectors, brackets and quotes.

punctuationChar :: (MonadParsec e s m, Token s ~ Char) => m Char
punctuationChar = satisfy isPunctuation <?> "punctuation"
{-# INLINE punctuationChar #-}

-- | Parses Unicode symbol characters, including mathematical and currency
-- symbols.

symbolChar :: (MonadParsec e s m, Token s ~ Char) => m Char
symbolChar = satisfy isSymbol <?> "symbol"
{-# INLINE symbolChar #-}

-- | Parses Unicode space and separator characters.

separatorChar :: (MonadParsec e s m, Token s ~ Char) => m Char
separatorChar = satisfy isSeparator <?> "separator"
{-# INLINE separatorChar #-}

-- | Parses a character from the first 128 characters of the Unicode character set,
-- corresponding to the ASCII character set.

asciiChar :: (MonadParsec e s m, Token s ~ Char) => m Char
asciiChar = satisfy isAscii <?> "ASCII character"
{-# INLINE asciiChar #-}

-- | Parses a character from the first 256 characters of the Unicode
-- character set, corresponding to the ISO 8859-1 (Latin-1) character set.

latin1Char :: (MonadParsec e s m, Token s ~ Char) => m Char
latin1Char = satisfy isLatin1 <?> "Latin-1 character"
{-# INLINE latin1Char #-}

-- | @charCategory cat@ Parses character in Unicode General Category @cat@,
-- see 'Data.Char.GeneralCategory'.

charCategory :: (MonadParsec e s m, Token s ~ Char) => GeneralCategory -> m Char
charCategory cat = satisfy ((== cat) . generalCategory) <?> categoryName cat
{-# INLINE charCategory #-}

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

----------------------------------------------------------------------------
-- More general parsers

-- | @char c@ parses a single character @c@.
--
-- > semicolon = char ';'

char :: (MonadParsec e s m, Token s ~ Char) => Char -> m Char
char c = token testChar (Just c)
  where
    f x = E.singleton (Tokens (x:|[]))
    testChar x =
      if x == c
        then Right x
        else Left (f x, f c, E.empty)
{-# INLINE char #-}

-- | The same as 'char' but case-insensitive. This parser returns actually
-- parsed character preserving its case.
--
-- >>> parseTest (char' 'e') "E"
-- 'E'
-- >>> parseTest (char' 'e') "G"
-- 1:1:
-- unexpected 'G'
-- expecting 'E' or 'e'

char' :: (MonadParsec e s m, Token s ~ Char) => Char -> m Char
char' c = choice [char c, char $ swapCase c]
  where
    swapCase x
      | isUpper x = toLower x
      | isLower x = toUpper x
      | otherwise = x
{-# INLINE char' #-}

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: (MonadParsec e s m, Token s ~ Char) => m Char
anyChar = satisfy (const True) <?> "character"
{-# INLINE anyChar #-}

-- | @oneOf cs@ succeeds if the current character is in the supplied
-- list of characters @cs@. Returns the parsed character. Note that this
-- parser doesn't automatically generate “expected” component of error
-- message, so usually you should label it manually with 'label' or
-- ('<?>').
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"

oneOf :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
oneOf cs = satisfy (`elem` cs)
{-# INLINE oneOf #-}

-- | The same as 'oneOf', but case-insensitive. Returns the parsed character
-- preserving its case.
--
-- > vowel = oneOf' "aeiou" <?> "vowel"

oneOf' :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
oneOf' cs = satisfy (`elemi` cs)
{-# INLINE oneOf' #-}

-- | As the dual of 'oneOf', @noneOf cs@ succeeds if the current
-- character /not/ in the supplied list of characters @cs@. Returns the
-- parsed character.

noneOf :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
noneOf cs = satisfy (`notElem` cs)
{-# INLINE noneOf #-}

-- | The same as 'noneOf', but case-insensitive.
--
-- > consonant = noneOf' "aeiou" <?> "consonant"

noneOf' :: (Foldable f, MonadParsec e s m, Token s ~ Char) => f Char -> m Char
noneOf' cs = satisfy (`notElemi` cs)
{-# INLINE noneOf' #-}

-- | The parser @satisfy f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)

satisfy :: (MonadParsec e s m, Token s ~ Char) => (Char -> Bool) -> m Char
satisfy f = token testChar Nothing
  where
    testChar x =
      if f x
        then Right x
        else Left (E.singleton (Tokens (x:|[])), E.empty, E.empty)
{-# INLINE satisfy #-}

----------------------------------------------------------------------------
-- Sequence of characters

-- | @string s@ parses a sequence of characters given by @s@. Returns
-- the parsed string (i.e. @s@).
--
-- > divOrMod = string "div" <|> string "mod"

string :: (MonadParsec e s m, Token s ~ Char) => String -> m String
string = tokens (==)
{-# INLINE string #-}

-- | The same as 'string', but case-insensitive. On success returns string
-- cased as actually parsed input.
--
-- >>> parseTest (string' "foobar") "foObAr"
-- "foObAr"

string' :: (MonadParsec e s m, Token s ~ Char) => String -> m String
string' = tokens casei
{-# INLINE string' #-}

----------------------------------------------------------------------------
-- Helpers

-- | Case-insensitive equality test for characters.

casei :: Char -> Char -> Bool
casei x y = toUpper x == toUpper y
{-# INLINE casei #-}

-- | Case-insensitive 'elem'.

elemi :: Foldable f => Char -> f Char -> Bool
elemi = any . casei
{-# INLINE elemi #-}

-- | Case-insensitive 'notElem'.

notElemi :: Foldable f => Char -> f Char -> Bool
notElemi c = not . elemi c
{-# INLINE notElemi #-}
