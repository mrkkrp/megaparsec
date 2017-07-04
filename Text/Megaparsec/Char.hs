-- |
-- Module      :  Text.Megaparsec.Char
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- Commonly used character parsers.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Char
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
  , notChar
  , oneOf
  , noneOf
  , satisfy
    -- * Sequence of characters
  , string
  , string' )
where

import Control.Applicative
import Data.Char
import Data.Function (on)
import Data.Functor (void)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import qualified Data.CaseInsensitive as CI
import qualified Data.Set             as E

import Text.Megaparsec

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable (), elem, notElem)
import Prelude hiding (elem, notElem)
#endif

----------------------------------------------------------------------------
-- Simple parsers

-- | Parse a newline character.

newline :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
newline = char '\n'
{-# INLINE newline #-}

-- | Parse a carriage return character followed by a newline character.
-- Return the sequence of characters parsed.

crlf :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
crlf = string (tokensToChunk (Proxy :: Proxy s) "\r\n")
{-# INLINE crlf #-}

-- | Parse a CRLF (see 'crlf') or LF (see 'newline') end of line. Return the
-- sequence of characters parsed.

eol :: forall e s m. (MonadParsec e s m, Token s ~ Char) => m (Tokens s)
eol = (tokenToChunk (Proxy :: Proxy s) <$> newline)
  <|> crlf
  <?> "end of line"
{-# INLINE eol #-}

-- | Parse a tab character.

tab :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
tab = char '\t'
{-# INLINE tab #-}

-- | Skip /zero/ or more white space characters.
--
-- See also: 'skipMany' and 'spaceChar'.

space :: (MonadParsec e s m, Token s ~ Char) => m ()
space = void $ takeWhileP (Just "white space") isSpace
{-# INLINE space #-}

-- | Skip /one/ or more white space characters.
--
-- See also: 'skipSome' and 'spaceChar'.
--
-- @since 6.0.0

space1 :: (MonadParsec e s m, Token s ~ Char) => m ()
space1 = void $ takeWhile1P (Just "white space") isSpace
{-# INLINE space1 #-}

----------------------------------------------------------------------------
-- Categories of characters

-- | Parse a control character (a non-printing character of the Latin-1
-- subset of Unicode).

controlChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
controlChar = satisfy isControl <?> "control character"
{-# INLINE controlChar #-}

-- | Parse a Unicode space character, and the control characters: tab,
-- newline, carriage return, form feed, and vertical tab.

spaceChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
spaceChar = satisfy isSpace <?> "white space"
{-# INLINE spaceChar #-}

-- | Parse an upper-case or title-case alphabetic Unicode character. Title
-- case is used by a small number of letter ligatures like the
-- single-character form of Lj.

upperChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
upperChar = satisfy isUpper <?> "uppercase letter"
{-# INLINE upperChar #-}

-- | Parse a lower-case alphabetic Unicode character.

lowerChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
lowerChar = satisfy isLower <?> "lowercase letter"
{-# INLINE lowerChar #-}

-- | Parse an alphabetic Unicode character: lower-case, upper-case, or
-- title-case letter, or a letter of case-less scripts\/modifier letter.

letterChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
letterChar = satisfy isLetter <?> "letter"
{-# INLINE letterChar #-}

-- | Parse an alphabetic or numeric digit Unicode characters.
--
-- Note that the numeric digits outside the ASCII range are parsed by this
-- parser but not by 'digitChar'. Such digits may be part of identifiers but
-- are not used by the printer and reader to represent numbers.

alphaNumChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
alphaNumChar = satisfy isAlphaNum <?> "alphanumeric character"
{-# INLINE alphaNumChar #-}

-- | Parse a printable Unicode character: letter, number, mark, punctuation,
-- symbol or space.

printChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
printChar = satisfy isPrint <?> "printable character"
{-# INLINE printChar #-}

-- | Parse an ASCII digit, i.e between “0” and “9”.

digitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
digitChar = satisfy isDigit <?> "digit"
{-# INLINE digitChar #-}

-- | Parse an octal digit, i.e. between “0” and “7”.

octDigitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
octDigitChar = satisfy isOctDigit <?> "octal digit"
{-# INLINE octDigitChar #-}

-- | Parse a hexadecimal digit, i.e. between “0” and “9”, or “a” and “f”, or
-- “A” and “F”.

hexDigitChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
hexDigitChar = satisfy isHexDigit <?> "hexadecimal digit"
{-# INLINE hexDigitChar #-}

-- | Parse a Unicode mark character (accents and the like), which combines
-- with preceding characters.

markChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
markChar = satisfy isMark <?> "mark character"
{-# INLINE markChar #-}

-- | Parse a Unicode numeric character, including digits from various
-- scripts, Roman numerals, etc.

numberChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
numberChar = satisfy isNumber <?> "numeric character"
{-# INLINE numberChar #-}

-- | Parse a Unicode punctuation character, including various kinds of
-- connectors, brackets and quotes.

punctuationChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
punctuationChar = satisfy isPunctuation <?> "punctuation"
{-# INLINE punctuationChar #-}

-- | Parse a Unicode symbol characters, including mathematical and currency
-- symbols.

symbolChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
symbolChar = satisfy isSymbol <?> "symbol"
{-# INLINE symbolChar #-}

-- | Parse a Unicode space and separator characters.

separatorChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
separatorChar = satisfy isSeparator <?> "separator"
{-# INLINE separatorChar #-}

-- | Parse a character from the first 128 characters of the Unicode
-- character set, corresponding to the ASCII character set.

asciiChar :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
asciiChar = satisfy isAscii <?> "ASCII character"
{-# INLINE asciiChar #-}

-- | Parse a character from the first 256 characters of the Unicode
-- character set, corresponding to the ISO 8859-1 (Latin-1) character set.

latin1Char :: (MonadParsec e s m, Token s ~ Char) => m (Token s)
latin1Char = satisfy isLatin1 <?> "Latin-1 character"
{-# INLINE latin1Char #-}

-- | @'charCategory' cat@ parses character in Unicode General Category
-- @cat@, see 'Data.Char.GeneralCategory'.

charCategory :: (MonadParsec e s m, Token s ~ Char)
  => GeneralCategory
  -> m (Token s)
charCategory cat = satisfy ((== cat) . generalCategory) <?> categoryName cat
{-# INLINE charCategory #-}

-- | Return the human-readable name of Unicode General Category.

categoryName :: GeneralCategory -> String
categoryName = \case
  UppercaseLetter      -> "uppercase letter"
  LowercaseLetter      -> "lowercase letter"
  TitlecaseLetter      -> "titlecase letter"
  ModifierLetter       -> "modifier letter"
  OtherLetter          -> "other letter"
  NonSpacingMark       -> "non-spacing mark"
  SpacingCombiningMark -> "spacing combining mark"
  EnclosingMark        -> "enclosing mark"
  DecimalNumber        -> "decimal number character"
  LetterNumber         -> "letter number character"
  OtherNumber          -> "other number character"
  ConnectorPunctuation -> "connector punctuation"
  DashPunctuation      -> "dash punctuation"
  OpenPunctuation      -> "open punctuation"
  ClosePunctuation     -> "close punctuation"
  InitialQuote         -> "initial quote"
  FinalQuote           -> "final quote"
  OtherPunctuation     -> "other punctuation"
  MathSymbol           -> "math symbol"
  CurrencySymbol       -> "currency symbol"
  ModifierSymbol       -> "modifier symbol"
  OtherSymbol          -> "other symbol"
  Space                -> "white space"
  LineSeparator        -> "line separator"
  ParagraphSeparator   -> "paragraph separator"
  Control              -> "control character"
  Format               -> "format character"
  Surrogate            -> "surrogate character"
  PrivateUse           -> "private-use Unicode character"
  NotAssigned          -> "non-assigned Unicode character"

----------------------------------------------------------------------------
-- More general parsers

-- | @'char' c@ parses a single character @c@.
--
-- > semicolon = char ';'

char :: MonadParsec e s m => Token s -> m (Token s)
char c = token testChar (Just c)
  where
    f x = Tokens (x:|[])
    testChar x =
      if x == c
        then Right x
        else Left (pure (f x), E.singleton (f c))
{-# INLINE char #-}

-- | The same as 'char' but case-insensitive. This parser returns the
-- actually parsed character preserving its case.
--
-- >>> parseTest (char' 'e') "E"
-- 'E'
-- >>> parseTest (char' 'e') "G"
-- 1:1:
-- unexpected 'G'
-- expecting 'E' or 'e'

char' :: (MonadParsec e s m, Token s ~ Char) => Token s -> m (Token s)
char' c = choice [char c, char (swapCase c)]
  where
    swapCase x
      | isUpper x = toLower x
      | isLower x = toUpper x
      | otherwise = x
{-# INLINE char' #-}

-- | This parser succeeds for any character. Returns the parsed character.

anyChar :: MonadParsec e s m => m (Token s)
anyChar = satisfy (const True) <?> "character"
{-# INLINE anyChar #-}

-- | Match any character but the given one. It's a good idea to attach a
-- 'label' to this parser manually.
--
-- @since 6.0.0

notChar :: MonadParsec e s m => Token s -> m (Token s)
notChar c = satisfy (/= c)
{-# INLINE notChar #-}

-- | @'oneOf' cs@ succeeds if the current character is in the supplied
-- collection of characters @cs@. Returns the parsed character. Note that
-- this parser cannot automatically generate the “expected” component of
-- error message, so usually you should label it manually with 'label' or
-- ('<?>').
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"
--
-- __Performance note__: prefer 'satisfy' when you can because it's faster
-- when you have only a couple of tokens to compare to:
--
-- > quoteFast = satisfy (\x -> x == '\'' || x == '\"')
-- > quoteSlow = oneOf "'\""

oneOf :: (Foldable f, MonadParsec e s m)
  => f (Token s)
  -> m (Token s)
oneOf cs = satisfy (`elem` cs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @'noneOf' cs@ succeeds if the current character
-- /not/ in the supplied list of characters @cs@. Returns the parsed
-- character. Note that this parser cannot automatically generate the
-- “expected” component of error message, so usually you should label it
-- manually with 'label' or ('<?>').
--
-- See also: 'satisfy'.
--
-- __Performance note__: prefer 'satisfy' and 'notChar' when you can because
-- it's faster.

noneOf :: (Foldable f, MonadParsec e s m)
  => f (Token s)
  -> m (Token s)
noneOf cs = satisfy (`notElem` cs)
{-# INLINE noneOf #-}

-- | The parser @'satisfy' f@ succeeds for any character for which the
-- supplied function @f@ returns 'True'. Returns the character that is
-- actually parsed.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)

satisfy :: MonadParsec e s m
  => (Token s -> Bool) -- ^ Predicate to apply
  -> m (Token s)
satisfy f = token testChar Nothing
  where
    testChar x =
      if f x
        then Right x
        else Left (pure (Tokens (x:|[])), E.empty)
{-# INLINE satisfy #-}

----------------------------------------------------------------------------
-- Sequence of characters

-- | @'string' s@ parses a sequence of characters given by @s@. Returns the
-- parsed string (i.e. @s@).
--
-- > divOrMod = string "div" <|> string "mod"

string :: MonadParsec e s m
  => Tokens s
  -> m (Tokens s)
string = tokens (==)
{-# INLINE string #-}

-- | The same as 'string', but case-insensitive. On success returns string
-- cased as actually parsed input.
--
-- >>> parseTest (string' "foobar") "foObAr"
-- "foObAr"

string' :: (MonadParsec e s m, CI.FoldCase (Tokens s))
  => Tokens s
  -> m (Tokens s)
string' = tokens ((==) `on` CI.mk)
{-# INLINE string' #-}
