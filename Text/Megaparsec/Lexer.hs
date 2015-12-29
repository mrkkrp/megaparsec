-- |
-- Module      :  Text.Megaparsec.Lexer
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- High-level parsers to help you write your lexer. The module doesn't
-- impose how you should write your parser, but certain approaches may be
-- more elegant than others. Especially important theme is parsing of white
-- space, comments, and indentation.
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Megaparsec.Lexer as L

module Text.Megaparsec.Lexer
  ( -- * White space
    space
  , lexeme
  , symbol
  , symbol'
  , skipLineComment
  , skipBlockComment
    -- * Indentation
  , indentGuard
  , notIndented
  , withBlock
    -- * Character and string literals
  , charLiteral
    -- * Numbers
  , Signed (..)
  , integer
  , decimal
  , hexadecimal
  , octal
  , float
  , number
  , signed )
where

import Control.Applicative ((<|>), many, some)
import Control.Monad (void)
import Data.Char (readLitChar)
import Data.Maybe (listToMaybe)
import Prelude hiding (negate)
import qualified Prelude

import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken
import qualified Text.Megaparsec.Char as C

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
#endif

-- White space and indentation

-- | @space spaceChar lineComment blockComment@ produces parser that can
-- parse white space in general. It's expected that you create such a parser
-- once and pass it to other functions in this module as needed (when you
-- see @spaceConsumer@ in documentation, usually it means that something
-- like 'space' is expected there).
--
-- @spaceChar@ is used to parse trivial space characters. You can use
-- 'C.spaceChar' from "Text.Megaparsec.Char" for this purpose as well as
-- your own parser (if you don't want automatically consume newlines, for
-- example).
--
-- @lineComment@ is used to parse line comments. You can use
-- 'skipLineComment' if you don't need anything special.
--
-- @blockComment@ is used to parse block (multi-line) comments. You can use
-- 'skipBlockComment' if you don't need anything special.
--
-- Parsing of white space is an important part of any parser. We propose a
-- convention where every lexeme parser assumes no spaces before the lexeme
-- and consumes all spaces after the lexeme; this is what the 'lexeme'
-- combinator does, and so it's enough to wrap every lexeme parser with
-- 'lexeme' to achieve this. Note that you'll need to call 'space' manually
-- to consume any white space before the first lexeme (i.e. at the beginning
-- of the file).

space :: MonadParsec s m Char
  => m () -- ^ A parser for a space character (e.g. 'C.spaceChar')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
space ch line block = hidden . skipMany $ choice [ch, line, block]

-- | This is wrapper for lexemes. Typical usage is to supply first argument
-- (parser that consumes white space, probably defined via 'space') and use
-- the resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.integer

lexeme :: MonadParsec s m Char
  => m ()              -- ^ How to consume white space after lexeme
  -> m a               -- ^ How to parse actual lexeme
  -> m a
lexeme spc p = p <* spc

-- | This is a helper to parse symbols, i.e. verbatim strings. You pass the
-- first argument (parser that consumes white space, probably defined via
-- 'space') and then you can use the resulting function to parse strings:
--
-- > symbol    = L.symbol spaceConsumer
-- >
-- > parens    = between (symbol "(") (symbol ")")
-- > braces    = between (symbol "{") (symbol "}")
-- > angles    = between (symbol "<") (symbol ">")
-- > brackets  = between (symbol "[") (symbol "]")
-- > semicolon = symbol ";"
-- > comma     = symbol ","
-- > colon     = symbol ":"
-- > dot       = symbol "."

symbol :: MonadParsec s m Char
  => m ()              -- ^ How to consume white space after lexeme
  -> String            -- ^ String to parse
  -> m String
symbol spc = lexeme spc . C.string

-- | Case-insensitive version of 'symbol'. This may be helpful if you're
-- working with case-insensitive languages.

symbol' :: MonadParsec s m Char
  => m ()              -- ^ How to consume white space after lexeme
  -> String            -- ^ String to parse (case-insensitive)
  -> m String
symbol' spc = lexeme spc . C.string'

-- | Given comment prefix this function returns parser that skips line
-- comments. Note that it stops just before newline character but doesn't
-- consume the newline. Newline is either supposed to be consumed by 'space'
-- parser or picked up manually.

skipLineComment :: MonadParsec s m Char
  => String            -- ^ Line comment prefix
  -> m ()
skipLineComment prefix = p >> void (manyTill C.anyChar n)
  where p = try (C.string prefix)
        n = lookAhead C.newline

-- | @skipBlockComment start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: MonadParsec s m Char
  => String            -- ^ Start of block comment
  -> String            -- ^ End of block comment
  -> m ()
skipBlockComment start end = p >> void (manyTill C.anyChar n)
  where p = try (C.string start)
        n = try (C.string end)

-- Indentation

-- | @indentGuard spaceConsumer test@ first consumes all white space
-- (indentation) with @spaceConsumer@ parser, then it checks column
-- position. It should satisfy supplied predicate @test@, otherwise the
-- parser fails with error message “incorrect indentation”. On success
-- current column position is returned.
--
-- When you want to parse block of indentation first run this parser with
-- predicate like @(> 1)@ — this will make sure you have some
-- indentation. Use returned value to check indentation on every subsequent
-- line according to syntax of your language.

indentGuard :: MonadParsec s m Char
  => m ()              -- ^ How to consume indentation (white space)
  -> (Int -> Bool)     -- ^ Predicate checking indentation level
  -> m Int             -- ^ Current column (indentation level)
indentGuard spc p = do
  spc
  pos <- sourceColumn <$> getPosition
  if p pos
  then return pos
  else fail "incorrect indentation"

-- | Parse non-indented construction. This ensures that there is no
-- indentation before actual data. Useful as a wrapper for top-level
-- function definitions, for example.

notIndented :: MonadParsec s m Char
  => m ()              -- ^ How to consume indentation (white space)
  -> m a               -- ^ How to parse actual data
  -> m a
notIndented sc p = indentGuard sc (== 1) *> p

-- | Parse a “reference” token and a number of other tokens that have
-- greater (but the same) level of indentation than that of “reference”
-- token.
--
-- Note that the second argument (function that's used to combine results of
-- parsing of “reference” token and indented tokens) lives in parser monad,
-- this allows to perform additional checks and fail with custom error
-- messages, for example.

withBlock :: MonadParsec s m Char
  => m ()              -- ^ How to consume indentation (white space)
  -> (a -> [b] -> m c) -- ^ How to create output from parsed pieces
  -> m a               -- ^ How to parse “reference” token
  -> m b               -- ^ How to parse indented tokens
  -> m c
withBlock sc f r p = do
  ref <- indentGuard sc (const True)
  a <- r
  b <- many (indentGuard sc (> ref) *> p)
  f a b

-- Character and string literals

-- | The lexeme parser parses a single literal character without
-- quotes. Purpose of this parser is to help with parsing of conventional
-- escape sequences. It's your responsibility to take care of character
-- literal syntax in your language (by surrounding it with single quotes or
-- similar).
--
-- The literal character is parsed according to the grammar rules defined in
-- the Haskell report.
--
-- Note that you can use this parser as a building block to parse various
-- string literals:
--
-- > stringLiteral = char '"' >> manyTill L.charLiteral (char '"')

charLiteral :: MonadParsec s m Char => m Char
charLiteral = label "literal character" $ do
  r@(x:_) <- lookAhead $ count' 1 8 C.anyChar
  case listToMaybe (readLitChar r) of
    Just (c, r') -> count (length r - length r') C.anyChar >> return c
    Nothing      -> unexpected (showToken x)

-- Numbers

-- | This type class abstracts the concept of signed number in context of
-- this module. This is especially useful when you want to compose 'signed'
-- and 'number'.

class Signed a where

  -- | Unary negation.

  negate :: a -> a

instance Signed Integer where
  negate = Prelude.negate

instance Signed Double where
  negate = Prelude.negate

instance (Signed l, Signed r) => Signed (Either l r) where
  negate (Left  x) = Left  $ negate x
  negate (Right x) = Right $ negate x

-- | Parse an integer without sign in decimal representation (according to
-- format of integer literals described in Haskell report).
--
-- If you need to parse signed integers, see 'signed' combinator.

integer :: MonadParsec s m Char => m Integer
integer = decimal <?> "integer"

-- | The same as 'integer', but 'integer' is 'label'ed with “integer” label,
-- while this parser is labeled with “decimal integer”.

decimal :: MonadParsec s m Char => m Integer
decimal = nump "" C.digitChar <?> "decimal integer"

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to Haskell report except
-- for the fact that this parser doesn't parse “0x” or “0X” prefix. It is
-- responsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

hexadecimal :: MonadParsec s m Char => m Integer
hexadecimal = nump "0x" C.hexDigitChar <?> "hexadecimal integer"

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to Haskell report except for the fact
-- that this parser doesn't parse “0o” or “0O” prefix. It is responsibility
-- of the programmer to parse correct prefix before parsing the number
-- itself.

octal :: MonadParsec s m Char => m Integer
octal = nump "0o" C.octDigitChar <?> "octal integer"

-- | @nump prefix p@ parses /one/ or more characters with @p@ parser, then
-- prepends @prefix@ to returned value and tries to interpret the result as
-- an integer according to Haskell syntax.

nump :: MonadParsec s m Char => String -> m Char -> m Integer
nump prefix baseDigit = read . (prefix ++) <$> some baseDigit

-- | Parse a floating point value without sign. Representation of floating
-- point value is expected to be according to Haskell report.
--
-- If you need to parse signed floats, see 'signed'.

float :: MonadParsec s m Char => m Double
float = label "float" (read <$> f)
  where f = (++) <$> some C.digitChar <*> (fraction <|> fExp)

-- | This is a helper for 'float' parser. It parses fractional part of
-- floating point number, that is, dot and everything after it.

fraction :: MonadParsec s m Char => m String
fraction = do
  void (C.char '.')
  d <- some C.digitChar
  e <- option "" fExp
  return ('.' : d ++ e)

-- | This helper parses exponent of floating point numbers.

fExp :: MonadParsec s m Char => m String
fExp = do
  expChar <- C.char' 'e'
  signStr <- option "" (pure <$> choice (C.char <$> "+-"))
  d       <- some C.digitChar
  return (expChar : signStr ++ d)

-- | Parse a number: either integer or floating point. The parser can handle
-- overlapping grammars graciously.

number :: MonadParsec s m Char => m (Either Integer Double)
number = (Right <$> try float) <|> (Left <$> integer) <?> "number"

-- | @signed space p@ parser parses optional sign, then if there is a sign
-- it will consume optional white space (using @space@ parser), then it runs
-- parser @p@ which should return a number. Sign of the number is changed
-- according to previously parsed sign.
--
-- For example, to parse signed integer you can write:
--
-- > lexeme        = L.lexeme spaceConsumer
-- > integer       = lexeme L.integer
-- > signedInteger = L.signed spaceConsumer integer

signed :: (MonadParsec s m Char, Signed a) => m () -> m a -> m a
signed spc p = ($) <$> option id (lexeme spc sign) <*> p

-- | Parse a sign and return either 'id' or 'negate' according to parsed
-- sign.

sign :: (MonadParsec s m Char, Signed a) => m (a -> a)
sign = (C.char '+' *> return id) <|> (C.char '-' *> return negate)
