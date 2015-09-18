-- |
-- Module      :  Text.Megaparsec.Lexer
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (uses local universal quantification: PolymorphicComponents)
--
-- High-level parsers to help you write your lexer. The module doesn't
-- impose how you should write your parser, but certain approaches may be
-- more elegant than others. Especially important theme is parsing of write
-- space, comments and indentation.
--
-- This module is supposed to be imported qualified:
--
-- > import qualified Text.Megaparsec.Lexer as L

module Text.Megaparsec.Lexer
  ( -- * White space and indentation
    space
  , lexeme
  , symbol
  , symbol'
  , indentGuard
  , skipLineComment
  , skipBlockComment
    -- * Character and string literals
  , charLiteral
    -- * Numbers
  , integer
  , decimal
  , hexadecimal
  , octal
  , float
  , number
  , signed )
where

import Control.Applicative ((<|>), some)
import Control.Monad (void)
import Data.Char (readLitChar)
import Data.Maybe (listToMaybe)

import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken
import qualified Text.Megaparsec.Char as C

-- White space and indentation

-- | @space spaceChar lineComment blockComment@ produces parser that can
-- parse white space in general. It's expected that you create such a parser
-- once and pass it to many other function in this module as needed (it's
-- usually called @spaceConsumer@ in doc-strings here).
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
-- Parsing of white space is important part of any parser. We propose scheme
-- where every lexeme should consume all trailing white space, but not
-- leading one. You should wrap every lexeme parser with 'lexeme' to achieve
-- this. You only need to call 'space' “manually” to consume any white space
-- before the first lexeme (at the beginning of file).

space :: MonadParsec s m Char => m () -> m () -> m () -> m ()
space ch line block = hidden . skipMany $ choice [ch, line, block]

-- | This is wrapper for lexemes. Typical usage is to supply first argument
-- (parser that consumes white space, probably defined via 'space') and use
-- resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.integer

lexeme :: MonadParsec s m Char => m () -> m a -> m a
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

symbol :: MonadParsec s m Char => m () -> String -> m String
symbol spc = lexeme spc . C.string

-- | Case-insensitive version of 'symbol'. This may be helpful if you're
-- working with case-insensitive languages.

symbol' :: MonadParsec s m Char => m () -> String -> m String
symbol' spc = lexeme spc . C.string'

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

indentGuard :: MonadParsec s m Char => m () -> (Int -> Bool) -> m Int
indentGuard spc p = do
  spc
  pos <- sourceColumn <$> getPosition
  if p pos
  then return pos
  else fail "incorrect indentation"

-- | Given comment prefix this function returns parser that skips line
-- comments. Note that it stops just before newline character but doesn't
-- consume the newline. Newline is either supposed to be consumed by 'space'
-- parser or picked up manually.

skipLineComment :: MonadParsec s m Char => String -> m ()
skipLineComment prefix = p >> void (manyTill C.anyChar n)
  where p = try $ C.string prefix
        n = lookAhead C.newline

-- | @skipBlockComment start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: MonadParsec s m Char => String -> String -> m ()
skipBlockComment start end = p >> void (manyTill C.anyChar n)
  where p = try $ C.string start
        n = try $ C.string end

-- Character and string literals

-- | The lexeme parser parses a single literal character without
-- quotes. Purpose of this parser is to help with parsing of commonly used
-- escape sequences. It's your responsibility to take care of character
-- literal syntax in your language (surround it with single quotes or
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

-- | Parse an integer without sign in decimal representation (according to
-- format of integer literals described in Haskell report).
--
-- If you need to parse signed integers, see 'signed' combinator.

integer :: MonadParsec s m Char => m Integer
integer = decimal <?> "integer"

-- | The same as 'integer', but 'integer' is 'label'ed with “integer” label,
-- while this parser is not labeled.

decimal :: MonadParsec s m Char => m Integer
decimal = nump "" C.digitChar

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to Haskell report except
-- for the fact that this parser doesn't parse “0x” or “0X” prefix. It is
-- reponsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

hexadecimal :: MonadParsec s m Char => m Integer
hexadecimal = nump "0x" C.hexDigitChar

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to Haskell report except for the fact
-- that this parser doesn't parse “0o” or “0O” prefix. It is responsibility
-- of the programmer to parse correct prefix before parsing the number
-- itself.

octal :: MonadParsec s m Char => m Integer
octal = nump "0o" C.octDigitChar

-- | @nump prefix p@ parses /one/ or more characters with @p@ parser, then
-- prepends @prefix@ to returned value and tries to interpret the result as
-- an integer according to Haskell syntax.

nump :: MonadParsec s m Char => String -> m Char -> m Integer
nump prefix baseDigit = read . (prefix ++) <$> some baseDigit

-- | Parse a floating point value without sign. Representation of floating
-- point value is expected to be according to Haskell report.
--
-- If you need to parse signed floats, see 'signed' combinator.

float :: MonadParsec s m Char => m Double
float = label "float" $ read <$> f
  where f = do
          d    <- some C.digitChar
          rest <- fraction <|> fExp
          return $ d ++ rest

-- | This is a helper for 'float' parser. It parses fractional part of
-- floating point number, that is, dot and everything after it.

fraction :: MonadParsec s m Char => m String
fraction = do
  void $ C.char '.'
  d <- some C.digitChar
  e <- option "" fExp
  return $ '.' : d ++ e

-- | This helper parses exponent of floating point numbers.

fExp :: MonadParsec s m Char => m String
fExp = do
  expChar <- C.char' 'e'
  signStr <- option "" (pure <$> choice (C.char <$> "+-"))
  d       <- some C.digitChar
  return $ expChar : signStr ++ d

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
-- > signedInteger = signed spaceConsumer integer

signed :: (MonadParsec s m Char, Num a) => m () -> m a -> m a
signed spc p = ($) <$> option id (lexeme spc sign) <*> p

-- | Parse a sign and return either 'id' or 'negate' according to parsed
-- sign.

sign :: (MonadParsec s m Char, Num a) => m (a -> a)
sign = (C.char '+' *> return id) <|> (C.char '-' *> return negate)
