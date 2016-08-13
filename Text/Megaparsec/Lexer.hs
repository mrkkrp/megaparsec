-- |
-- Module      :  Text.Megaparsec.Lexer
-- Copyright   :  © 2015–2016 Megaparsec contributors
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

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Text.Megaparsec.Lexer
  ( -- * White space
    space
  , lexeme
  , symbol
  , symbol'
  , skipLineComment
  , skipBlockComment
  , skipBlockCommentNested
    -- * Indentation
  , indentLevel
  , incorrectIndent
  , indentGuard
  , nonIndented
  , IndentOpt (..)
  , indentBlock
  , lineFold
    -- * Character and string literals
  , charLiteral
    -- * Numbers
  , integer
  , decimal
  , hexadecimal
  , octal
  , scientific
  , float
  , number
  , signed )
where

import Control.Applicative ((<|>), some, optional)
import Control.Monad (void)
import Data.Char (readLitChar)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Scientific (Scientific, toRealFloat)
import qualified Data.Set as E

import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char as C

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (*>), (<*>), pure)
#endif

----------------------------------------------------------------------------
-- White space

-- | @space spaceChar lineComment blockComment@ produces parser that can
-- parse white space in general. It's expected that you create such a parser
-- once and pass it to other functions in this module as needed (when you
-- see @spaceConsumer@ in documentation, usually it means that something
-- like 'space' is expected there).
--
-- @spaceChar@ is used to parse trivial space characters. You can use
-- 'C.spaceChar' from "Text.Megaparsec.Char" for this purpose as well as
-- your own parser (if you don't want to automatically consume newlines, for
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

space :: MonadParsec e s m
  => m () -- ^ A parser for a space character (e.g. 'C.spaceChar')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
space ch line block = hidden . skipMany $ choice [ch, line, block]

-- | This is a wrapper for lexemes. Typical usage is to supply the first
-- argument (parser that consumes white space, probably defined via 'space')
-- and use the resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.integer

lexeme :: MonadParsec e s m
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

symbol :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume white space after lexeme
  -> String            -- ^ String to parse
  -> m String
symbol spc = lexeme spc . C.string

-- | Case-insensitive version of 'symbol'. This may be helpful if you're
-- working with case-insensitive languages.

symbol' :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume white space after lexeme
  -> String            -- ^ String to parse (case-insensitive)
  -> m String
symbol' spc = lexeme spc . C.string'

-- | Given comment prefix this function returns a parser that skips line
-- comments. Note that it stops just before newline character but doesn't
-- consume the newline. Newline is either supposed to be consumed by 'space'
-- parser or picked up manually.

skipLineComment :: (MonadParsec e s m, Token s ~ Char)
  => String            -- ^ Line comment prefix
  -> m ()
skipLineComment prefix = p >> void (manyTill C.anyChar n)
  where p = C.string prefix
        n = lookAhead (void C.newline) <|> eof

-- | @skipBlockComment start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: (MonadParsec e s m, Token s ~ Char)
  => String            -- ^ Start of block comment
  -> String            -- ^ End of block comment
  -> m ()
skipBlockComment start end = p >> void (manyTill C.anyChar n)
  where p = C.string start
        n = C.string end

-- | @skipBlockCommentNested start end@ skips possibly nested block comment
-- starting with @start@ and ending with @end@.
--
-- @since 5.0.0

skipBlockCommentNested :: (MonadParsec e s m, Token s ~ Char)
  => String            -- ^ Start of block comment
  -> String            -- ^ End of block comment
  -> m ()
skipBlockCommentNested start end = p >> void (manyTill e n)
  where e = skipBlockCommentNested start end <|> void C.anyChar
        p = C.string start
        n = C.string end

----------------------------------------------------------------------------
-- Indentation

-- | Return current indentation level.
--
-- The function is a simple shortcut defined as:
--
-- > indentLevel = sourceColumn <$> getPosition
--
-- @since 4.3.0

indentLevel :: MonadParsec e s m => m Pos
indentLevel = sourceColumn <$> getPosition

-- | Fail reporting incorrect indentation error. The error has attached
-- information:
--
--     * Desired ordering between reference level and actual level
--     * Reference indentation level
--     * Actual indentation level
--
-- @since 5.0.0

incorrectIndent :: MonadParsec e s m
  => Ordering  -- ^ Desired ordering between reference level and actual level
  -> Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Actual indentation level
  -> m a
incorrectIndent ord ref actual = failure E.empty E.empty (E.singleton x)
  where x = representIndentation ord ref actual

-- | @indentGuard spaceConsumer ord ref@ first consumes all white space
-- (indentation) with @spaceConsumer@ parser, then it checks column
-- position. Ordering between current indentation level and the reference
-- indentation level @ref@ should be @ord@, otherwise the parser fails. On
-- success the current column position is returned.
--
-- When you want to parse a block of indentation, first run this parser with
-- arguments like @indentGuard spaceConsumer GT (unsafePos 1)@ — this will
-- make sure you have some indentation. Use returned value to check
-- indentation on every subsequent line according to syntax of your
-- language.

indentGuard :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> Ordering -- ^ Desired ordering between reference level and actual level
  -> Pos               -- ^ Reference indentation level
  -> m Pos             -- ^ Current column (indentation level)
indentGuard sc ord ref = do
  sc
  actual <- indentLevel
  if compare actual ref == ord
    then return actual
    else incorrectIndent ord ref actual

-- | Parse a non-indented construction. This ensures that there is no
-- indentation before actual data. Useful, for example, as a wrapper for
-- top-level function definitions.
--
-- @since 4.3.0

nonIndented :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> m a               -- ^ How to parse actual data
  -> m a
nonIndented sc p = indentGuard sc EQ (unsafePos 1) *> p

-- | The data type represents available behaviors for parsing of indented
-- tokens. This is used in 'indentBlock', which see.
--
-- @since 4.3.0

data IndentOpt m a b
  = IndentNone a
    -- ^ Parse no indented tokens, just return the value
  | IndentMany (Maybe Pos) ([b] -> m a) (m b)
    -- ^ Parse many indented tokens (possibly zero), use given indentation
    -- level (if 'Nothing', use level of the first indented token); the
    -- second argument tells how to get final result, and third argument
    -- describes how to parse indented token
  | IndentSome (Maybe Pos) ([b] -> m a) (m b)
    -- ^ Just like 'IndentMany', but requires at least one indented token to
    -- be present

-- | Parse a “reference” token and a number of other tokens that have
-- greater (but the same) level of indentation than that of “reference”
-- token. Reference token can influence parsing, see 'IndentOpt' for more
-- information.
--
-- Tokens /must not/ consume newlines after them. On the other hand, the
-- first argument of this function /must/ consume newlines among other white
-- space characters.
--
-- @since 4.3.0

indentBlock :: (MonadParsec e s m, Token s ~ Char)
  => m ()              -- ^ How to consume indentation (white space)
  -> m (IndentOpt m a b) -- ^ How to parse “reference” token
  -> m a
indentBlock sc r = do
  sc
  ref <- indentLevel
  a   <- r
  case a of
    IndentNone x -> return x
    IndentMany indent f p -> do
      mlvl <- optional . try $ C.eol *> indentGuard sc GT ref
      case mlvl of
        Nothing  -> sc *> f []
        Just lvl -> indentedItems ref (fromMaybe lvl indent) sc p >>= f
    IndentSome indent f p -> do
      lvl <- C.eol *> indentGuard sc GT ref
      indentedItems ref (fromMaybe lvl indent) sc p >>= f

-- | Grab indented items. This is a helper for 'indentBlock', it's not a
-- part of public API.

indentedItems :: MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = go
  where
    go = (sc *> indentLevel) >>= re
    re pos
      | pos <= ref = return []
      | pos == lvl = (:) <$> p <*> go
      | otherwise  = do
          done <- isJust <$> optional eof
          if done
            then return []
            else incorrectIndent EQ lvl pos

-- | Create a parser that supports line-folding. The first argument is used
-- to consume white space between components of line fold, thus it /must/
-- consume newlines in order to work properly. The second argument is a
-- callback that receives custom space-consuming parser as argument. This
-- parser should be used after separate components of line fold that can be
-- put on different lines.
--
-- An example should clarify the usage pattern:
--
-- > sc = L.space (void spaceChar) empty empty
-- >
-- > myFold = L.lineFold sc $ \sc' -> do
-- >   L.symbol sc' "foo"
-- >   L.symbol sc' "bar"
-- >   L.symbol sc  "baz" -- for the last symbol we use normal space consumer
--
-- @since 5.0.0

lineFold :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> (m () -> m a)     -- ^ Callback that uses provided space-consumer
  -> m a
lineFold sc action =
  sc >> indentLevel >>= action . void . indentGuard sc GT

----------------------------------------------------------------------------
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

charLiteral :: (MonadParsec e s m, Token s ~ Char) => m Char
charLiteral = label "literal character" $ do
  -- The @~@ is needed to avoid requiring a MonadFail constraint,
  -- and we do know that r will be non-empty if count' succeeds.
  ~r@(x:_) <- lookAhead $ count' 1 8 C.anyChar
  case listToMaybe (readLitChar r) of
    Just (c, r') -> count (length r - length r') C.anyChar >> return c
    Nothing      -> unexpected (Tokens (x:|[]))

----------------------------------------------------------------------------
-- Numbers

-- | Parse an integer without sign in decimal representation (according to
-- the format of integer literals described in the Haskell report).
--
-- If you need to parse signed integers, see 'signed' combinator.

integer :: (MonadParsec e s m, Token s ~ Char) => m Integer
integer = decimal <?> "integer"

-- | The same as 'integer', but 'integer' is 'label'ed with “integer” label,
-- while this parser is labeled with “decimal integer”.

decimal :: (MonadParsec e s m, Token s ~ Char) => m Integer
decimal = nump "" C.digitChar <?> "decimal integer"

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to the Haskell report
-- except for the fact that this parser doesn't parse “0x” or “0X” prefix.
-- It is responsibility of the programmer to parse correct prefix before
-- parsing the number itself.
--
-- For example you can make it conform to Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal

hexadecimal :: (MonadParsec e s m, Token s ~ Char) => m Integer
hexadecimal = nump "0x" C.hexDigitChar <?> "hexadecimal integer"

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to the Haskell report except for the
-- fact that this parser doesn't parse “0o” or “0O” prefix. It is
-- responsibility of the programmer to parse correct prefix before parsing
-- the number itself.

octal :: (MonadParsec e s m, Token s ~ Char) => m Integer
octal = nump "0o" C.octDigitChar <?> "octal integer"

-- | @nump prefix p@ parses /one/ or more characters with @p@ parser, then
-- prepends @prefix@ to returned value and tries to interpret the result as
-- an integer according to Haskell syntax.

nump :: MonadParsec e s m => String -> m Char -> m Integer
nump prefix baseDigit = read . (prefix ++) <$> some baseDigit

-- | Parse a floating point value as 'Scientific' number. 'Scientific' is
-- great for parsing of arbitrary precision numbers coming from an untrusted
-- source. See documentation in "Data.Scientific" for more information.
-- Representation of the floating point value is expected to be according to
-- the Haskell report.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.
--
-- @since 5.0.0

scientific :: (MonadParsec e s m, Token s ~ Char) => m Scientific
scientific = label "floating point number" (read <$> f)
  where f = (++) <$> some C.digitChar <*> (fraction <|> fExp)

-- | Parse a floating point number without sign. This is a simple shortcut
-- defined as:
--
-- > float = toRealFloat <$> scientific

float :: (MonadParsec e s m, Token s ~ Char) => m Double
float = toRealFloat <$> scientific

-- | This is a helper for 'float' parser. It parses fractional part of
-- floating point number, that is, dot and everything after it.

fraction :: (MonadParsec e s m, Token s ~ Char) => m String
fraction = do
  void (C.char '.')
  d <- some C.digitChar
  e <- option "" fExp
  return ('.' : d ++ e)

-- | This helper parses exponent of floating point numbers.

fExp :: (MonadParsec e s m, Token s ~ Char) => m String
fExp = do
  expChar <- C.char' 'e'
  signStr <- option "" (pure <$> choice (C.char <$> "+-"))
  d       <- some C.digitChar
  return (expChar : signStr ++ d)

-- | Parse a number: either integer or floating point. The parser can handle
-- overlapping grammars graciously. Use functions like
-- 'Data.Scientific.floatingOrInteger' from "Data.Scientific" to test and
-- extract integer or real values.

number :: (MonadParsec e s m, Token s ~ Char) => m Scientific
number = label "number" (read <$> f)
  where f = (++) <$> some C.digitChar <*> option "" (fraction <|> fExp)

-- | @signed space p@ parser parses an optional sign, then if there is a
-- sign it will consume optional white space (using @space@ parser), then it
-- runs parser @p@ which should return a number. Sign of the number is
-- changed according to previously parsed sign.
--
-- For example, to parse signed integer you can write:
--
-- > lexeme        = L.lexeme spaceConsumer
-- > integer       = lexeme L.integer
-- > signedInteger = L.signed spaceConsumer integer

signed :: (MonadParsec e s m, Token s ~ Char, Num a) => m () -> m a -> m a
signed spc p = ($) <$> option id (lexeme spc sign) <*> p

-- | Parse a sign and return either 'id' or 'negate' according to parsed
-- sign.

sign :: (MonadParsec e s m, Token s ~ Char, Num a) => m (a -> a)
sign = (C.char '+' *> return id) <|> (C.char '-' *> return negate)
