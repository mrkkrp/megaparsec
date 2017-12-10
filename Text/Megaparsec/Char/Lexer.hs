-- |
-- Module      :  Text.Megaparsec.Char.Lexer
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- High-level parsers to help you write your lexer. The module doesn't
-- impose how you should write your parser, but certain approaches may be
-- more elegant than others. Especially important theme is parsing of white
-- space, comments, and indentation.
--
-- Parsing of white space is an important part of any parser. We propose a
-- convention where __every lexeme parser assumes no spaces before the__
-- __lexeme and consumes all spaces after the lexeme__; this is what the
-- 'lexeme' combinator does, and so it's enough to wrap every lexeme parser
-- with 'lexeme' to achieve this. Note that you'll need to call 'space'
-- manually to consume any white space before the first lexeme (i.e. at the
-- beginning of the file).
--
-- This module is intended to be imported qualified:
--
-- > import qualified Text.Megaparsec.Char.Lexer as L
--
-- To do lexing of byte streams, see "Text.Megaparsec.Byte.Lexer".

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Text.Megaparsec.Char.Lexer
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
  , decimal
  , octal
  , hexadecimal
  , scientific
  , float
  , signed )
where

import Control.Applicative
import Control.Monad (void)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (listToMaybe, fromMaybe, isJust)
import Data.Proxy
import Data.Scientific (Scientific)
import Text.Megaparsec
import qualified Data.CaseInsensitive as CI
import qualified Data.Char            as Char
import qualified Data.Scientific      as Sci
import qualified Data.Set             as E
import qualified Text.Megaparsec.Char as C

----------------------------------------------------------------------------
-- White space

-- | @'space' sc lineComment blockComment@ produces parser that can parse
-- white space in general. It's expected that you create such a parser once
-- and pass it to other functions in this module as needed (when you see
-- @spaceConsumer@ in documentation, usually it means that something like
-- 'space' is expected there).
--
-- @sc@ is used to parse blocks of space characters. You can use 'C.space1'
-- from "Text.Megaparsec.Char" for this purpose as well as your own parser
-- (if you don't want to automatically consume newlines, for example). Make
-- sure the parser does not succeed on empty input though. In earlier
-- version 'C.spaceChar' was recommended, but now parsers based on
-- 'takeWhile1P' are preferred because of their speed.
--
-- @lineComment@ is used to parse line comments. You can use
-- 'skipLineComment' if you don't need anything special.
--
-- @blockComment@ is used to parse block (multi-line) comments. You can use
-- 'skipBlockComment' or 'skipBlockCommentNested' if you don't need anything
-- special.
--
-- If you don't want to allow a kind of comment, simply pass 'empty' which
-- will fail instantly when parsing of that sort of comment is attempted and
-- 'space' will just move on or finish depending on whether there is more
-- white space for it to consume.

space :: MonadParsec e s m
  => m () -- ^ A parser for space characters which does not accept empty
          -- input (e.g. 'C.space1')
  -> m () -- ^ A parser for a line comment (e.g. 'skipLineComment')
  -> m () -- ^ A parser for a block comment (e.g. 'skipBlockComment')
  -> m ()
space sp line block = skipMany $ choice
  [hidden sp, hidden line, hidden block]
{-# INLINEABLE space #-}

-- | This is a wrapper for lexemes. Typical usage is to supply the first
-- argument (parser that consumes white space, probably defined via 'space')
-- and use the resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.decimal

lexeme :: MonadParsec e s m
  => m ()              -- ^ How to consume white space after lexeme
  -> m a               -- ^ How to parse actual lexeme
  -> m a
lexeme spc p = p <* spc
{-# INLINEABLE lexeme #-}

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

symbol :: MonadParsec e s m
  => m ()              -- ^ How to consume white space after lexeme
  -> Tokens s          -- ^ Symbol to parse
  -> m (Tokens s)
symbol spc = lexeme spc . C.string
{-# INLINEABLE symbol #-}

-- | Case-insensitive version of 'symbol'. This may be helpful if you're
-- working with case-insensitive languages.

symbol' :: (MonadParsec e s m, CI.FoldCase (Tokens s))
  => m ()              -- ^ How to consume white space after lexeme
  -> Tokens s          -- ^ Symbol to parse (case-insensitive)
  -> m (Tokens s)
symbol' spc = lexeme spc . C.string'
{-# INLINEABLE symbol' #-}

-- | Given comment prefix this function returns a parser that skips line
-- comments. Note that it stops just before the newline character but
-- doesn't consume the newline. Newline is either supposed to be consumed by
-- 'space' parser or picked up manually.

skipLineComment :: (MonadParsec e s m, Token s ~ Char)
  => Tokens s          -- ^ Line comment prefix
  -> m ()
skipLineComment prefix =
  C.string prefix *> void (takeWhileP (Just "character") (/= '\n'))
{-# INLINEABLE skipLineComment #-}

-- | @'skipBlockComment' start end@ skips non-nested block comment starting
-- with @start@ and ending with @end@.

skipBlockComment :: (MonadParsec e s m, Token s ~ Char)
  => Tokens s          -- ^ Start of block comment
  -> Tokens s          -- ^ End of block comment
  -> m ()
skipBlockComment start end = p >> void (manyTill C.anyChar n)
  where
    p = C.string start
    n = C.string end
{-# INLINEABLE skipBlockComment #-}

-- | @'skipBlockCommentNested' start end@ skips possibly nested block
-- comment starting with @start@ and ending with @end@.
--
-- @since 5.0.0

skipBlockCommentNested :: (MonadParsec e s m, Token s ~ Char)
  => Tokens s          -- ^ Start of block comment
  -> Tokens s          -- ^ End of block comment
  -> m ()
skipBlockCommentNested start end = p >> void (manyTill e n)
  where
    e = skipBlockCommentNested start end <|> void C.anyChar
    p = C.string start
    n = C.string end
{-# INLINEABLE skipBlockCommentNested #-}

----------------------------------------------------------------------------
-- Indentation

-- | Return the current indentation level.
--
-- The function is a simple shortcut defined as:
--
-- > indentLevel = sourceColumn <$> getPosition
--
-- @since 4.3.0

indentLevel :: MonadParsec e s m => m Pos
indentLevel = sourceColumn <$> getPosition
{-# INLINEABLE indentLevel #-}

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
incorrectIndent ord ref actual = fancyFailure . E.singleton $
  ErrorIndentation ord ref actual
{-# INLINEABLE incorrectIndent #-}

-- | @'indentGuard' spaceConsumer ord ref@ first consumes all white space
-- (indentation) with @spaceConsumer@ parser, then it checks the column
-- position. Ordering between current indentation level and the reference
-- indentation level @ref@ should be @ord@, otherwise the parser fails. On
-- success the current column position is returned.
--
-- When you want to parse a block of indentation, first run this parser with
-- arguments like @'indentGuard' spaceConsumer 'GT' 'pos1'@—this will make
-- sure you have some indentation. Use returned value to check indentation
-- on every subsequent line according to syntax of your language.

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
{-# INLINEABLE indentGuard #-}

-- | Parse a non-indented construction. This ensures that there is no
-- indentation before actual data. Useful, for example, as a wrapper for
-- top-level function definitions.
--
-- @since 4.3.0

nonIndented :: MonadParsec e s m
  => m ()              -- ^ How to consume indentation (white space)
  -> m a               -- ^ How to parse actual data
  -> m a
nonIndented sc p = indentGuard sc EQ pos1 *> p
{-# INLINEABLE nonIndented #-}

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
    -- second argument tells how to get the final result, and the third
    -- argument describes how to parse an indented token
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
    IndentNone x -> sc *> return x
    IndentMany indent f p -> do
      mlvl <- (optional . try) (C.eol *> indentGuard sc GT ref)
      done <- isJust <$> optional eof
      case (mlvl, done) of
        (Just lvl, False) ->
          indentedItems ref (fromMaybe lvl indent) sc p >>= f
        _ -> sc *> f []
    IndentSome indent f p -> do
      pos <- C.eol *> indentGuard sc GT ref
      let lvl = fromMaybe pos indent
      x <- if | pos <= ref -> incorrectIndent GT ref pos
              | pos == lvl -> p
              | otherwise  -> incorrectIndent EQ lvl pos
      xs  <- indentedItems ref lvl sc p
      f (x:xs)
{-# INLINEABLE indentBlock #-}

-- | Grab indented items. This is a helper for 'indentBlock', it's not a
-- part of the public API.

indentedItems :: MonadParsec e s m
  => Pos               -- ^ Reference indentation level
  -> Pos               -- ^ Level of the first indented item ('lookAhead'ed)
  -> m ()              -- ^ How to consume indentation (white space)
  -> m b               -- ^ How to parse indented tokens
  -> m [b]
indentedItems ref lvl sc p = go
  where
    go = do
      sc
      pos  <- indentLevel
      done <- isJust <$> optional eof
      if done
        then return []
        else if | pos <= ref -> return []
                | pos == lvl -> (:) <$> p <*> go
                | otherwise  -> incorrectIndent EQ lvl pos

-- | Create a parser that supports line-folding. The first argument is used
-- to consume white space between components of line fold, thus it /must/
-- consume newlines in order to work properly. The second argument is a
-- callback that receives a custom space-consuming parser as argument. This
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
{-# INLINEABLE lineFold #-}

----------------------------------------------------------------------------
-- Character and string literals

-- | The lexeme parser parses a single literal character without quotes. The
-- purpose of this parser is to help with parsing of conventional escape
-- sequences. It's your responsibility to take care of character literal
-- syntax in your language (by surrounding it with single quotes or
-- similar).
--
-- The literal character is parsed according to the grammar rules defined in
-- the Haskell report.
--
-- Note that you can use this parser as a building block to parse various
-- string literals:
--
-- > stringLiteral = char '"' >> manyTill L.charLiteral (char '"')
--
-- __Performance note__: the parser is not particularly efficient at the
-- moment.

charLiteral :: (MonadParsec e s m, Token s ~ Char) => m Char
charLiteral = label "literal character" $ do
  -- The @~@ is needed to avoid requiring a MonadFail constraint,
  -- and we do know that r will be non-empty if count' succeeds.
  ~r@(x:_) <- lookAhead $ count' 1 8 C.anyChar
  case listToMaybe (Char.readLitChar r) of
    Just (c, r') -> count (length r - length r') C.anyChar >> return c
    Nothing      -> unexpected (Tokens (x:|[]))
{-# INLINEABLE charLiteral #-}

----------------------------------------------------------------------------
-- Numbers

-- | Parse an integer in decimal representation according to the format of
-- integer literals described in the Haskell report.
--
-- If you need to parse signed integers, see 'signed' combinator.
--
-- __Note__: before version 6.0.0 the function returned 'Integer', i.e. it
-- wasn't polymorphic in its return type.

decimal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
decimal = decimal_ <?> "integer"
{-# INLINEABLE decimal #-}

-- | A non-public helper to parse decimal integers.

decimal_
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
decimal_ = mkNum <$> takeWhile1P (Just "digit") Char.isDigit
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a c = a * 10 + fromIntegral (Char.digitToInt c)

-- | Parse an integer in octal representation. Representation of octal
-- number is expected to be according to the Haskell report except for the
-- fact that this parser doesn't parse “0o” or “0O” prefix. It is a
-- responsibility of the programmer to parse correct prefix before parsing
-- the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > octal = char '0' >> char' 'o' >> L.octal
--
-- __Note__: before version 6.0.0 the function returned 'Integer', i.e. it
-- wasn't polymorphic in its return type.

octal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
octal = mkNum
  <$> takeWhile1P Nothing Char.isOctDigit
  <?> "octal integer"
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a c = a * 8 + fromIntegral (Char.digitToInt c)
{-# INLINEABLE octal #-}

-- | Parse an integer in hexadecimal representation. Representation of
-- hexadecimal number is expected to be according to the Haskell report
-- except for the fact that this parser doesn't parse “0x” or “0X” prefix.
-- It is a responsibility of the programmer to parse correct prefix before
-- parsing the number itself.
--
-- For example you can make it conform to the Haskell report like this:
--
-- > hexadecimal = char '0' >> char' 'x' >> L.hexadecimal
--
-- __Note__: before version 6.0.0 the function returned 'Integer', i.e. it
-- wasn't polymorphic in its return type.

hexadecimal
  :: forall e s m a. (MonadParsec e s m, Token s ~ Char, Integral a)
  => m a
hexadecimal = mkNum
  <$> takeWhile1P Nothing Char.isHexDigit
  <?> "hexadecimal integer"
  where
    mkNum    = foldl' step 0 . chunkToTokens (Proxy :: Proxy s)
    step a c = a * 16 + fromIntegral (Char.digitToInt c)
{-# INLINEABLE hexadecimal #-}

-- | Parse a floating point value as a 'Scientific' number. 'Scientific' is
-- great for parsing of arbitrary precision numbers coming from an untrusted
-- source. See documentation in "Data.Scientific" for more information.
--
-- The parser can be used to parse integers or floating point values. Use
-- functions like 'Data.Scientific.floatingOrInteger' from "Data.Scientific"
-- to test and extract integer or real values.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.
--
-- @since 5.0.0

scientific
  :: forall e s m. (MonadParsec e s m, Token s ~ Char)
  => m Scientific
scientific = do
  c'      <- decimal_
  SP c e' <- option (SP c' 0) (dotDecimal_ (Proxy :: Proxy s) c')
  e       <- option e' (exponent_ e')
  return (Sci.scientific c e)
{-# INLINEABLE scientific #-}

data SP = SP !Integer {-# UNPACK #-} !Int

-- | Parse a floating point number according to the syntax for floating
-- point literals described in the Haskell report.
--
-- This function does not parse sign, if you need to parse signed numbers,
-- see 'signed'.
--
-- __Note__: before version 6.0.0 the function returned 'Double', i.e. it
-- wasn't polymorphic in its return type.
--
-- __Note__: in versions 6.0.0–6.1.1 this function accepted plain integers.

float :: (MonadParsec e s m, Token s ~ Char, RealFloat a) => m a
float = do
  c' <- decimal_
  Sci.toRealFloat <$>
    ((do SP c e' <- dotDecimal_ (Proxy :: Proxy s) c'
         e       <- option e' (exponent_ e')
         return (Sci.scientific c e))
     <|> (Sci.scientific c' <$> exponent_ 0))
{-# INLINEABLE float #-}

dotDecimal_ :: (MonadParsec e s m, Token s ~ Char)
  => Proxy s
  -> Integer
  -> m SP
dotDecimal_ pxy c' = do
  void (C.char '.')
  let mkNum    = foldl' step (SP c' 0) . chunkToTokens pxy
      step (SP a e') c = SP
        (a * 10 + fromIntegral (Char.digitToInt c))
        (e' - 1)
  mkNum <$> takeWhile1P (Just "digit") Char.isDigit
{-# INLINE dotDecimal_ #-}

exponent_ :: (MonadParsec e s m, Token s ~ Char)
  => Int
  -> m Int
exponent_ e' = do
  void (C.char' 'e')
  (+ e') <$> signed (return ()) decimal_
{-# INLINE exponent_ #-}

-- | @'signed' space p@ parser parses an optional sign character (“+” or
-- “-”), then if there is a sign it consumes optional white space (using
-- @space@ parser), then it runs parser @p@ which should return a number.
-- Sign of the number is changed according to the previously parsed sign
-- character.
--
-- For example, to parse signed integer you can write:
--
-- > lexeme        = L.lexeme spaceConsumer
-- > integer       = lexeme L.decimal
-- > signedInteger = L.signed spaceConsumer integer

signed :: (MonadParsec e s m, Token s ~ Char, Num a)
  => m ()              -- ^ How to consume white space after the sign
  -> m a               -- ^ How to parse the number itself
  -> m a               -- ^ Parser for signed numbers
signed spc p = ($) <$> option id (lexeme spc sign) <*> p
  where
    sign = (id <$ C.char '+') <|> (negate <$ C.char '-')
{-# INLINEABLE signed #-}
