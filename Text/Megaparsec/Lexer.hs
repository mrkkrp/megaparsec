{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Safe #-}

-- |
-- Module      :  Text.Megaparsec.Common
-- Copyright   :  © 2018–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Common token combinators. This module is not public, the functions from
-- it are re-exported in "Text.Megaparsec.Byte" and "Text.Megaparsec.Char".
--
-- @since 7.0.0
module Text.Megaparsec.Lexer
  ( -- * White space
    space,
    lexeme,
    symbol,
    symbol',
  )
where

import qualified Data.CaseInsensitive as CI
import Text.Megaparsec
import Text.Megaparsec.Common

----------------------------------------------------------------------------
-- White space

-- | @'space' sc lineComment blockComment@ produces a parser that can parse
-- white space in general. It's expected that you create such a parser once
-- and pass it to other functions in this module as needed (when you see
-- @spaceConsumer@ in documentation, usually it means that something like
-- 'space' is expected there).
--
-- @sc@ is used to parse blocks of space characters. You can use
-- 'Text.Megaparsec.Char.space1' from "Text.Megaparsec.Char" for this
-- purpose as well as your own parser (if you don't want to automatically
-- consume newlines, for example). Make sure that the parser does not
-- succeed on the empty input though. In an earlier version of the library
-- 'Text.Megaparsec.Char.spaceChar' was recommended, but now parsers based
-- on 'takeWhile1P' are preferred because of their speed.
--
-- @lineComment@ is used to parse line comments. You can use
-- @skipLineComment@ if you don't need anything special.
--
-- @blockComment@ is used to parse block (multi-line) comments. You can use
-- @skipBlockComment@ or @skipBlockCommentNested@ if you don't need anything
-- special.
--
-- If you don't want to allow a kind of comment, simply pass 'empty' which
-- will fail instantly when parsing of that sort of comment is attempted and
-- 'space' will just move on or finish depending on whether there is more
-- white space for it to consume.
space ::
  MonadParsec e s m =>
  -- | A parser for space characters which does not accept empty
  -- input (e.g. 'Text.Megaparsec.Char.space1')
  m () ->
  -- | A parser for a line comment (e.g. 'skipLineComment')
  m () ->
  -- | A parser for a block comment (e.g. 'skipBlockComment')
  m () ->
  m ()
space sp line block =
  skipMany $
    choice
      [hidden sp, hidden line, hidden block]
{-# INLINEABLE space #-}

-- | This is a wrapper for lexemes. The typical usage is to supply the first
-- argument (parser that consumes white space, probably defined via 'space')
-- and use the resulting function to wrap parsers for every lexeme.
--
-- > lexeme  = L.lexeme spaceConsumer
-- > integer = lexeme L.decimal
lexeme ::
  MonadParsec e s m =>
  -- | How to consume white space after lexeme
  m () ->
  -- | How to parse actual lexeme
  m a ->
  m a
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
symbol ::
  MonadParsec e s m =>
  -- | How to consume white space after lexeme
  m () ->
  -- | Symbol to parse
  Tokens s ->
  m (Tokens s)
symbol spc = lexeme spc . string
{-# INLINEABLE symbol #-}

-- | A case-insensitive version of 'symbol'. This may be helpful if you're
-- working with case-insensitive languages.
symbol' ::
  (MonadParsec e s m, CI.FoldCase (Tokens s)) =>
  -- | How to consume white space after lexeme
  m () ->
  -- | Symbol to parse (case-insensitive)
  Tokens s ->
  m (Tokens s)
symbol' spc = lexeme spc . string'
{-# INLINEABLE symbol' #-}
