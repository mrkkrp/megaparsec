-- |
-- Module      :  Text.MegaParsec
-- Copyright   :  © 2015 MegaParsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started writing a parser.
--
-- By default this module is set up to parse character data. If you'd like to
-- parse the result of your own tokenizer you should start with the following
-- imports:
--
-- > import Text.MegaParsec.Prim
-- > import Text.MegaParsec.Combinator
--
-- Then you can implement your own version of 'satisfy' on top of the
-- 'tokenPrim' primitive.

module Text.MegaParsec
    (
-- * Parsers
      ParsecT
    , Parsec
    , token
    , tokens
    , runParserT
    , runParser
    , parse
    , parseMaybe
    , parseTest
    , getPosition
    , getInput
    , getState
    , putState
    , modifyState
-- * Combinators
    , (A.<|>)
    -- $assocbo
    , A.many
    -- $many
    , A.some
    -- $some
    , A.optional
    -- $optional
    , (<?>)
    , label
    , try
    , unexpected
    , choice
    , skipMany
    , skipSome
    , count
    , between
    , option
    , optionMaybe
    , sepBy
    , sepBy1
    , endBy
    , endBy1
    , sepEndBy
    , sepEndBy1
    , chainl
    , chainl1
    , chainr
    , chainr1
    , eof
    , notFollowedBy
    , manyTill
    , lookAhead
    , anyToken
-- * Character parsing
    , oneOf
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
    , string
-- * Error messages
    , ParseError
    , errorPos
-- * Position
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    , incSourceLine, incSourceColumn
    , setSourceLine, setSourceColumn, setSourceName
-- * Low-level operations
    , tokenPrim
    , unknownError
    , sysUnExpectError
    , mergeErrorReply
    , getParserState
    , setParserState
    , updateParserState
    , Stream (..)
    , runParsecT
    , mkPT
    , Consumed (..)
    , Reply (..)
    , State (..)
    , setPosition
    , setInput )
where

import qualified Control.Applicative as A

import Text.MegaParsec.Char
import Text.MegaParsec.Combinator
import Text.MegaParsec.Error
import Text.MegaParsec.Pos
import Text.MegaParsec.Prim

-- $assocbo
--
-- This combinator implements choice. The parser @p \<|> q@ first applies
-- @p@. If it succeeds, the value of @p@ is returned. If @p@ fails
-- /without consuming any input/, parser @q@ is tried.
--
-- The parser is called /predictive/ since @q@ is only tried when parser @p@
-- didn't consume any input (i.e. the look ahead is 1). This
-- non-backtracking behaviour allows for both an efficient implementation of
-- the parser combinators and the generation of good error messages.

-- $many
--
-- @many p@ applies the parser @p@ /zero/ or more times. Returns a list of
-- the returned values of @p@.
--
-- > identifier = (:) <$> letter <*> many (alphaNum <|> char '_')

-- $some
--
-- @some p@ applies the parser @p@ /one/ or more times. Returns a list of
-- the returned values of @p@.
--
-- > word = some letter

-- $optional
--
-- @optional p@ tries to apply parser @p@. It will parse @p@ or nothing. It
-- only fails if @p@ fails after consuming input. On success result of @p@
-- is returned inside of 'Just', on failure 'Nothing' is returned.
