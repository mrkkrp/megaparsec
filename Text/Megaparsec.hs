-- |
-- Module      :  Text.Megaparsec
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started writing a parser.
-- If you are new to Megaparsec and don't know where to begin, take a look
-- at our tutorials <https://mrkkrp.github.io/megaparsec/tutorials.html>.
--
-- By default this module is set up to parse character data. If you'd like to
-- parse the result of your own tokenizer you should start with the following
-- imports:
--
-- > import Text.Megaparsec.Prim
-- > import Text.Megaparsec.Combinator
--
-- Then you can implement your own version of 'satisfy' on top of the
-- 'token' primitive.
--
-- Typical import section looks like this:
--
-- > import Text.Megaparsec
-- > import Text.Megaparsec.String
-- > -- import Text.Megaparsec.ByteString
-- > -- import Text.Megaparsec.ByteString.Lazy
-- > -- import Text.Megaparsec.Text
-- > -- import Text.Megaparsec.Text.Lazy
--
-- As you can see the second import depends on the data type you want to use
-- as input stream. It just defines the useful type-synonym @Parser@.
--
-- Megaparsec 5 uses some type-level machinery to provide flexibility
-- without compromising on type safety. Thus type signatures are sometimes
-- necessary to avoid ambiguous types. If you're seeing a error message that
-- reads like “Ambiguous type variable @e0@ arising from … prevents the
-- constraint @(ErrorComponent e0)@ from being resolved”, you need to give
-- an explicit signature to your parser to resolve the ambiguity. It's a
-- good idea to provide type signatures for all top-level definitions.
--
-- Megaparsec is capable of a lot. Apart from this standard functionality
-- you can parse permutation phrases with "Text.Megaparsec.Perm",
-- expressions with "Text.Megaparsec.Expr", and even entire languages with
-- "Text.Megaparsec.Lexer". These modules should be imported explicitly
-- along with the two modules mentioned above.

module Text.Megaparsec
  ( -- * Running parser
    Parsec
  , ParsecT
  , parse
  , parseMaybe
  , parseTest
  , runParser
  , runParser'
  , runParserT
  , runParserT'
    -- * Combinators
  , (A.<|>)
  -- $assocbo
  , A.many
  -- $many
  , A.some
  -- $some
  , A.optional
  -- $optional
  , unexpected
  , match
  , region
  , failure
  , (<?>)
  , label
  , hidden
  , try
  , lookAhead
  , notFollowedBy
  , withRecovery
  , observing
  , eof
  , token
  , tokens
  , between
  , choice
  , count
  , count'
  , eitherP
  , endBy
  , endBy1
  , manyTill
  , someTill
  , option
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany
  , skipSome
    -- * Character parsing
  , newline
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
  , char
  , char'
  , anyChar
  , oneOf
  , oneOf'
  , noneOf
  , noneOf'
  , satisfy
  , string
  , string'
    -- * Textual source position
  , Pos
  , mkPos
  , unPos
  , unsafePos
  , InvalidPosException (..)
  , SourcePos (..)
  , initialPos
  , sourcePosPretty
    -- * Error messages
  , ErrorItem (..)
  , ErrorComponent (..)
  , Dec (..)
  , ParseError (..)
  , ShowToken (..)
  , ShowErrorComponent (..)
  , parseErrorPretty
    -- * Debugging
  , dbg
    -- * Low-level operations
  , Stream (..)
  , State (..)
  , getInput
  , setInput
  , getPosition
  , getNextTokenPosition
  , setPosition
  , pushPosition
  , popPosition
  , getTokensProcessed
  , setTokensProcessed
  , getTabWidth
  , setTabWidth
  , getParserState
  , setParserState
  , updateParserState )
where

import qualified Control.Applicative as A

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

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
-- @many p@ applies the parser @p@ /zero/ or more times and returns a list
-- of the returned values of @p@. Note that if the @p@ parser fails
-- consuming input, then the entire @many p@ parser fails with the error
-- message @p@ produced instead of just stopping iterating. In these cases
-- wrapping @p@ with 'try' may be desirable.
--
-- > identifier = (:) <$> letter <*> many (alphaNumChar <|> char '_')

-- $some
--
-- @some p@ applies the parser @p@ /one/ or more times and returns a list of
-- the returned values of @p@. The note about behavior of the combinator in
-- case when @p@ fails consuming input (see 'A.many') applies to 'some' as
-- well.
--
-- > word = some letter

-- $optional
--
-- @optional p@ tries to apply parser @p@. It will parse @p@ or nothing. It
-- only fails if @p@ fails after consuming input. On success result of @p@
-- is returned inside of 'Just', on failure 'Nothing' is returned.
