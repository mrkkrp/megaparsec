-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors.

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances  #-}

module Text.Megaparsec.Error
  ( MessageItem    (..)
  , ErrorComponent (..)
  , ParseError     (..)
  , ShowToken      (..)
  , ShowErrorComponent (..)
  , parseErrorPretty
  , sourcePosStackPretty )
where

import Control.Monad.Catch
import Data.Foldable (concat)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Monoid (Monoid (..))
#endif

-- | Data type that represents default components of parse error message.
-- The data type is parametrized over token type @t@.

data MessageItem t
  = Token t
  | TokenStream (NonEmpty t)
  | Label (NonEmpty Char)
  | EndOfInput
  deriving (Show, Read, Eq, Ord, Typeable)

-- | The type class defines how to represent information about various
-- exceptional situations in given data type. Data types that are used as
-- custom data component in 'ParseError' must be instances of this type
-- class.

class Ord e => ErrorComponent e where

  -- | Represent message passed to 'fail' in parser monad.

  representFail :: String -> e

  -- | Represent exception thrown in parser monad. (It implements
  -- 'Control.Monad.Catch.MonadThrow').

  representException :: Exception e' => e' -> e

  -- | Represent information about incorrect indentation.

  representIndentation
    :: Pos             -- ^ Actual indentation level
    -> Pos             -- ^ Expected indentation level
    -> e

instance ErrorComponent [Char] where
  representFail = id
  representException = ("exception: " ++) . show
  representIndentation = undefined -- TODO

-- TODO More instances?

-- | The data type @ParseError@ represents parse errors. It provides the
-- stack of source positions, set of expected and unexpected tokens as well
-- as set of custom associated data. The data type is parametrized over
-- token type @t@ and custom data @e@.
--
-- Note that stack of source positions contains current position as its
-- head, and the rest of positions allows to track full sequence of include
-- files with topmost source file at the end of the list.
--
-- 'Semigroup' (or 'Monoid') instance of the data type allows to merge parse
-- errors from different branches of parsing. When merging two
-- 'ParseError's, longest match is preferred; if positions are the same,
-- collections of message items are combined.

data ParseError t e = ParseError
  { errorPos        :: NonEmpty SourcePos  -- ^ Stack of source positions
  , errorUnexpected :: Set (MessageItem t) -- ^ Unexpected items
  , errorExpected   :: Set (MessageItem t) -- ^ Expected items
  , errorData       :: Set e               -- ^ Associated data, if any
  } deriving (Show, Read, Eq, Typeable)

instance (Ord t, Ord e) => Semigroup (ParseError t e) where
  (<>) = mergeError
  {-# INLINE (<>) #-}

instance (Ord t, Ord e) => Monoid (ParseError t e) where
  mempty  = ParseError (initialPos "" :| []) E.empty E.empty E.empty
  mappend = (<>)
  {-# INLINE mappend #-}

instance (Show t, Typeable t, Show e, Typeable e) => Exception (ParseError t e)

-- | Merge two error data structures into one joining their collections of
-- message items and preferring longest match. In other words, earlier error
-- message is discarded. This may seem counter-intuitive, but @mergeError@
-- is only used to merge error messages of alternative branches of parsing
-- and in this case longest match should be preferred.

mergeError :: (Ord t, Ord e)
  => ParseError t e -> ParseError t e -> ParseError t e
mergeError e1@(ParseError pos1 u1 p1 x1) e2@(ParseError pos2 u2 p2 x2) =
  case pos1 `compare` pos2 of
    LT -> e2
    EQ -> ParseError pos1 (E.union u1 u2) (E.union p1 p2) (E.union x1 x2)
    GT -> e1
{-# INLINE mergeError #-}

-- | Type class 'ShowToken' includes methods that allow to pretty-print
-- single token as well as stream of tokens. This is used for rendering of
-- error messages.

class ShowToken a where

  -- | Pretty-print given token. This is used to get token representation
  -- to use in error messages.

  showToken :: a -> String

  -- | Pretty-print non-empty stream of tokens.

  showTokenStream :: NonEmpty a -> String

instance ShowToken Char where
  showToken       = charPretty
  showTokenStream = stringPretty

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.

charPretty :: Char -> String
charPretty '\0' = "null"
charPretty '\a' = "bell"
charPretty '\b' = "backspace"
charPretty '\t' = "tab"
charPretty '\n' = "newline"
charPretty '\v' = "vertical tab"
charPretty '\f' = "form feed"
charPretty '\r' = "carriage return"
charPretty ' '  = "space"
charPretty x    = "'" ++ [x] ++ "'"

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" ++ NE.toList xs ++ "\""

-- | The type class defines how to print custom data component of
-- 'ParseError'.

class ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> String

instance ShowToken t => ShowErrorComponent (MessageItem t) where
  showErrorComponent (Token        t) = showToken t
  showErrorComponent (TokenStream ts) = showTokenStream ts
  showErrorComponent (Label    label) = NE.toList label
  showErrorComponent EndOfInput       = "end of input"

instance ShowErrorComponent String where
  showErrorComponent = id

-- TODO Instances

-- | Pretty-print 'ParseError'.

parseErrorPretty :: (ShowToken t, ShowErrorComponent e)
  => ParseError t e    -- ^ Parse error to render
  -> String            -- ^ Result of rendering
parseErrorPretty (ParseError pos us ps xs) =
  sourcePosStackPretty pos ++ ":\n" ++
  if E.null us && E.null ps && E.null xs
    then "unknown parse error\n"
    else concat
      [ messageItemsPretty "unexpected " us
      , messageItemsPretty "expecting "  ps
      , unlines (showErrorComponent <$> E.toAscList xs) ]

-- | Pretty-print stack of source positions.

sourcePosStackPretty :: NonEmpty SourcePos -> String
sourcePosStackPretty ms = concatMap f rest ++ sourcePosPretty pos
  where (pos :| rest') = ms
        rest           = reverse rest'
        f p = "in file included from " ++ sourcePosPretty p ++ "\n"

-- | @messagesPretty ms@ transforms list of error messages @ms@ into
-- their textual representation.

messageItemsPretty :: ShowErrorComponent t
  => String            -- ^ Prefix to prepend
  -> Set t             -- ^ Collection of messages
  -> String            -- ^ Result of rendering
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . E.toAscList . E.map showErrorComponent
    in prefix ++ f ts ++ "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to rules of English punctuation.

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x ++ " or " ++ y
orList xs       = intercalate ", " (NE.init xs) ++ ", or " ++ NE.last xs
