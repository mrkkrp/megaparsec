-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015–2016 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors. Current version of Megaparsec supports well-typed errors
-- instead of 'String'-based ones. This gives a lot of flexibility in
-- describing what exactly went wrong as well as a way to return arbitrary
-- data in case of failure.

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE FlexibleInstances  #-}

module Text.Megaparsec.Error
  ( ErrorItem (..)
  , ErrorComponent (..)
  , Dec (..)
  , ParseError (..)
  , ShowToken (..)
  , ShowErrorComponent (..)
  , parseErrorPretty
  , sourcePosStackPretty )
where

import Control.DeepSeq
import Control.Monad.Catch
import Data.Data (Data)
import Data.Foldable (concat)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

-- | Data type that is used to represent “unexpected\/expected” items in
-- parse error. The data type is parametrized over token type @t@.
--
-- @since 5.0.0

data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance NFData t => NFData (ErrorItem t)

-- | The type class defines how to represent information about various
-- exceptional situations. Data types that are used as custom data component
-- in 'ParseError' must be instances of this type class.
--
-- @since 5.0.0

class Ord e => ErrorComponent e where

  -- | Represent message passed to 'fail' in parser monad.
  --
  -- @since 5.0.0

  representFail :: String -> e

  -- | Represent information about incorrect indentation.
  --
  -- @since 5.0.0

  representIndentation
    :: Ordering -- ^ Desired ordering between reference level and actual level
    -> Pos             -- ^ Reference indentation level
    -> Pos             -- ^ Actual indentation level
    -> e

-- | “Default error component”. This in our instance of 'ErrorComponent'
-- provided out-of-box.
--
-- @since 5.0.0

data Dec
  = DecFail String         -- ^ 'fail' has been used in parser monad
  | DecIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  deriving (Show, Read, Eq, Ord, Data, Typeable)

instance NFData Dec where
  rnf (DecFail str) = rnf str
  rnf (DecIndentation ord ref act) = ord `seq` rnf ref `seq` rnf act

instance ErrorComponent Dec where
  representFail        = DecFail
  representIndentation = DecIndentation

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
-- custom data sets and collections of message items are combined.

data ParseError t e = ParseError
  { errorPos        :: NonEmpty SourcePos -- ^ Stack of source positions
  , errorUnexpected :: Set (ErrorItem t)  -- ^ Unexpected items
  , errorExpected   :: Set (ErrorItem t)  -- ^ Expected items
  , errorCustom     :: Set e              -- ^ Associated data, if any
  } deriving (Show, Read, Eq, Data, Typeable, Generic)

instance (NFData t, NFData e) => NFData (ParseError t e)

instance (Ord t, Ord e) => Semigroup (ParseError t e) where
  (<>) = mergeError
  {-# INLINE (<>) #-}

instance (Ord t, Ord e) => Monoid (ParseError t e) where
  mempty  = ParseError (initialPos "" :| []) E.empty E.empty E.empty
  mappend = (<>)
  {-# INLINE mappend #-}

instance ( Show t
         , Typeable t
         , Ord t
         , ShowToken t
         , Show e
         , Typeable e
         , ShowErrorComponent e )
  => Exception (ParseError t e) where
#if MIN_VERSION_base(4,8,0)
  displayException = parseErrorPretty
#endif

-- | Merge two error data structures into one joining their collections of
-- message items and preferring longest match. In other words, earlier error
-- message is discarded. This may seem counter-intuitive, but 'mergeError'
-- is only used to merge error messages of alternative branches of parsing
-- and in this case longest match should be preferred.

mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
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

  -- | Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).
  --
  -- @since 5.0.0

  showTokens :: NonEmpty a -> String

instance ShowToken Char where
  showTokens = stringPretty

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> String
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" ++ NE.toList xs ++ "\""

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

-- | The type class defines how to print custom data component of
-- 'ParseError'.
--
-- @since 5.0.0

class Ord a => ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> String

instance (Ord t, ShowToken t) => ShowErrorComponent (ErrorItem t) where
  showErrorComponent (Tokens   ts) = showTokens ts
  showErrorComponent (Label label) = NE.toList label
  showErrorComponent EndOfInput    = "end of input"

instance ShowErrorComponent Dec where
  showErrorComponent (DecFail msg) = msg
  showErrorComponent (DecIndentation ord ref actual) =
    "incorrect indentation (got " ++ show (unPos actual) ++
    ", should be " ++ p ++ show (unPos ref) ++ ")"
    where p = case ord of
                LT -> "less than "
                EQ -> "equal to "
                GT -> "greater than "

-- | Pretty-print 'ParseError'. Note that rendered 'String' always ends with
-- a newline.
--
-- @since 5.0.0

parseErrorPretty :: ( Ord t
                    , ShowToken t
                    , ShowErrorComponent e )
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
--
-- @since 5.0.0

sourcePosStackPretty :: NonEmpty SourcePos -> String
sourcePosStackPretty ms = concatMap f rest ++ sourcePosPretty pos
  where (pos :| rest') = ms
        rest           = reverse rest'
        f p = "in file included from " ++ sourcePosPretty p ++ ",\n"

-- | Transforms list of error messages into their textual representation.

messageItemsPretty :: ShowErrorComponent a
  => String            -- ^ Prefix to prepend
  -> Set a             -- ^ Collection of messages
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
