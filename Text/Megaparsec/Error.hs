-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors. Current version of Megaparsec supports well-typed errors
-- instead of 'String'-based ones. This gives a lot of flexibility in
-- describing what exactly went wrong as well as a way to return arbitrary
-- data in case of failure.
--
-- You probably do not want to import this module because "Text.Megaparsec"
-- re-exports it anyway.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Megaparsec.Error
  ( ErrorItem (..)
  , ErrorData (..)
  , ParseError (..)
  , ShowErrorComponent (..)
  , parseErrorPretty
  , sourcePosStackPretty
  , parseErrorTextPretty )
where

import Control.DeepSeq
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Semigroup
import Data.Set (Set)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Prelude hiding (concat)
import Test.QuickCheck hiding (label)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as E
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TB
import qualified Data.Text.Lazy.Builder.Int as TB

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

-- | Data type that is used to represent “unexpected\/expected” items in
-- 'ParseError'. The data type is parametrized over the token type @t@.
--
-- @since 5.0.0

data ErrorItem t
  = Tokens (NonEmpty t)      -- ^ Non-empty stream of tokens
  | Label (NonEmpty Char)    -- ^ Label (cannot be empty)
  | EndOfInput               -- ^ End of input
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance NFData t => NFData (ErrorItem t)

instance Arbitrary t => Arbitrary (ErrorItem t) where
  arbitrary = oneof
    [
#if !MIN_VERSION_QuickCheck(2,9,0)
      Tokens <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , Label  <$> (NE.fromList . getNonEmpty <$> arbitrary)
#else
      Tokens <$> arbitrary
    , Label  <$> arbitrary
#endif
    , return EndOfInput ]

-- | Additional error data, extendable by the user.
--
-- @since 6.0.0

data ErrorData a
  = ErrorFail String
    -- ^ 'fail' has been used in parser monad
  | ErrorIndentation Ordering Pos Pos
    -- ^ Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
  | ErrorCustom a
    -- ^ Custom error data, can be conveniently disabled by indexing
    -- 'ErrorData' by 'Data.Void.Void'
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance NFData a => NFData (ErrorData a) where
  rnf (ErrorFail str) = rnf str
  rnf (ErrorIndentation ord ref act) = ord `seq` rnf ref `seq` rnf act
  rnf (ErrorCustom a) = rnf a

instance Arbitrary (ErrorData a) where
  arbitrary = oneof
    [ sized (\n -> do
        k <- choose (0, n `div` 2)
        ErrorFail <$> vectorOf k arbitrary)
    , ErrorIndentation <$> arbitrary <*> arbitrary <*> arbitrary ]

-- | 'ParseError' represents… parse errors. It provides the stack of source
-- positions, a set of expected and unexpected tokens as well as a set of
-- custom associated data. The data type is parametrized over the token type
-- @t@ and the custom data @e@.
--
-- Note that the stack of source positions contains current position as its
-- head, and the rest of positions allows to track full sequence of include
-- files with topmost source file at the end of the list.
--
-- 'Semigroup' (and 'Monoid') instance of the data type allows to merge
-- parse errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
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

-- instance ( Stream s
--          , Typeable (Token s)
--          , Show e
--          , Typeable e
--          , ShowErrorComponent e
--          , t ~ Token s )
--   => Exception (ParseError t e) where
-- #if MIN_VERSION_base(4,8,0)
--   displayException = T.unpack . parseErrorPretty
-- #endif

instance (Arbitrary t, Ord t, Arbitrary e, Ord e)
    => Arbitrary (ParseError t e) where
  arbitrary = ParseError
#if MIN_VERSION_QuickCheck(2,9,0)
    <$> arbitrary
#else
    <$> (NE.fromList . getNonEmpty <$> arbitrary)
#endif
#if MIN_VERSION_QuickCheck(2,8,2)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
#else
    <*> (E.fromList <$> arbitrary)
    <*> (E.fromList <$> arbitrary)
    <*> (E.fromList <$> arbitrary)
#endif

-- | Merge two error data structures into one joining their collections of
-- message items and preferring the longest match. In other words, earlier
-- error message is discarded. This may seem counter-intuitive, but
-- 'mergeError' is only used to merge error messages of alternative branches
-- of parsing and in this case longest match should be preferred.

mergeError :: (Ord t, Ord e)
  => ParseError t e
  -> ParseError t e
  -> ParseError t e
mergeError e1@(ParseError s1 u1 p1 x1) e2@(ParseError s2 u2 p2 x2) =
  case s1 `compare` s2 of
    LT -> e2
    EQ -> ParseError s1 (E.union u1 u2) (E.union p1 p2) (E.union x1 x2)
    GT -> e1
{-# INLINE mergeError #-}

-- | Type class 'ShowToken' includes methods that allow to pretty-print
-- single token as well as stream of tokens. This is used for rendering of
-- error messages.

class ShowToken a where

  -- | Pretty-print non-empty stream of tokens. This function is also used
  -- to print single tokens (represented as singleton lists).
  --
  -- @since 6.0.0

  showTokens :: NonEmpty a -> TB.Builder

instance ShowToken Char where
  showTokens = stringPretty

-- | The type class defines how to print custom data component of
-- 'ParseError'.
--
-- @since 5.0.0

class Ord a => ShowErrorComponent a where

  -- | Pretty-print custom data component of 'ParseError'.

  showErrorComponent :: a -> TB.Builder

instance (Ord t, ShowToken t) => ShowErrorComponent (ErrorItem t) where
  showErrorComponent (Tokens   ts) = showTokens ts
  showErrorComponent (Label label) = (TB.fromText . T.pack . NE.toList) label
  showErrorComponent EndOfInput    = "end of input"

instance ShowErrorComponent a => ShowErrorComponent (ErrorData a) where
  showErrorComponent (ErrorFail msg) = TB.fromText (T.pack msg)
  showErrorComponent (ErrorIndentation ord ref actual) =
    "incorrect indentation (got " <> TB.decimal (unPos actual) <>
    ", should be " <> p <> TB.decimal (unPos ref) <> ")"
    where
      p = case ord of
            LT -> "less than "
            EQ -> "equal to "
            GT -> "greater than "
  showErrorComponent (ErrorCustom a) = showErrorComponent a

-- | Pretty-print a 'ParseError'. The rendered 'String' always ends with a
-- newline.
--
-- @since 6.0.0

parseErrorPretty
  :: ( Ord t
     , ShowToken t
     , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> Text              -- ^ Result of rendering
parseErrorPretty e = TL.toStrict . TB.toLazyText $
  sourcePosStackPretty (errorPos e) <> ":\n" <> parseErrorTextPretty e

-- | Pretty-print a stack of source positions.
--
-- @since 6.0.0

sourcePosStackPretty :: NonEmpty SourcePos -> TB.Builder
sourcePosStackPretty ms = mconcat (f <$> rest) <> sourcePosPretty pos
  where
    (pos :| rest') = ms
    rest           = reverse rest'
    f p = "in file included from " <> sourcePosPretty p <> ",\n"

-- | Pretty-print a textual part of a 'ParseError', that is, everything
-- except stack of source positions. The rendered staring always ends with a
-- new line.
--
-- @since 6.0.0

parseErrorTextPretty
  :: ( Ord t
     , ShowToken t
     , ShowErrorComponent e )
  => ParseError t e    -- ^ Parse error to render
  -> TB.Builder        -- ^ Result of rendering
parseErrorTextPretty (ParseError _ us ps xs) =
  if E.null us && E.null ps && E.null xs
    then "unknown parse error\n"
    else mconcat
      [ messageItemsPretty "unexpected " us
      , messageItemsPretty "expecting "  ps
      , unlines' (showErrorComponent <$> E.toAscList xs) ]
  where
    unlines' []     = mempty
    unlines' (l:ls) = l <> "\n" <> unlines' ls

----------------------------------------------------------------------------
-- Helpers

-- | @stringPretty s@ returns pretty representation of string @s@. This is
-- used when printing string tokens in error messages.

stringPretty :: NonEmpty Char -> TB.Builder
stringPretty (x:|[])      = charPretty x
stringPretty ('\r':|"\n") = "crlf newline"
stringPretty xs           = "\"" <> mconcat (f <$> NE.toList xs) <> "\""
  where
    f ch =
      case charPretty' ch of
        Nothing     -> TB.singleton ch
        Just pretty -> "<" <> pretty <> ">"

-- | @charPretty ch@ returns user-friendly string representation of given
-- character @ch@, suitable for using in error messages.

charPretty :: Char -> TB.Builder
charPretty ' ' = "space"
charPretty ch = fromMaybe ("'" <> TB.singleton ch <> "'") (charPretty' ch)

-- | If the given character has a pretty representation, return that,
-- otherwise 'Nothing'. This is an internal helper.

charPretty' :: Char -> Maybe TB.Builder
charPretty' '\NUL' = pure "null"
charPretty' '\SOH' = pure "start of heading"
charPretty' '\STX' = pure "start of text"
charPretty' '\ETX' = pure "end of text"
charPretty' '\EOT' = pure "end of transmission"
charPretty' '\ENQ' = pure "enquiry"
charPretty' '\ACK' = pure "acknowledge"
charPretty' '\BEL' = pure "bell"
charPretty' '\BS'  = pure "backspace"
charPretty' '\t'   = pure "tab"
charPretty' '\n'   = pure "newline"
charPretty' '\v'   = pure "vertical tab"
charPretty' '\f'   = pure "form feed"
charPretty' '\r'   = pure "carriage return"
charPretty' '\SO'  = pure "shift out"
charPretty' '\SI'  = pure "shift in"
charPretty' '\DLE' = pure "data link escape"
charPretty' '\DC1' = pure "device control one"
charPretty' '\DC2' = pure "device control two"
charPretty' '\DC3' = pure "device control three"
charPretty' '\DC4' = pure "device control four"
charPretty' '\NAK' = pure "negative acknowledge"
charPretty' '\SYN' = pure "synchronous idle"
charPretty' '\ETB' = pure "end of transmission block"
charPretty' '\CAN' = pure "cancel"
charPretty' '\EM'  = pure "end of medium"
charPretty' '\SUB' = pure "substitute"
charPretty' '\ESC' = pure "escape"
charPretty' '\FS'  = pure "file separator"
charPretty' '\GS'  = pure "group separator"
charPretty' '\RS'  = pure "record separator"
charPretty' '\US'  = pure "unit separator"
charPretty' '\DEL' = pure "delete"
charPretty' '\160' = pure "non-breaking space"
charPretty' _      = Nothing

-- | Transforms a list of error messages into their textual representation.

messageItemsPretty :: ShowErrorComponent a
  => TB.Builder        -- ^ Prefix to prepend
  -> Set a             -- ^ Collection of messages
  -> TB.Builder        -- ^ Result of rendering
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
    let f = orList . NE.fromList . E.toAscList . E.map showErrorComponent
    in prefix <> f ts <> "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.

orList :: NonEmpty TB.Builder -> TB.Builder
orList (x:|[])  = x
orList (x:|[y]) = x <> " or " <> y
orList xs       = intercalate (NE.init xs) <> ", or " <> NE.last xs
  where
    intercalate []     = ""
    intercalate [a]    = a
    intercalate (a:as) = a <> ", " <> intercalate as
