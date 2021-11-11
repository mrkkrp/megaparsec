{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors. The current version of Megaparsec supports typed errors
-- instead of 'String'-based ones. This gives a lot of flexibility in
-- describing what exactly went wrong as well as a way to return arbitrary
-- data in case of failure.
--
-- You probably do not want to import this module directly because
-- "Text.Megaparsec" re-exports it anyway.
module Text.Megaparsec.Error
  ( -- * Parse error type
    ErrorItem (..),
    ErrorFancy (..),
    ParseError (..),
    mapParseError,
    errorOffset,
    setErrorOffset,
    ParseErrorBundle (..),
    attachSourcePos,

    -- * Pretty-printing
    ShowErrorComponent (..),
    errorBundlePretty,
    parseErrorPretty,
    parseErrorTextPretty,
  )
where

import Control.DeepSeq
import Control.Exception
import Control.Monad.State.Strict
import Data.Data (Data)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isNothing)
import Data.Proxy
import Data.Set (Set)
import qualified Data.Set as E
import Data.Typeable (Typeable)
import Data.Void
import GHC.Generics
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import Text.Megaparsec.Stream

----------------------------------------------------------------------------
-- Parse error type

-- | A data type that is used to represent “unexpected\/expected” items in
-- 'ParseError'. It is parametrized over the token type @t@.
--
-- @since 5.0.0
data ErrorItem t
  = -- | Non-empty stream of tokens
    Tokens (NonEmpty t)
  | -- | Label (cannot be empty)
    Label (NonEmpty Char)
  | -- | End of input
    EndOfInput
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)

instance NFData t => NFData (ErrorItem t)

-- | Additional error data, extendable by user. When no custom data is
-- necessary, the type is typically indexed by 'Void' to “cancel” the
-- 'ErrorCustom' constructor.
--
-- @since 6.0.0
data ErrorFancy e
  = -- | 'fail' has been used in parser monad
    ErrorFail String
  | -- | Incorrect indentation error: desired ordering between reference
    -- level and actual level, reference indentation level, actual
    -- indentation level
    ErrorIndentation Ordering Pos Pos
  | -- | Custom error data
    ErrorCustom e
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic, Functor)

instance NFData a => NFData (ErrorFancy a) where
  rnf (ErrorFail str) = rnf str
  rnf (ErrorIndentation ord ref act) = ord `seq` rnf ref `seq` rnf act
  rnf (ErrorCustom a) = rnf a

-- | @'ParseError' s e@ represents a parse error parametrized over the
-- stream type @s@ and the custom data @e@.
--
-- 'Semigroup' and 'Monoid' instances of the data type allow us to merge
-- parse errors from different branches of parsing. When merging two
-- 'ParseError's, the longest match is preferred; if positions are the same,
-- custom data sets and collections of message items are combined. Note that
-- fancy errors take precedence over trivial errors in merging.
--
-- @since 7.0.0
data ParseError s e
  = -- | Trivial errors, generated by the Megaparsec's machinery. The data
    -- constructor includes the offset of error, unexpected token (if any),
    -- and expected tokens.
    --
    -- Type of the first argument was changed in the version /7.0.0/.
    TrivialError Int (Maybe (ErrorItem (Token s))) (Set (ErrorItem (Token s)))
  | -- | Fancy, custom errors.
    --
    -- Type of the first argument was changed in the version /7.0.0/.
    FancyError Int (Set (ErrorFancy e))
  deriving (Typeable, Generic)

deriving instance
  ( Show (Token s),
    Show e
  ) =>
  Show (ParseError s e)

deriving instance
  ( Eq (Token s),
    Eq e
  ) =>
  Eq (ParseError s e)

deriving instance
  ( Data s,
    Data (Token s),
    Ord (Token s),
    Data e,
    Ord e
  ) =>
  Data (ParseError s e)

instance
  ( NFData (Token s),
    NFData e
  ) =>
  NFData (ParseError s e)

instance (Stream s, Ord e) => Semigroup (ParseError s e) where
  (<>) = mergeError
  {-# INLINE (<>) #-}

instance (Stream s, Ord e) => Monoid (ParseError s e) where
  mempty = TrivialError 0 Nothing E.empty
  mappend = (<>)
  {-# INLINE mappend #-}

instance
  ( Show s,
    Show (Token s),
    Show e,
    ShowErrorComponent e,
    VisualStream s,
    Typeable s,
    Typeable e
  ) =>
  Exception (ParseError s e)
  where
  displayException = parseErrorPretty

-- | Modify the custom data component in a parse error. This could be done
-- via 'fmap' if not for the 'Ord' constraint.
--
-- @since 7.0.0
mapParseError ::
  Ord e' =>
  (e -> e') ->
  ParseError s e ->
  ParseError s e'
mapParseError _ (TrivialError o u p) = TrivialError o u p
mapParseError f (FancyError o x) = FancyError o (E.map (fmap f) x)

-- | Get the offset of a 'ParseError'.
--
-- @since 7.0.0
errorOffset :: ParseError s e -> Int
errorOffset (TrivialError o _ _) = o
errorOffset (FancyError o _) = o

-- | Set the offset of a 'ParseError'.
--
-- @since 8.0.0
setErrorOffset :: Int -> ParseError s e -> ParseError s e
setErrorOffset o (TrivialError _ u p) = TrivialError o u p
setErrorOffset o (FancyError _ x) = FancyError o x

-- | Merge two error data structures into one joining their collections of
-- message items and preferring the longest match. In other words, earlier
-- error message is discarded. This may seem counter-intuitive, but
-- 'mergeError' is only used to merge error messages of alternative branches
-- of parsing and in this case longest match should be preferred.
mergeError ::
  (Stream s, Ord e) =>
  ParseError s e ->
  ParseError s e ->
  ParseError s e
mergeError e1 e2 =
  case errorOffset e1 `compare` errorOffset e2 of
    LT -> e2
    EQ ->
      case (e1, e2) of
        (TrivialError s1 u1 p1, TrivialError _ u2 p2) ->
          TrivialError s1 (n u1 u2) (E.union p1 p2)
        (FancyError {}, TrivialError {}) -> e1
        (TrivialError {}, FancyError {}) -> e2
        (FancyError s1 x1, FancyError _ x2) ->
          FancyError s1 (E.union x1 x2)
    GT -> e1
  where
    -- NOTE The logic behind this merging is that since we only combine
    -- parse errors that happen at exactly the same position, all the
    -- unexpected items will be prefixes of input stream at that position or
    -- labels referring to the same thing. Our aim here is to choose the
    -- longest prefix (merging with labels and end of input is somewhat
    -- arbitrary, but is necessary because otherwise we can't make
    -- ParseError lawful Monoid and have nice parse errors at the same
    -- time).
    n Nothing Nothing = Nothing
    n (Just x) Nothing = Just x
    n Nothing (Just y) = Just y
    n (Just x) (Just y) = Just (max x y)
{-# INLINE mergeError #-}

-- | A non-empty collection of 'ParseError's equipped with 'PosState' that
-- allows us to pretty-print the errors efficiently and correctly.
--
-- @since 7.0.0
data ParseErrorBundle s e = ParseErrorBundle
  { -- | A collection of 'ParseError's that is sorted by parse error offsets
    bundleErrors :: NonEmpty (ParseError s e),
    -- | The state that is used for line\/column calculation
    bundlePosState :: PosState s
  }
  deriving (Generic)

deriving instance
  ( Show s,
    Show (Token s),
    Show e
  ) =>
  Show (ParseErrorBundle s e)

deriving instance
  ( Eq s,
    Eq (Token s),
    Eq e
  ) =>
  Eq (ParseErrorBundle s e)

deriving instance
  ( Typeable s,
    Typeable (Token s),
    Typeable e
  ) =>
  Typeable (ParseErrorBundle s e)

deriving instance
  ( Data s,
    Data (Token s),
    Ord (Token s),
    Data e,
    Ord e
  ) =>
  Data (ParseErrorBundle s e)

instance
  ( NFData s,
    NFData (Token s),
    NFData e
  ) =>
  NFData (ParseErrorBundle s e)

instance
  ( Show s,
    Show (Token s),
    Show e,
    ShowErrorComponent e,
    VisualStream s,
    TraversableStream s,
    Typeable s,
    Typeable e
  ) =>
  Exception (ParseErrorBundle s e)
  where
  displayException = errorBundlePretty

-- | Attach 'SourcePos'es to items in a 'Traversable' container given that
-- there is a projection allowing us to get an offset per item.
--
-- Items must be in ascending order with respect to their offsets.
--
-- @since 7.0.0
attachSourcePos ::
  (Traversable t, TraversableStream s) =>
  -- | How to project offset from an item (e.g. 'errorOffset')
  (a -> Int) ->
  -- | The collection of items
  t a ->
  -- | Initial 'PosState'
  PosState s ->
  -- | The collection with 'SourcePos'es added and the final 'PosState'
  (t (a, SourcePos), PosState s)
attachSourcePos projectOffset xs = runState (traverse f xs)
  where
    f a = do
      pst <- get
      let pst' = reachOffsetNoLine (projectOffset a) pst
      put pst'
      return (a, pstateSourcePos pst')
{-# INLINEABLE attachSourcePos #-}

----------------------------------------------------------------------------
-- Pretty-printing

-- | The type class defines how to print a custom component of 'ParseError'.
--
-- @since 5.0.0
class Ord a => ShowErrorComponent a where
  -- | Pretty-print a component of 'ParseError'.
  showErrorComponent :: a -> String

  -- | Length of the error component in characters, used for highlighting of
  -- parse errors in input string.
  --
  -- @since 7.0.0
  errorComponentLen :: a -> Int
  errorComponentLen _ = 1

instance ShowErrorComponent Void where
  showErrorComponent = absurd

-- | Pretty-print a 'ParseErrorBundle'. All 'ParseError's in the bundle will
-- be pretty-printed in order together with the corresponding offending
-- lines by doing a single pass over the input stream. The rendered 'String'
-- always ends with a newline.
--
-- @since 7.0.0
errorBundlePretty ::
  forall s e.
  ( VisualStream s,
    TraversableStream s,
    ShowErrorComponent e
  ) =>
  -- | Parse error bundle to display
  ParseErrorBundle s e ->
  -- | Textual rendition of the bundle
  String
errorBundlePretty ParseErrorBundle {..} =
  let (r, _) = foldl f (id, bundlePosState) bundleErrors
   in drop 1 (r "")
  where
    f ::
      (ShowS, PosState s) ->
      ParseError s e ->
      (ShowS, PosState s)
    f (o, !pst) e = (o . (outChunk ++), pst')
      where
        (msline, pst') = reachOffset (errorOffset e) pst
        epos = pstateSourcePos pst'
        outChunk =
          "\n" <> sourcePosPretty epos <> ":\n"
            <> offendingLine
            <> parseErrorTextPretty e
        offendingLine =
          case msline of
            Nothing -> ""
            Just sline ->
              let rpadding =
                    if pointerLen > 0
                      then replicate rpshift ' '
                      else ""
                  pointerLen =
                    if rpshift + elen > slineLen
                      then slineLen - rpshift + 1
                      else elen
                  pointer = replicate pointerLen '^'
                  lineNumber = (show . unPos . sourceLine) epos
                  padding = replicate (length lineNumber + 1) ' '
                  rpshift = unPos (sourceColumn epos) - 1
                  slineLen = length sline
               in padding <> "|\n" <> lineNumber <> " | " <> sline
                    <> "\n"
                    <> padding
                    <> "| "
                    <> rpadding
                    <> pointer
                    <> "\n"
        pxy = Proxy :: Proxy s
        elen =
          case e of
            TrivialError _ Nothing _ -> 1
            TrivialError _ (Just x) _ -> errorItemLength pxy x
            FancyError _ xs ->
              E.foldl' (\a b -> max a (errorFancyLength b)) 1 xs

-- | Pretty-print a 'ParseError'. The rendered 'String' always ends with a
-- newline.
--
-- @since 5.0.0
parseErrorPretty ::
  (VisualStream s, ShowErrorComponent e) =>
  -- | Parse error to render
  ParseError s e ->
  -- | Result of rendering
  String
parseErrorPretty e =
  "offset=" <> show (errorOffset e) <> ":\n" <> parseErrorTextPretty e

-- | Pretty-print a textual part of a 'ParseError', that is, everything
-- except for its position. The rendered 'String' always ends with a
-- newline.
--
-- @since 5.1.0
parseErrorTextPretty ::
  forall s e.
  (VisualStream s, ShowErrorComponent e) =>
  -- | Parse error to render
  ParseError s e ->
  -- | Result of rendering
  String
parseErrorTextPretty (TrivialError _ us ps) =
  if isNothing us && E.null ps
    then "unknown parse error\n"
    else
      messageItemsPretty "unexpected " (showErrorItem pxy `E.map` maybe E.empty E.singleton us)
        <> messageItemsPretty "expecting " (showErrorItem pxy `E.map` ps)
  where
    pxy = Proxy :: Proxy s
parseErrorTextPretty (FancyError _ xs) =
  if E.null xs
    then "unknown fancy parse error\n"
    else unlines (showErrorFancy <$> E.toAscList xs)

----------------------------------------------------------------------------
-- Helpers

-- | Pretty-print an 'ErrorItem'.
showErrorItem :: VisualStream s => Proxy s -> ErrorItem (Token s) -> String
showErrorItem pxy = \case
  Tokens ts -> showTokens pxy ts
  Label label -> NE.toList label
  EndOfInput -> "end of input"

-- | Get length of the “pointer” to display under a given 'ErrorItem'.
errorItemLength :: VisualStream s => Proxy s -> ErrorItem (Token s) -> Int
errorItemLength pxy = \case
  Tokens ts -> tokensLength pxy ts
  _ -> 1

-- | Pretty-print an 'ErrorFancy'.
showErrorFancy :: ShowErrorComponent e => ErrorFancy e -> String
showErrorFancy = \case
  ErrorFail msg -> msg
  ErrorIndentation ord ref actual ->
    "incorrect indentation (got " <> show (unPos actual)
      <> ", should be "
      <> p
      <> show (unPos ref)
      <> ")"
    where
      p = case ord of
        LT -> "less than "
        EQ -> "equal to "
        GT -> "greater than "
  ErrorCustom a -> showErrorComponent a

-- | Get length of the “pointer” to display under a given 'ErrorFancy'.
errorFancyLength :: ShowErrorComponent e => ErrorFancy e -> Int
errorFancyLength = \case
  ErrorCustom a -> errorComponentLen a
  _ -> 1

-- | Transform a list of error messages into their textual representation.
messageItemsPretty ::
  -- | Prefix to prepend
  String ->
  -- | Collection of messages
  Set String ->
  -- | Result of rendering
  String
messageItemsPretty prefix ts
  | E.null ts = ""
  | otherwise =
      prefix <> (orList . NE.fromList . E.toAscList) ts <> "\n"

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to the rules of English punctuation.
orList :: NonEmpty String -> String
orList (x :| []) = x
orList (x :| [y]) = x <> " or " <> y
orList xs = intercalate ", " (NE.init xs) <> ", or " <> NE.last xs
