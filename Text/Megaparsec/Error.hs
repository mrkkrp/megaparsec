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

module Text.Megaparsec.Error
  ( Message (..)
  , isUnexpected
  , isExpected
  , isMessage
  , messageString
  , badMessage
  , ParseError
  , errorPos
  , errorMessages
  , errorIsUnknown
  , newErrorMessage
  , newErrorMessages
  , newErrorUnknown
  , addErrorMessage
  , addErrorMessages
  , setErrorMessage
  , setErrorPos
  , mergeError
  , showMessages )
where

import Control.Exception (Exception)
import Data.Foldable (find, concat)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, fromJust)
import Data.Semigroup (Semigroup((<>)))
import Data.Typeable (Typeable)
import Prelude hiding (concat)
import qualified Data.List.NonEmpty as NE

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Foldable (foldMap)
import Data.Monoid (Monoid(..))
#endif

-- | This data type represents parse error messages.

data Message
  = Unexpected !String -- ^ Parser ran into an unexpected token
  | Expected   !String -- ^ What is expected instead
  | Message    !String -- ^ General-purpose error message component
  deriving (Show, Eq, Ord)

-- | Check if given 'Message' is created with 'Unexpected' constructor.
--
-- @since 4.4.0

isUnexpected :: Message -> Bool
isUnexpected (Unexpected _) = True
isUnexpected _              = False
{-# INLINE isUnexpected #-}

-- | Check if given 'Message' is created with 'Expected' constructor.
--
-- @since 4.4.0

isExpected :: Message -> Bool
isExpected (Expected _) = True
isExpected _            = False
{-# INLINE isExpected #-}

-- | Check if given 'Message' is created with 'Message' constructor.
--
-- @since 4.4.0

isMessage :: Message -> Bool
isMessage (Message _) = True
isMessage _           = False
{-# INLINE isMessage #-}

-- | Extract the message string from an error message.

messageString :: Message -> String
messageString (Unexpected s) = s
messageString (Expected   s) = s
messageString (Message    s) = s
{-# INLINE messageString #-}

-- | Test if message string is empty.

badMessage :: Message -> Bool
badMessage = null . messageString
{-# INLINE badMessage #-}

-- | The data type @ParseError@ represents parse errors. It provides the
-- source position ('SourcePos') of the error and a list of error messages
-- ('Message').

data ParseError = ParseError
  { -- | Extract the source position from 'ParseError'.
    errorPos :: !SourcePos
    -- | Extract the list of error messages from 'ParseError'.
  , errorMessages :: [Message] }
  deriving (Eq, Typeable)

instance Show ParseError where
  show e = show (errorPos e) ++ ":\n" ++ showMessages (errorMessages e)

instance Monoid ParseError where
  mempty  = newErrorUnknown (initialPos "")
  mappend = (<>)

instance Semigroup ParseError where
  (<>) = mergeError

instance Exception ParseError

-- | Test whether given 'ParseError' has associated collection of error
-- messages. Return @True@ if it has none and @False@ otherwise.

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError _ ms) = null ms

-- | @newErrorMessage m pos@ creates 'ParseError' with message @m@ and
-- associated position @pos@. If message @m@ has empty message string, it
-- won't be included.

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage m = newErrorMessages [m]

-- | @newErrorMessages ms pos@ creates 'ParseError' with messages @ms@ and
-- associated position @pos@.
--
-- @since 4.2.0

newErrorMessages :: [Message] -> SourcePos -> ParseError
newErrorMessages ms pos = addErrorMessages ms (newErrorUnknown pos)

-- | @newErrorUnknown pos@ creates 'ParseError' without any associated
-- message but with specified position @pos@.

newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos = ParseError pos []

-- | @addErrorMessage m err@ returns @err@ with message @m@ added. This
-- function makes sure that list of messages is always sorted and doesn't
-- contain duplicates or messages with empty message strings.

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage m (ParseError pos ms) =
  ParseError pos $ if badMessage m then ms else pre ++ [m] ++ post
  where pre  = filter (< m) ms
        post = filter (> m) ms
{-# INLINE addErrorMessage #-}

-- | @addErrorMessages ms err@ returns @err@ with messages @ms@ added. The
-- function is defined in terms of 'addErrorMessage'.
--
-- @since 4.2.0

addErrorMessages :: [Message] -> ParseError -> ParseError
addErrorMessages ms err = foldr addErrorMessage err ms
{-# INLINE addErrorMessages #-}

-- | @setErrorMessage m err@ returns @err@ with message @m@ added. This
-- function also deletes all existing error messages that were created with
-- the same constructor as @m@. If message @m@ has empty message string, the
-- function does not add the message to the result (it still deletes all
-- messages of the same type, though).

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage m (ParseError pos ms) =
  if badMessage m then err else addErrorMessage m err
  where err = ParseError pos (filter (not . f) ms)
        f   = fromJust $ find ($ m) [isUnexpected, isExpected, isMessage]

-- | @setErrorPos pos err@ returns 'ParseError' identical to @err@, but with
-- position @pos@.

setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos pos (ParseError _ ms) = ParseError pos ms

-- | Merge two error data structures into one joining their collections of
-- messages and preferring longest match. In other words, earlier error
-- message is discarded. This may seem counter-intuitive, but @mergeError@
-- is only used to merge error messages of alternative branches of parsing
-- and in this case longest match should be preferred.

mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError pos1 _) e2@(ParseError pos2 ms2) =
  case pos1 `compare` pos2 of
    LT -> e2
    EQ -> addErrorMessages ms2 e1
    GT -> e1
{-# INLINE mergeError #-}

-- | @showMessages ms@ transforms list of error messages @ms@ into
-- their textual representation.

showMessages :: [Message] -> String
showMessages [] = "unknown parse error"
showMessages ms = tail $ foldMap (fromMaybe "") (zipWith f ns rs)
  where (unexpected,    ms') = span isUnexpected ms
        (expected, messages) = span isExpected   ms'
        f prefix m = (prefix ++) <$> m
        ns = ["\nunexpected ","\nexpecting ","\n"]
        rs = (renderMsgs orList <$> [unexpected, expected]) ++
             [renderMsgs (concat . NE.intersperse "\n") messages]

-- | Render collection of messages. If the collection is empty, return
-- 'Nothing', otherwise return textual representation of the messages inside
-- 'Just'.

renderMsgs
  :: (NonEmpty String -> String) -- ^ Function to combine results
  -> [Message]         -- ^ Collection of messages to render
  -> Maybe String      -- ^ Result, if any
-- renderMsgs _ [] = Nothing
renderMsgs f ms = f . fmap messageString <$> NE.nonEmpty ms

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to rules of English punctuation.

orList :: NonEmpty String -> String
orList (x:|[])  = x
orList (x:|[y]) = x ++ " or " ++ y
orList xs       = intercalate ", " (NE.init xs) ++ ", or " ++ NE.last xs
