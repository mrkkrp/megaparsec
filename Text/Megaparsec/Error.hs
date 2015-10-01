-- |
-- Module      :  Text.Megaparsec.Error
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Parse errors.

module Text.Megaparsec.Error
  ( Message (..)
  , messageString
  , badMessage
  , ParseError
  , errorPos
  , errorMessages
  , errorIsUnknown
  , newErrorMessage
  , newErrorUnknown
  , addErrorMessage
  , setErrorMessage
  , setErrorPos
  , mergeError
  , showMessages )
where

import Data.Bool (bool)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Foldable (foldMap)
#endif

-- | This data type represents parse error messages. There are three kinds
-- of messages:
--
-- > data Message = Unexpected String
-- >              | Expected   String
-- >              | Message    String
--
-- The fine distinction between different kinds of parse errors allows the
-- system to generate quite good error messages for the user.

data Message
  = Unexpected !String
  | Expected   !String
  | Message    !String
  deriving (Show, Eq)

instance Enum Message where
  fromEnum (Unexpected _) = 0
  fromEnum (Expected   _) = 1
  fromEnum (Message    _) = 2
  toEnum _ = error "Text.Megaparsec.Error: toEnum is undefined for Message"

instance Ord Message where
  compare m1 m2 =
    case compare (fromEnum m1) (fromEnum m2) of
      LT -> LT
      EQ -> compare (messageString m1) (messageString m2)
      GT -> GT

-- | Extract the message string from an error message.

messageString :: Message -> String
messageString (Unexpected s) = s
messageString (Expected   s) = s
messageString (Message    s) = s

-- | Test if message string is empty.

badMessage :: Message -> Bool
badMessage = null . messageString

-- | The data type @ParseError@ represents parse errors. It provides the
-- source position ('SourcePos') of the error and a list of error messages
-- ('Message'). A @ParseError@ can be returned by the function
-- 'Text.Parsec.Prim.parse'. @ParseError@ is an instance of the 'Show' and
-- 'Eq' type classes.

data ParseError = ParseError
  { -- | Extract the source position from 'ParseError'.
    errorPos :: !SourcePos
    -- | Extract the list of error messages from 'ParseError'.
  , errorMessages :: [Message] }
  deriving Eq

instance Show ParseError where
  show e = show (errorPos e) ++ ":\n" ++ showMessages (errorMessages e)

-- | Test whether given 'ParseError' has associated collection of error
-- messages. Return @True@ if it has none and @False@ otherwise.

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError _ ms) = null ms

-- Creation of parse errors

-- | @newErrorUnknown pos@ creates 'ParseError' without any associated
-- message but with specified position @pos@.

newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos = ParseError pos []

-- | @newErrorMessage m pos@ creates 'ParseError' with message @m@ and
-- associated position @pos@. If message @m@ has empty message string, it
-- won't be included.

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage m pos = ParseError pos $ bool [m] [] (badMessage m)

-- | @addErrorMessage m err@ returns @err@ with message @m@ added. This
-- function makes sure that list of messages is always sorted and doesn't
-- contain duplicates or messages with empty message strings.

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage m (ParseError pos ms) =
  ParseError pos $ bool (pre ++ [m] ++ post) ms (badMessage m)
  where pre  = filter (< m) ms
        post = filter (> m) ms

-- | @setErrorMessage m err@ returns @err@ with message @m@ added. This
-- function also deletes all existing error messages that were created with
-- the same constructor as @m@. If message @m@ has empty message string, the
-- function does not add the message to the result (it still deletes all
-- messages of the same type, though).

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage m (ParseError pos ms) =
  bool (addErrorMessage m pe) pe (badMessage m)
  where pe = ParseError pos xs
        xs = filter ((/= fromEnum m) . fromEnum) ms

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
    EQ -> foldr addErrorMessage e1 ms2
    GT -> e1

-- | @showMessages ms@ transforms list of error messages @ms@ into
-- their textual representation.

showMessages :: [Message] -> String
showMessages [] = "unknown parse error"
showMessages ms = tail $ foldMap (fromMaybe "") (zipWith f ns rs)
  where (unexpected,    ms') = span ((== 0) . fromEnum) ms
        (expected, messages) = span ((== 1) . fromEnum) ms'
        f prefix m = (prefix ++) <$> m
        ns = ["\nunexpected ","\nexpecting ","\n"]
        rs = renderMsgs <$> [unexpected, expected, messages]

-- | Render collection of messages. If the collection is empty, return
-- 'Nothing', else return textual representation of the messages inside
-- 'Just'.

renderMsgs :: [Message] -> Maybe String
renderMsgs [] = Nothing
renderMsgs ms = Just . orList $ messageString <$> ms

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to rules of English punctuation.

orList :: [String] -> String
orList []    = ""
orList [x]   = x
orList [x,y] = x ++ " or " ++ y
orList xs    = intercalate ", " (init xs) ++ ", or " ++ last xs
