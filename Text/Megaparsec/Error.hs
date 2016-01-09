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

module Text.Megaparsec.Error
  ( Message (..)
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
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
import Data.Foldable (foldMap)
import Data.Monoid
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
  = Unexpected !String -- ^ Parser ran into an unexpected token
  | Expected   !String -- ^ What is expected instead
  | Message    !String -- ^ General-purpose error message component
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
  deriving (Eq, Typeable)

instance Show ParseError where
  show e = show (errorPos e) ++ ":\n" ++ showMessages (errorMessages e)

instance Monoid ParseError where
  mempty  = newErrorUnknown (initialPos "")
  mappend = mergeError

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

newErrorMessages :: [Message] -> SourcePos -> ParseError
newErrorMessages ms pos = addErrorMessages ms $ newErrorUnknown pos

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

-- | @addErrorMessages ms err@ returns @err@ with messages @ms@ added. The
-- function is defined in terms of 'addErrorMessage'.

addErrorMessages :: [Message] -> ParseError -> ParseError
addErrorMessages ms err = foldr addErrorMessage err ms

-- | @setErrorMessage m err@ returns @err@ with message @m@ added. This
-- function also deletes all existing error messages that were created with
-- the same constructor as @m@. If message @m@ has empty message string, the
-- function does not add the message to the result (it still deletes all
-- messages of the same type, though).

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage m (ParseError pos ms) =
  if badMessage m then err else addErrorMessage m err
  where err = ParseError pos xs
        xs  = filter ((/= fromEnum m) . fromEnum) ms

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

-- | @showMessages ms@ transforms list of error messages @ms@ into
-- their textual representation.

showMessages :: [Message] -> String
showMessages [] = "unknown parse error"
showMessages ms = tail $ foldMap (fromMaybe "") (zipWith f ns rs)
  where (unexpected,    ms') = span ((== 0) . fromEnum) ms
        (expected, messages) = span ((== 1) . fromEnum) ms'
        f prefix m = (prefix ++) <$> m
        ns = ["\nunexpected ","\nexpecting ","\n"]
        rs = (renderMsgs orList <$> [unexpected, expected]) ++
             [renderMsgs (intercalate "\n") messages]

-- | Render collection of messages. If the collection is empty, return
-- 'Nothing', otherwise return textual representation of the messages inside
-- 'Just'.

renderMsgs
  :: ([String] -> String) -- ^ Function to combine results
  -> [Message]         -- ^ Collection of messages to render
  -> Maybe String      -- ^ Result, if any
renderMsgs _ [] = Nothing
renderMsgs f ms = Just . f $ messageString <$> ms

-- | Print a pretty list where items are separated with commas and the word
-- “or” according to rules of English punctuation.

orList :: [String] -> String
orList []    = ""
orList [x]   = x
orList [x,y] = x ++ " or " ++ y
orList xs    = intercalate ", " (init xs) ++ ", or " ++ last xs
