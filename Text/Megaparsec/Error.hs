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

import Text.Megaparsec.Pos

-- | This data type represents parse error messages. There are three kinds
-- of messages:
--
-- > data Message = Unexpected String > | Expected String > | Message String
--
-- The fine distinction between different kinds of parse errors allows the
-- system to generate quite good error messages for the user.

data Message = Unexpected !String
             | Expected   !String
             | Message    !String
               deriving Show

instance Enum Message where
    fromEnum (Unexpected _) = 0
    fromEnum (Expected   _) = 1
    fromEnum (Message    _) = 2
    toEnum _ = error "Text.Megaparsec.Error: toEnum is undefined for Message"

instance Eq Message where
    m1 == m2 =
        fromEnum m1 == fromEnum m2 && messageString m1 == messageString m2

instance Ord Message where
    compare m1 m2 =
        case compare (fromEnum m1) (fromEnum m2) of
          LT -> LT
          EQ -> compare (messageString m1) (messageString m2)
          GT -> GT

-- | Extract the message string from an error message

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
    { -- | Extract the source position from the parse error.
      errorPos :: !SourcePos
      -- | Extract the list of error messages from the parse error.
    , errorMessages :: [Message] }

instance Show ParseError where
    show e = show (errorPos e) ++ ":\n" ++ showMessages (errorMessages e)

instance Eq ParseError where
    l == r = errorPos l == errorPos r && errorMessages l == errorMessages r

-- | Test whether given @ParseError@ has associated collection of error
-- messages. Return @True@ if it has none and @False@ otherwise.

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError _ ms) = null ms

-- Creation of parse errors

-- | @newErrorUnknown pos@ creates @ParseError@ without any associated
-- message but with specified position @pos@.

newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos = ParseError pos []

-- | @newErrorMessage m pos@ creates @ParseError@ with message @m@ and
-- associated position @pos@. If message @m@ has empty message string, it
-- won't be included.

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage m pos = ParseError pos $ bool [m] [] (badMessage m)

-- | @addErrorMessage m err@ returns @ParseError@ @err@ with message @m@
-- added. This function makes sure that list of messages is always sorted
-- and doesn't contain duplicates or messages with empty message strings.

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage m (ParseError pos ms) =
    ParseError pos $ bool (pre ++ [m] ++ post) ms (badMessage m)
    where pre  = filter (< m) ms
          post = filter (> m) ms

-- | @setErrorMessage m err@ returns @err@ with message @m@ added. This
-- function also deletes all existing error messages that were created with
-- the same constructor as @m@. If message @m@ has empty message string, the
-- function just returns the original @err@.

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage m e@(ParseError pos ms) =
    bool (addErrorMessage m $ ParseError pos xs) e (badMessage m)
    where xs = filter ((/= fromEnum m) . fromEnum) ms

-- | @setErrorPos pos err@ returns @ParseError@ identical to @err@, but with
-- position @pos@.

setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos pos (ParseError _ ms) = ParseError pos ms

-- | Merge two error data structures into one joining their collections of
-- messages and preferring shortest match.

mergeError :: ParseError -> ParseError -> ParseError
mergeError e1@(ParseError pos1 ms1) e2@(ParseError pos2 ms2) =
    case pos1 `compare` pos2 of
      LT -> e1
      EQ -> foldr addErrorMessage (ParseError pos1 ms1) ms2
      GT -> e2

-- | @showMessages ms@ transforms list of error messages @ms@ into
-- their textual representation.

showMessages :: [Message] -> String
showMessages [] = "unknown parse error"
showMessages ms = intercalate "\n" $
                  filter (not . null) [unexpected', expected', messages']
    where (unexpected,    ms') = span ((== 0) . fromEnum) ms
          (expected, messages) = span ((== 1) . fromEnum) ms'

          unexpected' = showMany "unexpected " unexpected
          expected'   = showMany "expecting "  expected
          messages'   = showMany ""            messages

          showMany pre msgs =
            case messageString <$> msgs of
              [] -> ""
              xs | null pre  -> commasOr xs
                 | otherwise -> pre ++ commasOr xs

          commasOr []  = ""
          commasOr [x] = x
          commasOr xs  = intercalate ", " (init xs) ++ " or " ++ last xs
