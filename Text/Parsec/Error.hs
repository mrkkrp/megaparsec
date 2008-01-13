-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Error
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Parse errors
-- 
-----------------------------------------------------------------------------

module Text.Parsec.Error
    ( Message ( SysUnExpect, UnExpect, Expect, Message )
    , messageString
    , ParseError, errorPos, errorMessages, errorIsUnknown
    , showErrorMessages
    , newErrorMessage, newErrorUnknown
    , addErrorMessage, setErrorPos, setErrorMessage
    , mergeError
    ) where

import Data.List ( nub, sortBy )

import Text.Parsec.Pos

-- | Messages

data Message = SysUnExpect !String -- @ library generated unexpect
             | UnExpect    !String -- @ unexpected something
             | Expect      !String -- @ expecting something
             | Message     !String -- @ raw message

instance Enum Message where
    fromEnum (SysUnExpect _) = 0
    fromEnum (UnExpect    _) = 1
    fromEnum (Expect      _) = 2
    fromEnum (Message     _) = 3
    toEnum _ = error "toEnum is undefined for Message"

instance Eq Message where
    m1 == m2 = fromEnum m1 == fromEnum m2

instance Ord Message where
    compare msg1 msg2 = compare (fromEnum msg1) (fromEnum msg2)

messageString :: Message -> String
messageString (SysUnExpect s) = s
messageString (UnExpect    s) = s
messageString (Expect      s) = s
messageString (Message     s) = s

-- | Parse Errors

data ParseError = ParseError !SourcePos [Message]

errorPos :: ParseError -> SourcePos
errorPos (ParseError pos msgs)
    = pos

errorMessages :: ParseError -> [Message]
errorMessages (ParseError pos msgs)
    = sortBy compare msgs

errorIsUnknown :: ParseError -> Bool
errorIsUnknown (ParseError pos msgs)
    = null msgs

-- | Create parse errors

newErrorUnknown :: SourcePos -> ParseError
newErrorUnknown pos
    = ParseError pos []

newErrorMessage :: Message -> SourcePos -> ParseError
newErrorMessage msg pos
    = ParseError pos [msg]

addErrorMessage :: Message -> ParseError -> ParseError
addErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg:msgs)

setErrorPos :: SourcePos -> ParseError -> ParseError
setErrorPos pos (ParseError _ msgs)
    = ParseError pos msgs

setErrorMessage :: Message -> ParseError -> ParseError
setErrorMessage msg (ParseError pos msgs)
    = ParseError pos (msg : filter (msg /=) msgs)

mergeError :: ParseError -> ParseError -> ParseError
mergeError (ParseError pos msgs1) (ParseError _ msgs2)
    = ParseError pos (msgs1 ++ msgs2)

-- | Show parse errors

instance Show ParseError where
    show err
        = show (errorPos err) ++ ":" ++
          showErrorMessages "or" "unknown parse error"
                            "expecting" "unexpected" "end of input"
                           (errorMessages err)

-- Language indipendent show function

showErrorMessages ::
    String -> String -> String -> String -> String -> [Message] -> String
showErrorMessages msgOr msgUnknown msgExpecting msgUnExpected msgEndOfInput msgs
    | null msgs = msgUnknown
    | otherwise = concat $ map ("\n"++) $ clean $
                 [showSysUnExpect,showUnExpect,showExpect,showMessages]
    where
      (sysUnExpect,msgs1) = span ((SysUnExpect "") ==) msgs
      (unExpect,msgs2)    = span ((UnExpect    "") ==) msgs1
      (expect,messages)   = span ((Expect      "") ==) msgs2

      showExpect      = showMany msgExpecting expect
      showUnExpect    = showMany msgUnExpected unExpect
      showSysUnExpect | not (null unExpect) ||
                        null sysUnExpect = ""
                      | null firstMsg    = msgUnExpected ++ " " ++ msgEndOfInput
                      | otherwise        = msgUnExpected ++ " " ++ firstMsg
          where
              firstMsg  = messageString (head sysUnExpect)

      showMessages      = showMany "" messages

      -- helpers
      showMany pre msgs = case clean (map messageString msgs) of
                            [] -> ""
                            ms | null pre  -> commasOr ms
                               | otherwise -> pre ++ " " ++ commasOr ms

      commasOr []       = ""
      commasOr [m]      = m
      commasOr ms       = commaSep (init ms) ++ " " ++ msgOr ++ " " ++ last ms

      commaSep          = seperate ", " . clean
      semiSep           = seperate "; " . clean

      seperate sep []     = ""
      seperate sep [m]    = m
      seperate sep (m:ms) = m ++ sep ++ seperate sep ms

      clean             = nub . filter (not . null)
