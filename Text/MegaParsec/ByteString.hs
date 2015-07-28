-- |
-- Module      :  Text.MegaParsec.ByteString
-- Copyright   :  © 2007 Paolo Martini, © 2015 MegaParsec contributors
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  provisional
-- Portability :  portable
--
-- Convenience definitions for working with 'C.ByteString's.

module Text.MegaParsec.ByteString
    ( Parser
    , GenParser
    , parseFromFile )
where

import Text.MegaParsec.Error
import Text.MegaParsec.Prim

import qualified Data.ByteString.Char8 as C

type Parser         = Parsec C.ByteString ()
type GenParser t st = Parsec C.ByteString st

-- | @parseFromFile p filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'ByteString.Char8.readFile'. Returns
-- either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- @
--  main = do
--    result <- parseFromFile numbers "digits.txt"
--    case result of
--      Left err  -> print err
--      Right xs  -> print (sum xs)
-- @

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runP p () fname <$> C.readFile fname
