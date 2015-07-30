-- |
-- Module      :  Text.MegaParsec.String
-- Copyright   :  © 2007 Paolo Martini, © 2015 MegaParsec contributors
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Make Strings an instance of 'Stream' with 'Char' token type.

module Text.MegaParsec.String
    ( Parser
    , GenParser
    , parseFromFile )
where

import Text.MegaParsec.Error
import Text.MegaParsec.Prim

type Parser           = Parsec String ()
type GenParser tok st = Parsec [tok] st

-- | @parseFromFile p filePath@ runs a string parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a
-- 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- @
--  main = do
--    result <- parseFromFile numbers "digits.txt"
--    case result of
--      Left err  -> print err
--      Right xs  -> print (sum xs)
-- @

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runParser p () fname <$> readFile fname
