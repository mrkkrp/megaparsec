-- |
-- Module      :  Text.Megaparsec.ByteString
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with 'C.ByteString'.

module Text.Megaparsec.ByteString
  ( Parser
  , parseFromFile )
where

import Text.Megaparsec.Error
import Text.Megaparsec.Prim

import qualified Data.ByteString.Char8 as C

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strict bytestrings.

type Parser = Parsec C.ByteString

-- | @parseFromFile p filePath@ runs a strict bytestring parser @p@ on the
-- input read from @filePath@ using 'ByteString.Char8.readFile'. Returns
-- either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Left err -> print err
-- >     Right xs -> print (sum xs)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runParser p fname `fmap` C.readFile fname
