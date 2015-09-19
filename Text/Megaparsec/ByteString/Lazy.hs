-- |
-- Module      :  Text.Megaparsec.ByteString.Lazy
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with lazy 'C.ByteString'.

module Text.Megaparsec.ByteString.Lazy
  ( Parser
  , parseFromFile )
where

import Text.Megaparsec.Error
import Text.Megaparsec.Prim

import qualified Data.ByteString.Lazy.Char8 as C

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for lazy bytestrings.

type Parser = Parsec C.ByteString

-- | @parseFromFile p filePath@ runs a lazy bytestring parser @p@ on the
-- input read from @filePath@ using 'ByteString.Lazy.Char8.readFile'.
-- Returns either a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Left err -> print err
-- >     Right xs -> print (sum xs)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runParser p fname <$> C.readFile fname
