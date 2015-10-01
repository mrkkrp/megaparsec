-- |
-- Module      :  Text.Megaparsec.String
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Make Strings an instance of 'Stream' with 'Char' token type.

module Text.Megaparsec.String
  ( Parser
  , parseFromFile )
where

import Text.Megaparsec.Error
import Text.Megaparsec.Prim

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strings.

type Parser = Parsec String

-- | @parseFromFile p filePath@ runs a string parser @p@ on the
-- input read from @filePath@ using 'Prelude.readFile'. Returns either a
-- 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Left err -> print err
-- >     Right xs -> print (sum xs)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runParser p fname `fmap` readFile fname
