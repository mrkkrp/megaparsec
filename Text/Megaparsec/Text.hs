-- |
-- Module      :  Text.Megaparsec.Text
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2011 Antoine Latter
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with 'Text.Text'.

module Text.Megaparsec.Text
  ( Parser
  , parseFromFile )
where

import Text.Megaparsec.Error
import Text.Megaparsec.Prim
import qualified Data.Text as T
import qualified Data.Text.IO as T

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strict text.

type Parser = Parsec T.Text

-- | @parseFromFile p filePath@ runs a lazy text parser @p@ on the
-- input read from @filePath@ using 'Data.Text.IO.readFile'. Returns either
-- a 'ParseError' ('Left') or a value of type @a@ ('Right').
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Left err -> print err
-- >     Right xs -> print (sum xs)

parseFromFile :: Parser a -> String -> IO (Either ParseError a)
parseFromFile p fname = runParser p fname <$> T.readFile fname
