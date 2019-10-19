{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.CSV.Attoparsec
  ( parseCSV )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8 hiding (sepBy1)
import Data.ByteString (ByteString)
import Data.Vector (Vector)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8            as BC8
import qualified Data.Vector                      as V

import Control.Applicative.Combinators
-- NOTE We use the combinators from this module to make both implementations
-- as close as possible. The combinators from the ‘parser-combinators’
-- packages are not slower than Attoparsec's.

type Record = Vector Field
type Field  = ByteString

parseCSV :: ByteString -> [Record]
parseCSV bs =
  case parseOnly csv bs of
    Left err -> error err
    Right x -> x

csv :: Parser [Record]
csv = do
  xs <- sepEndBy1 record endOfLine
  endOfInput
  return xs

record :: Parser Record
record = do
  endAlready <- atEnd
  when endAlready empty -- to prevent reading empty line at the end of file
  V.fromList <$!> (sepBy1 field (char ',') <?> "record")

field :: Parser Field
field = (escapedField <|> unescapedField) <?> "field"

escapedField :: Parser ByteString
escapedField =
  BC8.pack <$!> between (char '"') (char '"') (many $ normalChar <|> escapedDq)
  where
    normalChar = notChar '"' <?> "unescaped character"
    escapedDq  = '"' <$ string "\"\""

unescapedField :: Parser ByteString
unescapedField =
  A.takeWhile (`notElem` (",\"\n\r" :: String))
{-# INLINE unescapedField #-}
