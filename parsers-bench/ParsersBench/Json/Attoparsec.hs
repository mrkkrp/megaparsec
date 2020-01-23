{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- Mostly stolen from (with simplifications):
-- https://github.com/bos/attoparsec/blob/master/benchmarks/Aeson.hs

module ParsersBench.Json.Attoparsec
  ( parseJson,
  )
where

import Control.Applicative
import qualified Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word8)
import ParsersBench.Json.Common

#define CLOSE_CURLY 125
#define CLOSE_SQUARE 93
#define COMMA 44
#define C_0 48
#define C_9 57
#define C_MINUS 45
#define C_f 102
#define C_n 110
#define C_t 116
#define DOUBLE_QUOTE 34
#define OPEN_CURLY 123
#define OPEN_SQUARE 91

parseJson :: ByteString -> Value
parseJson bs =
  case parseOnly json bs of
    Left err -> error err
    Right x -> x

json :: Parser Value
json = json_ object_ array_

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- skipSpace *> A.satisfy (\w -> w == OPEN_CURLY || w == OPEN_SQUARE)
  if w == OPEN_CURLY
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser Value
object_ = Object <$> objectValues jstring value

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
  skipSpace
  let pair = liftA2 (,) (str <* skipSpace) (char ':' *> skipSpace *> val)
  H.fromList <$> commaSeparated pair CLOSE_CURLY
{-# INLINE objectValues #-}

array_ :: Parser Value
array_ = Array <$> arrayValues value

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  skipSpace
  V.fromList <$> commaSeparated val CLOSE_SQUARE
{-# INLINE arrayValues #-}

commaSeparated :: Parser a -> Word8 -> Parser [a]
commaSeparated item endByte = do
  w <- A.peekWord8'
  if w == endByte
    then A.anyWord8 >> return []
    else loop
  where
    loop = do
      v <- item <* skipSpace
      ch <- A.satisfy $ \w -> w == COMMA || w == endByte
      if ch == COMMA
        then skipSpace >> (v :) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

value :: Parser Value
value = do
  w <- A.peekWord8'
  case w of
    DOUBLE_QUOTE -> A.anyWord8 *> (String <$> jstring_)
    OPEN_CURLY -> A.anyWord8 *> object_
    OPEN_SQUARE -> A.anyWord8 *> array_
    C_f -> string "false" *> pure (Bool False)
    C_t -> string "true" *> pure (Bool True)
    C_n -> string "null" *> pure Null
    _
      | w >= C_0 && w <= C_9 || w == C_MINUS -> Number <$> scientific
      | otherwise -> fail "not a valid json value"

jstring :: Parser Text
jstring = A.word8 DOUBLE_QUOTE *> jstring_

jstring_ :: Parser Text
jstring_ =
  TE.decodeUtf8
    <$> A.takeWhile (/= DOUBLE_QUOTE) <* A.word8 DOUBLE_QUOTE
{-# INLINE jstring_ #-}
