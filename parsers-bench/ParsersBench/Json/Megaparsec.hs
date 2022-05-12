{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Json.Megaparsec
  ( parseJson,
  )
where

import Control.Applicative
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Void
import Data.Word (Word8)
import ParsersBench.Json.Common
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

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
#define COLON 58

parseJson :: ByteString -> Value
parseJson bs =
  case parse json "" bs of
    Left err -> error (errorBundlePretty err)
    Right x -> x

json :: Parser Value
json = json_ object_ array_

json_ :: Parser Value -> Parser Value -> Parser Value
json_ obj ary = do
  w <- space *> (char OPEN_CURLY <|> char OPEN_SQUARE)
  if w == OPEN_CURLY
    then obj
    else ary
{-# INLINE json_ #-}

object_ :: Parser Value
object_ = Object <$> objectValues jstring value

objectValues :: Parser Text -> Parser Value -> Parser (H.HashMap Text Value)
objectValues str val = do
  space
  let pair = liftA2 (,) (str <* space) (char COLON *> space *> val)
  H.fromList <$> commaSeparated pair CLOSE_CURLY
{-# INLINE objectValues #-}

array_ :: Parser Value
array_ = Array <$> arrayValues value

arrayValues :: Parser Value -> Parser (Vector Value)
arrayValues val = do
  space
  V.fromList <$> commaSeparated val CLOSE_SQUARE
{-# INLINE arrayValues #-}

commaSeparated :: Parser a -> Word8 -> Parser [a]
commaSeparated item endByte = do
  w <- lookAhead anySingle
  if w == endByte
    then [] <$ anySingle
    else loop
  where
    loop = do
      v <- item <* space
      ch <- char COMMA <|> char endByte
      if ch == COMMA
        then space >> (v :) <$> loop
        else return [v]
{-# INLINE commaSeparated #-}

value :: Parser Value
value = do
  w <- lookAhead anySingle
  case w of
    DOUBLE_QUOTE -> anySingle *> (String <$> jstring_)
    OPEN_CURLY -> anySingle *> object_
    OPEN_SQUARE -> anySingle *> array_
    C_f -> Bool False <$ string "false"
    C_t -> Bool True <$ string "true"
    C_n -> string "null" *> pure Null
    _
      | w >= C_0 && w <= C_9 || w == C_MINUS -> Number <$> L.scientific
      | otherwise -> fail "not a valid json value"

jstring :: Parser Text
jstring = char DOUBLE_QUOTE *> jstring_

jstring_ :: Parser Text
jstring_ =
  TE.decodeUtf8
    <$> takeWhileP (Just "string char") (/= DOUBLE_QUOTE)
    <* char DOUBLE_QUOTE
{-# INLINE jstring_ #-}
