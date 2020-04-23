{-# LANGUAGE OverloadedStrings #-}

module ParsersBench.Log.Megaparsec
  ( parseLog,
  )
where

import Control.Monad
import Data.ByteString (ByteString)
import Data.Char (chr)
import Data.Time
import Data.Void
import Data.Word (Word8)
import ParsersBench.Log.Common
import Text.Megaparsec
import Text.Megaparsec.Byte
import qualified Text.Megaparsec.Byte.Lexer as L

type Parser = Parsec Void ByteString

parseLog :: ByteString -> Log
parseLog bs =
  case parse logParser "" bs of
    Left err -> error (errorBundlePretty err)
    Right x -> x

parseIP :: Parser IP
parseIP = do
  d1 <- L.decimal
  void (char 46)
  d2 <- L.decimal
  void (char 46)
  d3 <- L.decimal
  void (char 46)
  d4 <- L.decimal
  return (IP d1 d2 d3 d4)

timeParser :: Parser LocalTime
timeParser = do
  y <- fmap byteToChar <$> count 4 digitChar
  void (char 45)
  mm <- fmap byteToChar <$> count 2 digitChar
  void (char 45)
  d <- fmap byteToChar <$> count 2 digitChar
  void (char 32)
  h <- fmap byteToChar <$> count 2 digitChar
  void (char 58)
  m <- fmap byteToChar <$> count 2 digitChar
  void (char 58)
  s <- fmap byteToChar <$> count 2 digitChar
  return
    LocalTime
      { localDay = fromGregorian (read y) (read mm) (read d),
        localTimeOfDay = TimeOfDay (read h) (read m) (read s)
      }

productParser :: Parser Product
productParser =
  (Mouse <$ string "mouse")
    <|> (Keyboard <$ string "keyboard")
    <|> (Monitor <$ string "monitor")
    <|> (Speakers <$ string "speakers")

logEntryParser :: Parser LogEntry
logEntryParser = do
  t <- timeParser
  void (char 32)
  ip <- parseIP
  void (char 32)
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser Log
logParser = many (logEntryParser <* eol)

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral
{-# INLINE byteToChar #-}
