{-# LANGUAGE OverloadedStrings #-}

-- Mostly stolen from:
-- https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/attoparsec

module ParsersBench.Log.Attoparsec
  ( parseLog,
  )
where

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import Data.Time
import ParsersBench.Log.Common

parseLog :: ByteString -> Log
parseLog bs =
  case parseOnly logParser bs of
    Left err -> error err
    Right x -> x

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  void (char '.')
  d2 <- decimal
  void (char '.')
  d3 <- decimal
  void (char '.')
  d4 <- decimal
  return $ IP d1 d2 d3 d4

timeParser :: Parser LocalTime
timeParser = do
  y <- count 4 digit
  void (char '-')
  mm <- count 2 digit
  void (char '-')
  d <- count 2 digit
  void (char ' ')
  h <- count 2 digit
  void (char ':')
  m <- count 2 digit
  void (char ':')
  s <- count 2 digit
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
  void (char ' ')
  ip <- parseIP
  void (char ' ')
  p <- productParser
  return (LogEntry t ip p)

logParser :: Parser Log
logParser = many (logEntryParser <* endOfLine)
