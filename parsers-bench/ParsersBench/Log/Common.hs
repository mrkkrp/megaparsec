{-# LANGUAGE DeriveGeneric #-}

module ParsersBench.Log.Common
  ( IP (..),
    Product (..),
    LogEntry (..),
    Log,
  )
where

import Control.DeepSeq
import Data.Time
import Data.Word
import GHC.Generics

data IP = IP Word8 Word8 Word8 Word8
  deriving (Show, Eq, Generic)

instance NFData IP

data Product
  = Mouse
  | Keyboard
  | Monitor
  | Speakers
  deriving (Show, Eq, Generic)

instance NFData Product

data LogEntry
  = LogEntry
      { entryTime :: LocalTime,
        entryIP :: IP,
        entryProduct :: Product
      }
  deriving (Show, Eq, Generic)

instance NFData LogEntry

type Log = [LogEntry]
