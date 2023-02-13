module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified ParsersBench.CSV.Attoparsec as A
import qualified ParsersBench.CSV.Megaparsec as M
import qualified ParsersBench.Json.Attoparsec as A
import qualified ParsersBench.Json.Megaparsec as M
import qualified ParsersBench.Log.Attoparsec as A
import qualified ParsersBench.Log.Megaparsec as M
import Weigh

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  forM_ csvFiles $ \file ->
    bparser "CSV (Attoparsec)" file A.parseCSV
  forM_ csvFiles $ \file ->
    bparser "CSV (Megaparsec)" file M.parseCSV
  forM_ logFiles $ \file ->
    bparser "Log (Attoparsec)" file A.parseLog
  forM_ logFiles $ \file ->
    bparser "Log (Megaparsec)" file M.parseLog
  forM_ jsonFiles $ \file ->
    bparser "JSON (Attoparsec)" file A.parseJson
  forM_ jsonFiles $ \file ->
    bparser "JSON (Megaparsec)" file M.parseJson

bparser :: (NFData a) => String -> FilePath -> (ByteString -> a) -> Weigh ()
bparser pre desc f = io (pre ++ "-" ++ desc) m path
  where
    path = "data/" ++ desc
    m pth = f <$> B.readFile pth

csvFiles :: [FilePath]
csvFiles =
  [ "csv-5.csv",
    "csv-10.csv",
    "csv-20.csv",
    "csv-40.csv"
  ]

logFiles :: [FilePath]
logFiles =
  [ "log-5.log",
    "log-10.log",
    "log-20.log",
    "log-40.log"
  ]

jsonFiles :: [FilePath]
jsonFiles =
  [ "json-5.json",
    "json-10.json",
    "json-20.json",
    "json-40.json"
  ]
