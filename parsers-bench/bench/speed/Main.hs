module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.ByteString (ByteString)
import qualified Data.ByteString              as B
import qualified ParsersBench.CSV.Attoparsec  as A
import qualified ParsersBench.CSV.Megaparsec  as M
import qualified ParsersBench.Json.Attoparsec as A
import qualified ParsersBench.Json.Megaparsec as M
import qualified ParsersBench.Log.Attoparsec  as A
import qualified ParsersBench.Log.Megaparsec  as M

main :: IO ()
main = defaultMain
  [ bgroup "CSV (Attoparsec)"
    [ bparser file A.parseCSV | file <- csvFiles ]
  , bgroup "CSV (Megaparsec)"
    [ bparser file M.parseCSV | file <- csvFiles ]
  , bgroup "Log (Attoparsec)"
    [ bparser file A.parseLog | file <- logFiles ]
  , bgroup "Log (Megaparsec)"
    [ bparser file M.parseLog | file <- logFiles ]
  , bgroup "JSON (Attoparsec)"
    [ bparser file A.parseJson | file <- jsonFiles ]
  , bgroup "JSON (Megapasec)"
    [ bparser file M.parseJson | file <- jsonFiles ]
  ]

bparser :: NFData a => FilePath -> (ByteString -> a) -> Benchmark
bparser desc f = env (B.readFile path) (bench desc . nf f)
  where
    path = "data/" ++ desc

csvFiles :: [FilePath]
csvFiles =
  [ "csv-5.csv"
  , "csv-10.csv"
  , "csv-20.csv"
  , "csv-40.csv" ]

logFiles :: [FilePath]
logFiles =
  [ "log-5.log"
  , "log-10.log"
  , "log-20.log"
  , "log-40.log" ]

jsonFiles :: [FilePath]
jsonFiles =
  [ "json-5.json"
  , "json-10.json"
  , "json-20.json"
  , "json-40.json" ]
