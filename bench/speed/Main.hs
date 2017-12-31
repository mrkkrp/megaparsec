{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Text                  as T
import qualified Text.Megaparsec.Char.Lexer as L

-- | The type of parser that consumes 'String's.

type Parser = Parsec Void Text

main :: IO ()
main = defaultMain
  [ bparser "string"   manyAs (string . fst)
  , bparser "string'"  manyAs (string' . fst)
  , bparser "many"     manyAs (const $ many (char 'a'))
  , bparser "some"     manyAs (const $ some (char 'a'))
  , bparser "choice"   (const "b") (choice . fmap char . manyAsB' . snd)
  , bparser "count"    manyAs (\(_,n) -> count n (char 'a'))
  , bparser "count'"   manyAs (\(_,n) -> count' 1 n (char 'a'))
  , bparser "endBy"    manyAbs' (const $ endBy (char 'a') (char 'b'))
  , bparser "endBy1"   manyAbs' (const $ endBy1 (char 'a') (char 'b'))
  , bparser "manyTill" manyAsB (const $ manyTill (char 'a') (char 'b'))
  , bparser "someTill" manyAsB (const $ someTill (char 'a') (char 'b'))
  , bparser "sepBy"    manyAbs (const $ sepBy (char 'a') (char 'b'))
  , bparser "sepBy1"   manyAbs (const $ sepBy1 (char 'a') (char 'b'))
  , bparser "sepEndBy"  manyAbs' (const $ sepEndBy (char 'a') (char 'b'))
  , bparser "sepEndBy1" manyAbs' (const $ sepEndBy1 (char 'a') (char 'b'))
  , bparser "skipMany" manyAs (const $ skipMany (char 'a'))
  , bparser "skipSome" manyAs (const $ skipSome (char 'a'))
  , bparser "skipCount" manyAs (\(_,n) -> skipCount n (char 'a'))
  , bparser "skipManyTill" manyAsB (const $ skipManyTill (char 'a') (char 'b'))
  , bparser "skipSomeTill" manyAsB (const $ skipSomeTill (char 'a') (char 'b'))
  , bparser "takeWhileP" manyAs (const $ takeWhileP Nothing (== 'a'))
  , bparser "takeWhile1P" manyAs (const $ takeWhile1P Nothing (== 'a'))
  , bparser "decimal" mkInt (const (L.decimal :: Parser Integer))
  , bparser "octal" mkInt (const (L.octal :: Parser Integer))
  , bparser "hexadecimal" mkInt (const (L.hexadecimal :: Parser Integer))
  , bparser "scientific" mkInt (const L.scientific)
  ]

-- | Perform a series to measurements with the same parser.

bparser :: NFData a
  => String            -- ^ Name of the benchmark group
  -> (Int -> Text)     -- ^ How to construct input
  -> ((Text, Int) -> Parser a) -- ^ The parser receiving its future input
  -> Benchmark         -- ^ The benchmark
bparser name f p = bgroup name (bs <$> stdSeries)
  where
    bs n = env (return (f n, n)) (bench (show n) . nf p')
    p' (s,n) = parse (p (s,n)) "" s

-- | The series of sizes to try as part of 'bparser'.

stdSeries :: [Int]
stdSeries = [500,1000,2000,4000]

----------------------------------------------------------------------------
-- Helpers

-- | Generate that many \'a\' characters.

manyAs :: Int -> Text
manyAs n = T.replicate n "a"

-- | Like 'manyAs', but interspersed with \'b\'s.

manyAbs :: Int -> Text
manyAbs n = T.take (if even n then n + 1 else n) (T.replicate n "ab")

-- | Like 'manyAs', but with a \'b\' added to the end.

manyAsB :: Int -> Text
manyAsB n = manyAs n <> "b"

-- | Like 'manyAsB', but returns a 'String'.

manyAsB' :: Int -> String
manyAsB' n = replicate n 'a' ++ "b"

-- | Like 'manyAbs', but ends in a \'b\'.

manyAbs' :: Int -> Text
manyAbs' n = T.take (if even n then n else n + 1) (T.replicate n "ab")

-- | Render an 'Integer' with the number of digits linearly dependent on the
-- argument.

mkInt :: Int -> Text
mkInt n = (T.pack . show) ((10 :: Integer) ^ (n `quot` 100))
