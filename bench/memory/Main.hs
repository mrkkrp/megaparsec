{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.DeepSeq
import Control.Monad
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import Weigh
import qualified Data.Text as T

-- | The type of parser that consumes 'String's.

type Parser = Parsec Void Text

main :: IO ()
main = mainWith $ do
  setColumns [Case, Allocated, GCs, Max]
  bparser "string"   manyAs (string . fst)
  bparser "string'"  manyAs (string' . fst)
  bparser "many"     manyAs (const $ many (char 'a'))
  bparser "some"     manyAs (const $ some (char 'a'))
  bparser "choice"   (const "b") (choice . fmap char . manyAsB' . snd)
  bparser "count"    manyAs (\(_,n) -> count n (char 'a'))
  bparser "count'"   manyAs (\(_,n) -> count' 1 n (char 'a'))
  bparser "endBy"    manyAbs' (const $ endBy (char 'a') (char 'b'))
  bparser "endBy1"   manyAbs' (const $ endBy1 (char 'a') (char 'b'))
  bparser "manyTill" manyAsB (const $ manyTill (char 'a') (char 'b'))
  bparser "someTill" manyAsB (const $ someTill (char 'a') (char 'b'))
  bparser "sepBy"    manyAbs (const $ sepBy (char 'a') (char 'b'))
  bparser "sepBy1"   manyAbs (const $ sepBy1 (char 'a') (char 'b'))
  bparser "sepEndBy"  manyAbs' (const $ sepEndBy (char 'a') (char 'b'))
  bparser "sepEndBy1" manyAbs' (const $ sepEndBy1 (char 'a') (char 'b'))
  bparser "skipMany" manyAs (const $ skipMany (char 'a'))
  bparser "skipSome" manyAs (const $ skipSome (char 'a'))
  bparser "skipManyTill" manyAsB (const $ skipManyTill (char 'a') (char 'b'))
  bparser "skipSomeTill" manyAsB (const $ skipSomeTill (char 'a') (char 'b'))
  bparser "takeWhileP" manyAs (const $ takeWhileP Nothing (== 'a'))
  bparser "takeWhile1P" manyAs (const $ takeWhile1P Nothing (== 'a'))

-- | Perform a series of measurements with the same parser.

bparser :: NFData a
  => String            -- ^ Name of the benchmark group
  -> (Int -> Text)     -- ^ How to construct input
  -> ((Text, Int) -> Parser a) -- ^ The parser receiving its future input
  -> Weigh ()
bparser name f p = forM_ stdSeries $ \i -> do
  let arg      = (f i,i)
      p' (s,n) = parse (p (s,n)) "" s
  func (name ++ "/" ++ show i) p' arg

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
