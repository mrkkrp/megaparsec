{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Main (main) where

import Control.DeepSeq
import Criterion.Main
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup ((<>))
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as E
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

  , bgroup "" [bbundle "single error" n [n] | n <- stdSeries]

  , bbundle "2 errors" 1000 [1, 1000]
  , bbundle "4 errors" 1000 [1, 500, 1000]
  , bbundle "100 errors" 1000 [10,20..1000]

  , breachOffset 0 1000
  , breachOffset 0 2000
  , breachOffset 0 4000
  , breachOffset 1000 1000

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

-- | Bench the 'errorBundlePretty' function.

bbundle
  :: String            -- ^ Name of the benchmark
  -> Int               -- ^ Number of lines in input stream
  -> [Int]             -- ^ Lines with parse errors
  -> Benchmark
bbundle name totalLines sps =
  let s = take (totalLines * 80) (cycle as)
      as = replicate 79 'a' ++ "\n"
      f l = TrivialError
        (20 + l * 80)
        (Just $ Tokens ('a' :| ""))
        (E.singleton $ Tokens ('b' :| ""))
      bundle :: ParseErrorBundle String Void
      bundle = ParseErrorBundle
        { bundleErrors = f <$> NE.fromList sps
        , bundlePosState = PosState
          { pstateInput = s
          , pstateOffset = 0
          , pstateSourcePos = initialPos ""
          , pstateTabWidth = defaultTabWidth
          , pstateLinePrefix = ""
          }
        }
  in bench ("errorBundlePretty-" ++ show totalLines ++ "-" ++ name)
           (nf errorBundlePretty bundle)

-- | Bench the 'reachOffset' function.

breachOffset
  :: Int               -- ^ Starting offset in 'PosState'
  -> Int               -- ^ Offset to reach
  -> Benchmark
breachOffset o0 o1 = bench
  ("reachOffset-" ++ show o0 ++ "-" ++ show o1)
  (nf f (o0 * 80, o1 * 80))
  where
    f :: (Int, Int) -> (SourcePos, PosState Text)
    f (startOffset, targetOffset) =
      let (x, _, y) = reachOffset targetOffset PosState
             { pstateInput = manyAs (targetOffset - startOffset)
             , pstateOffset = startOffset
             , pstateSourcePos = initialPos ""
             , pstateTabWidth = defaultTabWidth
             , pstateLinePrefix = ""
             }
      in (x, y)

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
