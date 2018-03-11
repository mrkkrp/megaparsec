{-# LANGUAGE CPP        #-}
{-# LANGUAGE MultiWayIf #-}

module Control.Monad.CombinatorsSpec (spec) where

import Data.List (intersperse)
import Data.Maybe (maybeToList, isNothing, fromJust)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative hiding (many, some)
#endif
#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

spec :: Spec
spec = do

  describe "count" $ do
    it "works" . property $ \n x' -> do
      let x = getNonNegative x'
          p = count n (char 'x')
          p' = count' n n (char 'x')
          s = replicate x 'x'
      prs_ p s `shouldBe` prs_ p' s
    rightOrder (count 3 letterChar) "abc" "abc"

  describe "count'" $ do
    it "works" . property $ \m n x' -> do
      let x = getNonNegative x'
          p = count' m n (char 'x')
          s = replicate x 'x'
      if | n <= 0 || m > n ->
           if x == 0
             then prs_ p s `shouldParse` ""
             else prs_ p s `shouldFailWith` err posI (utok 'x' <> eeof)
         | m <= x && x <= n ->
           prs_ p s `shouldParse` s
         | x < m ->
           prs_ p s `shouldFailWith` err (posN x s) (ueof <> etok 'x')
         | otherwise ->
           prs_ p s `shouldFailWith` err (posN n s) (utok 'x' <> eeof)
    rightOrder (count' 1 3 letterChar) "abc" "abc"

  describe "endBy" $ do
    it "works" . property $ \n' c -> do
      let n = getNonNegative n'
          p = endBy (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ [c]
      if | c == 'a' && n == 0 ->
           prs_ p s `shouldFailWith` err (posN (1 :: Int) s) (ueof <> etok '-')
         | c == 'a' ->
           prs_ p s `shouldFailWith` err (posN (g n) s) (utok 'a' <> etok '-')
         | c == '-' && n == 0 ->
           prs_ p s `shouldFailWith` err posI (utok '-' <> etok 'a'<> eeof)
         | c /= '-' ->
           prs_ p s `shouldFailWith` err (posN (g n) s)
             ( utok c <>
               (if n > 0 then etok '-' else eeof) <>
               (if n == 0 then etok 'a' else mempty) )
         | otherwise -> prs_ p s `shouldParse` replicate n 'a'
    rightOrder (endBy letterChar (char ',')) "a,b,c," "abc"

  describe "endBy1" $ do
    it "works" . property $ \n' c -> do
      let n = getNonNegative n'
          p = endBy1 (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ [c]
      if | c == 'a' && n == 0 ->
           prs_ p s `shouldFailWith` err (posN (1 :: Int) s) (ueof <> etok '-')
         | c == 'a' ->
           prs_ p s `shouldFailWith` err (posN (g n) s) (utok 'a' <> etok '-')
         | c == '-' && n == 0 ->
           prs_ p s `shouldFailWith` err posI (utok '-' <> etok 'a')
         | c /= '-' ->
           prs_ p s `shouldFailWith` err (posN (g n) s)
             ( utok c <>
               (if n > 0 then etok '-' else mempty) <>
               (if n == 0 then etok 'a' else mempty) )
         | otherwise -> prs_ p s `shouldParse` replicate n 'a'
    rightOrder (endBy1 letterChar (char ',')) "a,b,c," "abc"

  describe "manyTill" $ do
    it "works" . property $ \a' b' c' -> do
      let [a,b,c] = getNonNegative <$> [a',b',c']
          p = (,) <$> manyTill letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if c == 0
        then prs_ p s `shouldFailWith` err (posN (a + b) s)
             (ueof <> etok 'c' <> elabel "letter")
        else let (pre, post) = break (== 'c') s
             in prs_ p s `shouldParse` (pre, drop 1 post)
    rightOrder (manyTill letterChar (char 'd')) "abcd" "abc"

  describe "someTill" $ do
    it "works" . property $ \a' b' c' -> do
      let [a,b,c] = getNonNegative <$> [a',b',c']
          p = (,) <$> someTill letterChar (char 'c') <*> many letterChar
          s = abcRow a b c
      if | null s ->
           prs_ p s `shouldFailWith` err posI (ueof <> elabel "letter")
         | c == 0 ->
           prs_ p s `shouldFailWith` err (posN (a + b) s)
             (ueof <> etok 'c' <> elabel "letter")
         | s == "c" ->
           prs_ p s `shouldFailWith` err
             (posN (1 :: Int) s) (ueof <> etok 'c' <> elabel "letter")
         | head s == 'c' ->
           prs_ p s `shouldParse` ("c", drop 2 s)
         | otherwise ->
           let (pre, post) = break (== 'c') s
           in prs_ p s `shouldParse` (pre, drop 1 post)
    rightOrder (someTill letterChar (char 'd')) "abcd" "abc"

  describe "sepBy" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepBy (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if | isNothing c' ->
           prs_ p s `shouldParse` replicate n 'a'
         | c == 'a' && n == 0 ->
           prs_ p s `shouldParse` "a"
         | n == 0 ->
           prs_ p s `shouldFailWith` err posI
             (utok c <> etok 'a' <> eeof)
         | c == '-' ->
           prs_ p s `shouldFailWith` err (posN (length s) s)
             (ueof <> etok 'a')
         | otherwise ->
           prs_ p s `shouldFailWith` err (posN (g n) s)
             (utok c <> etok '-' <> eeof)
    rightOrder (sepBy letterChar (char ',')) "a,b,c" "abc"

  describe "sepBy1" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepBy1 (char 'a') (char '-')
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if | isNothing c' && n >= 1 ->
           prs_ p s `shouldParse` replicate n 'a'
         | isNothing c' ->
           prs_ p s `shouldFailWith` err posI (ueof <> etok 'a')
         | c == 'a' && n == 0 ->
           prs_ p s `shouldParse` "a"
         | n == 0 ->
           prs_ p s `shouldFailWith` err posI (utok c <> etok 'a')
         | c == '-' ->
           prs_ p s `shouldFailWith` err (posN (length s) s) (ueof <> etok 'a')
         | otherwise ->
           prs_ p s `shouldFailWith` err (posN (g n) s) (utok c <> etok '-' <> eeof)
    rightOrder (sepBy1 letterChar (char ',')) "a,b,c" "abc"

  describe "sepEndBy" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepEndBy (char 'a') (char '-')
          a = replicate n 'a'
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if | isNothing c' ->
           prs_ p s `shouldParse` a
         | c == 'a' && n == 0 ->
           prs_ p s `shouldParse` "a"
         | n == 0 ->
           prs_ p s `shouldFailWith` err posI (utok c <> etok 'a' <> eeof)
         | c == '-' ->
           prs_ p s `shouldParse` a
         | otherwise ->
           prs_ p s `shouldFailWith` err (posN (g n) s) (utok c <> etok '-' <> eeof)
    rightOrder (sepEndBy letterChar (char ',')) "a,b,c," "abc"

  describe "sepEndBy1" $ do
    it "works" . property $ \n' c' -> do
      let n = getNonNegative n'
          c = fromJust c'
          p = sepEndBy1 (char 'a') (char '-')
          a = replicate n 'a'
          s = intersperse '-' (replicate n 'a') ++ maybeToList c'
      if | isNothing c' && n >= 1 ->
           prs_ p s `shouldParse` a
         | isNothing c' ->
           prs_ p s `shouldFailWith` err posI (ueof <> etok 'a')
         | c == 'a' && n == 0 ->
           prs_ p s `shouldParse` "a"
         | n == 0 ->
           prs_ p s `shouldFailWith` err posI (utok c <> etok 'a')
         | c == '-' ->
           prs_ p s `shouldParse` a
         | otherwise ->
           prs_ p s `shouldFailWith` err (posN (g n) s) (utok c <> etok '-' <> eeof)
    rightOrder (sepEndBy1 letterChar (char ',')) "a,b,c," "abc"

  describe "skipMany" $
    it "works" . property $ \c n' a -> do
      let p = skipMany (char c) *> string a
          n = getNonNegative n'
          p' = many (char c) >> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipSome" $
    it "works" . property $ \c n' a -> do
      let p = skipSome (char c) *> string a
          n = getNonNegative n'
          p' = some (char c) >> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipCount" $
    it "works" . property $ \c n' a -> do
      let p = skipCount n (char c) *> string a
          n = getNonNegative n'
          p' = count n (char c) *> string a
          s = replicate n c ++ a
      prs_ p s `shouldBe` prs_ p' s

  describe "skipManyTill" $
    it "works" . property $ \c n' a -> c /= a ==> do
      let p = skipManyTill (char c) (char a)
          n = getNonNegative n'
          s = replicate n c ++ [a]
      prs_ p s `shouldParse` a

  describe "skipSomeTill" $
    it "works" . property $ \c n' a -> c /= a ==> do
      let p = skipSomeTill (char c) (char a)
          n = getNonNegative n'
          s = replicate n c ++ [a]
      if n == 0
        then prs_ p s `shouldFailWith` err posI (utok a <> etok c)
        else prs_ p s `shouldParse` a

----------------------------------------------------------------------------
-- Helpers

g :: Int -> Int
g x = x + if x > 0 then x - 1 else 0
