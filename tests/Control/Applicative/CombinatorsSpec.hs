{-# LANGUAGE MultiWayIf #-}

module Control.Applicative.CombinatorsSpec (spec) where

import Control.Applicative
import Data.Char (isLetter, isDigit)
import Data.List (intersperse)
import Data.Maybe (fromMaybe, maybeToList, isNothing, fromJust)
import Data.Monoid
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

spec :: Spec
spec = do

  describe "between" . it "works" . property $ \pre c n' post -> do
    let p = between (string pre) (string post) (many (char c))
        n = getNonNegative n'
        b = length (takeWhile (== c) post)
        z = replicate n c
        s = pre ++ z ++ post
    if b > 0
      then prs_ p s `shouldFailWith` err (posN (length pre + n + b) s)
        ( etoks post <> etok c <>
          (if length post == b then ueof else utoks [post !! b]) )
      else prs_ p s `shouldParse` z

  describe "choice" . it "works" . property $ \cs' s' -> do
    let cs = getNonEmpty cs'
        p = choice (char <$> cs)
        s = [s']
    if s' `elem` cs
      then prs_ p s `shouldParse` s'
      else prs_ p s `shouldFailWith` err posI (utok s' <> mconcat (etok <$> cs))

  describe "count" . it "works" . property $ \n x' -> do
    let x = getNonNegative x'
        p = count n (char 'x')
        p' = count' n n (char 'x')
        s = replicate x 'x'
    prs_ p s `shouldBe` prs_ p' s

  describe "count'" . it "works" . property $ \m n x' -> do
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

  describe "eitherP" . it "works" . property $ \ch -> do
    let p = eitherP letterChar digitChar
        s = pure ch
    if | isLetter ch -> prs_ p s `shouldParse` Left ch
       | isDigit  ch -> prs_ p s `shouldParse` Right ch
       | otherwise   -> prs_ p s `shouldFailWith`
         err posI (utok ch <> elabel "letter" <> elabel "digit")

  describe "endBy" . it "works" . property $ \n' c -> do
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

  describe "endBy1" . it "works" . property $ \n' c -> do
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

  describe "manyTill" . it "works" . property $ \a' b' c' -> do
    let [a,b,c] = getNonNegative <$> [a',b',c']
        p = (,) <$> manyTill letterChar (char 'c') <*> many letterChar
        s = abcRow a b c
    if c == 0
      then prs_ p s `shouldFailWith` err (posN (a + b) s)
           (ueof <> etok 'c' <> elabel "letter")
      else let (pre, post) = break (== 'c') s
           in prs_ p s `shouldParse` (pre, drop 1 post)

  describe "someTill" . it "works" . property $ \a' b' c' -> do
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

  describe "option" . it "works" . property $ \d a s -> do
    let p = option d (string a)
        p' = fromMaybe d <$> optional (string a)
    prs_ p s `shouldBe` prs_ p' s

  describe "sepBy" . it "works" . property $ \n' c' -> do
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

  describe "sepBy1" . it "works" . property $ \n' c' -> do
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

  describe "sepEndBy" . it "works" . property $ \n' c' -> do
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

  describe "sepEndBy1" . it "works" . property $ \n' c' -> do
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

  describe "skipMany" . it "works" . property $ \c n' a -> do
    let p = skipMany (char c) *> string a
        n = getNonNegative n'
        p' = many (char c) >> string a
        s = replicate n c ++ a
    prs_ p s `shouldBe` prs_ p' s

  describe "skipSome" . it "works" . property $ \c n' a -> do
    let p = skipSome (char c) *> string a
        n = getNonNegative n'
        p' = some (char c) >> string a
        s = replicate n c ++ a
    prs_ p s `shouldBe` prs_ p' s

  describe "skipManyTill" . it "works" . property $ \c n' a -> c /= a ==> do
    let p = skipManyTill (char c) (char a)
        n = getNonNegative n'
        s = replicate n c ++ [a]
    prs_ p s `shouldParse` a

  describe "skipSomeTill" . it "works" . property $ \c n' a -> c /= a ==> do
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
