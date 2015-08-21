-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's primitive parser combinators.
--
-- Copyright © 2015 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders "as is" and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

{-# OPTIONS -fno-warn-orphans #-}

module Prim (tests) where

import Control.Applicative
import Control.Monad (guard)
import Data.Bool (bool)
import Data.Char (isLetter)
import Data.Maybe (maybeToList)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String

import Pos ()
import Util

tests :: Test
tests = testGroup "Primitive parser combinators"
        [ testProperty "ParsecT functor" prop_functor
        , testProperty "ParsecT applicative (<*>)" prop_applicative_0
        , testProperty "ParsecT applicative (*>)" prop_applicative_1
        , testProperty "ParsecT applicative (<*)" prop_applicative_2
        , testProperty "ParsecT alternative empty and (<|>)" prop_alternative_0
        , testProperty "ParsecT alternative (<|>)" prop_alternative_1
        , testProperty "ParsecT alternative many" prop_alternative_2
        , testProperty "ParsecT alternative some" prop_alternative_3
        , testProperty "ParsecT alternative optional" prop_alternative_4
        , testProperty "ParsecT monad return" prop_monad_0
        , testProperty "ParsecT monad (>>)" prop_monad_1
        , testProperty "ParsecT monad (>>=)" prop_monad_2
        , testProperty "ParsecT monad fail" prop_monad_3
        , testProperty "combinator unexpected" prop_unexpected
        , testProperty "combinator label" prop_label
        , testProperty "combinator hidden" prop_hidden
        , testProperty "combinator try" prop_try
        , testProperty "combinator lookAhead" prop_lookAhead_0
        , testProperty "combinator lookAhead hints" prop_lookAhead_1
        , testProperty "combinator lookAhead messages" prop_lookAhead_2
        , testProperty "combinator notFollowedBy" prop_notFollowedBy_0
        , testProperty "combinator notFollowedBy twice" prop_notFollowedBy_1
        , testProperty "combinator notFollowedBy eof" prop_notFollowedBy_2
        , testProperty "combinator token" prop_token
        , testProperty "combinator tokens" prop_tokens
        , testProperty "parser state position" prop_state_pos
        , testProperty "parser state input" prop_state_input
        , testProperty "parser state general" prop_state
        , testProperty "user state set and get" prop_user_state
        , testProperty "user state backtracking" prop_user_backtrack ]

instance Arbitrary u => Arbitrary (State String u) where
  arbitrary = State <$> arbitrary <*> arbitrary <*> arbitrary

-- Functor instance

prop_functor :: Integer -> Integer -> Property
prop_functor n m =
  ((+ m) <$> return n) /=\ n + m .&&. ((* n) <$> return m) /=\ n * m

-- Applicative instance

prop_applicative_0 :: Integer -> Integer -> Property
prop_applicative_0 n m = ((+) <$> pure n <*> pure m) /=\ n + m

prop_applicative_1 :: Integer -> Integer -> Property
prop_applicative_1 n m = (pure n *> pure m) /=\ m

prop_applicative_2 :: Integer -> Integer -> Property
prop_applicative_2 n m = (pure n <* pure m) /=\ n

-- Alternative instance

prop_alternative_0 :: Integer -> Property
prop_alternative_0 n = (empty <|> return n) /=\ n

prop_alternative_1 :: String -> String -> Property
prop_alternative_1 s0 s1
  | null s0 && null s1 = checkParser p (Right "") ""
  | null s0   = checkParser p (posErr 0 s1 [uneCh (head s1), exEof]) s1
  | otherwise = checkParser p (Right s0) s0 .&&. checkParser p (Right s1) s1
    where p = try (string s0) <|> string s1

prop_alternative_2 :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                      Property
prop_alternative_2 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> many (char 'b')
        r | null s = Right s
          | c > 0  = posErr (a + b) s $ [uneCh 'c', exCh 'b', exEof]
                     ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c

prop_alternative_3 :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                      Property
prop_alternative_3 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> some (char 'a') <*> some (char 'b')
        r | null s = posErr 0 s [uneEof, exCh 'a']
          | a == 0 = posErr 0 s [uneCh (head s), exCh 'a']
          | b == 0 = posErr a s $ [exCh 'a', exCh 'b'] ++
                     if c > 0 then [uneCh 'c'] else [uneEof]
          | c > 0 = posErr (a + b) s [uneCh 'c', exCh 'b', exEof]
          | otherwise = Right s
        s = abcRow a b c

prop_alternative_4 :: Bool -> Bool -> Bool -> Property
prop_alternative_4 a b c = checkParser p r s
  where p = f <$> optional (char 'a') <*> optional (char 'b')
        f x y = maybe "" (:[]) x ++ maybe "" (:[]) y
        r | c = posErr ab s $ [uneCh 'c', exEof] ++
                [exCh 'a' | not a && not b] ++ [exCh 'b' | not b]
          | otherwise = Right s
        s = abcRow' a b c
        ab = fromEnum a + fromEnum b

-- Monad instance

prop_monad_0 :: Integer -> Property
prop_monad_0 n = checkParser (return n) (Right n) ""

prop_monad_1 :: Char -> Char -> Maybe Char -> Property
prop_monad_1 a b c = checkParser p r s
  where p = char a >> char b
        r = simpleParse (char a *> char b) s
        s = a : b : maybeToList c

prop_monad_2 :: Char -> Char -> Maybe Char -> Property
prop_monad_2 a b c = checkParser p r s
  where p = char a >>= \x -> char b >> return x
        r = simpleParse (char a <* char b) s
        s = a : b : maybeToList c

prop_monad_3 :: String -> Property
prop_monad_3 m = checkParser p r s
  where p = fail m :: Parser ()
        r | null m    = posErr 0 s []
          | otherwise = posErr 0 s [msg m]
        s = ""

-- TODO MonadReader instance

-- TODO MonadState instance

-- Primitive combinators

prop_unexpected :: String -> Property
prop_unexpected m = checkParser p r s
  where p = unexpected m :: Parser ()
        r | null m    = posErr 0 s []
          | otherwise = posErr 0 s [uneSpec m]
        s = ""

prop_label :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
              String -> Property
prop_label a' b' c' l = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> (many (char 'b') <?> l)
        r | null s = Right s
          | c > 0 = posErr (a + b) s $ [uneCh 'c', exSpec l, exEof]
                    ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c

prop_hidden :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
               Property
prop_hidden a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> hidden (many (char 'b'))
        r | null s = Right s
          | c > 0  = posErr (a + b) s $ [uneCh 'c', exEof]
                     ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c

prop_try :: String -> String -> String -> Property
prop_try pre s1' s2' = checkParser p r s
  where s1 = pre ++ s1'
        s2 = pre ++ s2'
        p = try (string s1) <|> string s2
        r | s == s1 || s == s2 = Right s
          | otherwise = posErr 0 s $ bool [uneStr pre] [uneEof] (null s)
                        ++ [uneStr pre, exStr s1, exStr s2]
        s = pre

prop_lookAhead_0 :: Bool -> Bool -> Bool -> Property
prop_lookAhead_0 a b c = checkParser p r s
  where p = do
          l <- lookAhead (oneOf "ab" <?> "label")
          guard (l == h)
          char 'a'
        h = head s
        r | null s = posErr 0 s [uneEof, exSpec "label"]
          | s == "a" = Right 'a'
          | h == 'b' = posErr 0 s [uneCh 'b', exCh 'a']
          | h == 'c' = posErr 0 s [uneCh 'c', exSpec "label"]
          | otherwise  = posErr 1 s [uneCh (s !! 1), exEof]
        s = abcRow' a b c

prop_lookAhead_1 :: String -> Property
prop_lookAhead_1 s = checkParser p r s
  where p = lookAhead (some letterChar) >> fail "failed" :: Parser ()
        h = head s
        r | null s     = posErr 0 s [uneEof, exSpec "letter"]
          | isLetter h = posErr 0 s [msg "failed"]
          | otherwise  = posErr 0 s [uneCh h, exSpec "letter"]

prop_lookAhead_2 :: Bool -> Bool -> Bool -> Property
prop_lookAhead_2 a b c = checkParser p r s
  where p = lookAhead (some (char 'a')) >> char 'b'
        r | null s    = posErr 0 s [uneEof, exCh 'a']
          | a         = posErr 0 s [uneCh 'a', exCh 'b']
          | otherwise = posErr 0 s [uneCh (head s), exCh 'a']
        s = abcRow' a b c

prop_notFollowedBy_0 :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                        Property
prop_notFollowedBy_0 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* notFollowedBy (char 'b') <* many (char 'c')
        r | b > 0 = posErr a s [uneCh 'b', exCh 'a']
          | otherwise = Right (replicate a 'a')
        s = abcRow a b c

prop_notFollowedBy_1 :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                        Property
prop_notFollowedBy_1 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* f (char 'c') <* many (char 'c')
        f = notFollowedBy . notFollowedBy -- = 'lookAhead' in this case
        r | b == 0 && c > 0 = Right (replicate a 'a')
          | b > 0           = posErr a s [uneCh 'b', exCh 'a']
          | otherwise       = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c

prop_notFollowedBy_2 :: NonNegative Int -> NonNegative Int -> NonNegative Int ->
                        Property
prop_notFollowedBy_2 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* notFollowedBy eof <* many anyChar
        r | b > 0 || c > 0 = Right (replicate a 'a')
          | otherwise      = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c

-- We omit tests for 'eof' here because it's used virtually everywhere, it's
-- already thoroughly tested.

prop_token :: String -> Property
prop_token s = checkParser p r s
  where p = token nextPos testChar
        nextPos pos x _ = updatePosChar pos x
        testChar x      = if isLetter x then Just x else Nothing
        h = head s
        r | null s = posErr 0 s [uneEof]
          | isLetter h && length s == 1 = Right (head s)
          | isLetter h && length s > 1 = posErr 1 s [uneCh (s !! 1), exEof]
          | otherwise = posErr 0 s [uneCh h]

prop_tokens :: String -> String -> Property
prop_tokens a = checkString p a (showToken a)
  where p = tokens updatePosString a

-- Parser state combinators

prop_state_pos :: SourcePos -> Property
prop_state_pos pos = p /=\ pos
  where p = setPosition pos >> getPosition

prop_state_input :: String -> Property
prop_state_input s = p /=\ s
  where p = do
          st0    <- getInput
          guard (null st0)
          setInput s
          result <- string s
          st1    <- getInput
          guard (null st1)
          return result

prop_state :: State String Integer -> State String Integer -> Property
prop_state s1 s2 = runParser p 0 "" "" === Right (f s2 s1)
  where f (State s1' pos u1) (State s2' _ u2) =
          State (max s1' s2' ) pos (u1 + u2)
        p = do
          st <- getParserState
          guard (st == State "" (initialPos "") 0)
          setParserState s1
          updateParserState (f s2)
          getParserState

-- User state combinators

prop_user_state :: Integer -> Integer -> Property
prop_user_state n m = runParser p 0 "" "" === Right (n + m)
  where p = setState n >> modifyState (+ m) >> getState

prop_user_backtrack :: Integer -> Integer -> Property
prop_user_backtrack n m = runParser p 0 "" "" === Right n
  where p = setState n >> lookAhead (setState m >> eof) >> getState

-- Helpers

infix 4 /=\

(/=\) :: (Eq a, Show a) => Parser a -> a -> Property
p /=\ x = simpleParse p "" === Right x

abcRow :: Int -> Int -> Int -> String
abcRow a b c = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

abcRow' :: Bool -> Bool -> Bool -> String
abcRow' a b c = abcRow (fromEnum a) (fromEnum b) (fromEnum c)
