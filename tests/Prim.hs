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
import Data.Maybe (maybeToList)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Pos (SourcePos, initialPos)
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
        , testProperty "ParsecT alternative (<|>) again" prop_alternative_1
        , testProperty "ParsecT alternative many" prop_alternative_2
        , testProperty "ParsecT alternative some" prop_alternative_3
        , testProperty "ParsecT alternative optional" prop_alternative_4
        , testProperty "ParsecT monad return" prop_monad_0
        , testProperty "ParsecT monad (>>)" prop_monad_1
        , testProperty "ParsecT monad (>>=)" prop_monad_2
        , testProperty "ParsecT monad fail" prop_monad_3
        , testProperty "combinator unexpected" prop_unexpected
          -- NEXT
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
          | c > 0 = posErr (a + b) s $ [uneCh 'c', exCh 'b', exEof]
                    ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

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
        s = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

prop_alternative_4 :: Bool -> Bool -> Bool -> Property
prop_alternative_4 a b c = checkParser p r s
  where p = f <$> optional (char 'a') <*> optional (char 'b')
        f x y = maybe "" (:[]) x ++ maybe "" (:[]) y
        r | c = posErr ab s $ [uneCh 'c', exEof] ++
                [exCh 'a' | not a && not b] ++ [exCh 'b' | not b]
          | otherwise = Right s
        s = g 'a' a ++ g 'b' b ++ g 'c' c
        ab = fromEnum a + fromEnum b
        g x = bool [] [x]

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

-- Primitive combinators

prop_unexpected :: String -> Property
prop_unexpected m = checkParser p r s
  where p = unexpected m :: Parser ()
        r | null m    = posErr 0 s []
          | otherwise = posErr 0 s [uneSpec m]
        s = ""

-- TODO label (expected messages and also in conjunction with hints)
-- TODO hidden

-- parseTest ((space <?> "doda") <* eof) "r"
-- parse error at line 1, column 1:
-- unexpected 'r'
-- expecting doda or end of input

-- TODO try
-- parseTest (string "let" <|> string "lexical") "le"
-- parse error at line 1, column 1:
-- unexpected "le"
-- expecting "let" or "lexical"

-- TODO parseTest (try (string "let") <|> string "lexical") "le"
-- parse error at line 1, column 1:
-- unexpected "le"
-- expecting "let" or "lexical"

-- TODO lookAhead
-- TODO notFollowedBy

-- parseTest (many (char 'r') *> notFollowedBy (lookAhead (string "a")) <* eof) "ra"

-- We omit tests for 'eof' here because it's used virtually everywhere, it's
-- already thoroughly tested.

-- TODO token
-- TODO tokens

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
