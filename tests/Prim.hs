--
-- QuickCheck tests for Megaparsec's primitive parser combinators.
--
-- Copyright © 2015–2016 Megaparsec contributors
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
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types        #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Prim (tests) where

import Control.Applicative
import Data.Char (isLetter, toUpper, chr)
import Data.Foldable (asum)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList)
import Data.Word (Word8)

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Test.Framework
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (label)
import Test.HUnit (Assertion)

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String

import Pos ()
import Error ()
import Util

tests :: Test
tests = testGroup "Primitive parser combinators"
  [ testProperty "Stream lazy byte string"             prop_byteStringL
  , testProperty "Stream strict byte string"           prop_byteStringS
  , testProperty "Stream lazy text"                    prop_textL
  , testProperty "Stream strict text"                  prop_textS
  , testProperty "ParsecT functor"                     prop_functor
  , testProperty "ParsecT applicative (<*>)"           prop_applicative_0
  , testProperty "ParsecT applicative (*>)"            prop_applicative_1
  , testProperty "ParsecT applicative (<*)"            prop_applicative_2
  , testProperty "ParsecT alternative empty and (<|>)" prop_alternative_0
  , testProperty "ParsecT alternative (<|>)"           prop_alternative_1
  , testProperty "ParsecT alternative (<|>) pos"       prop_alternative_2
  , testProperty "ParsecT alternative (<|>) hints"     prop_alternative_3
  , testProperty "ParsecT alternative many"            prop_alternative_4
  , testProperty "ParsecT alternative some"            prop_alternative_5
  , testProperty "ParsecT alternative optional"        prop_alternative_6
  , testProperty "ParsecT monad return"                prop_monad_0
  , testProperty "ParsecT monad (>>)"                  prop_monad_1
  , testProperty "ParsecT monad (>>=)"                 prop_monad_2
  , testProperty "ParsecT monad fail"                  prop_monad_3
  , testProperty "ParsecT monad laws: left identity"   prop_monad_left_id
  , testProperty "ParsecT monad laws: right identity"  prop_monad_right_id
  , testProperty "ParsecT monad laws: associativity"   prop_monad_assoc
  , testProperty "ParsecT monad reader ask"   prop_monad_reader_ask
  , testProperty "ParsecT monad reader local" prop_monad_reader_local
  , testProperty "ParsecT monad state get"    prop_monad_state_get
  , testProperty "ParsecT monad state put"    prop_monad_state_put
  , testProperty "ParsecT monad cont"         prop_monad_cont
  , testProperty "ParsecT monad error: throw" prop_monad_error_throw
  , testProperty "ParsecT monad error: catch" prop_monad_error_catch
  , testProperty "combinator unexpected"      prop_unexpected
  , testProperty "combinator failure"                  prop_failure
  , testProperty "combinator label"                    prop_label
  , testProperty "combinator hidden hints"             prop_hidden_0
  , testProperty "combinator hidden error"             prop_hidden_1
  , testProperty "combinator try"                      prop_try
  , testProperty "combinator lookAhead"                prop_lookAhead_0
  , testProperty "combinator lookAhead hints"          prop_lookAhead_1
  , testProperty "combinator lookAhead messages"       prop_lookAhead_2
  , testCase     "combinator lookAhead cerr"           case_lookAhead_3
  , testProperty "combinator notFollowedBy"       prop_notFollowedBy_0
  , testProperty "combinator notFollowedBy twice" prop_notFollowedBy_1
  , testProperty "combinator notFollowedBy eof"   prop_notFollowedBy_2
  , testCase     "combinator notFollowedBy cerr"  case_notFollowedBy_3a
  , testCase     "combinator notFollowedBy cerr"  case_notFollowedBy_3b
  , testCase     "combinator notFollowedBy eerr"  case_notFollowedBy_4a
  , testCase     "combinator notFollowedBy eerr"  case_notFollowedBy_4b
  , testProperty "combinator withRecovery"             prop_withRecovery_0
  , testCase     "combinator withRecovery eok"         case_withRecovery_1
  , testCase     "combinator withRecovery meerr-rcerr" case_withRecovery_2
  , testCase     "combinator withRecovery meerr-reok"  case_withRecovery_3a
  , testCase     "combinator withRecovery meerr-reok"  case_withRecovery_3b
  , testCase     "combinator withRecovery mcerr-rcok"  case_withRecovery_4a
  , testCase     "combinator withRecovery mcerr-rcok"  case_withRecovery_4b
  , testCase     "combinator withRecovery mcerr-rcerr" case_withRecovery_5
  , testCase     "combinator withRecovery mcerr-reok"  case_withRecovery_6a
  , testCase     "combinator withRecovery mcerr-reok"  case_withRecovery_6b
  , testCase     "combinator withRecovery mcerr-reerr" case_withRecovery_7
  , testCase     "combinator eof return value"    case_eof
  , testProperty "combinator token"                    prop_token
  , testProperty "combinator tokens"                   prop_tokens_0
  , testProperty "combinator tokens (consumption)"     prop_tokens_1
  , testProperty "parser state position"               prop_state_pos
  , testProperty "parser state input"                  prop_state_input
  , testProperty "parser state tab width"              prop_state_tab
  , testProperty "parser state general"                prop_state
  , testProperty "custom state parsing"                prop_runParser'
  , testProperty "custom state parsing (transformer)"  prop_runParserT'
  , testProperty "state on failure (mplus)"         prop_stOnFail_0
  , testProperty "state on failure (tab)"           prop_stOnFail_1
  , testProperty "state on failure (eof)"           prop_stOnFail_2
  , testProperty "state on failure (notFollowedBy)" prop_stOnFail_3
  , testProperty "ReaderT try"              prop_ReaderT_try
  , testProperty "ReaderT notFollowedBy"    prop_ReaderT_notFollowedBy
  , testProperty "StateT alternative (<|>)" prop_StateT_alternative
  , testProperty "StateT lookAhead"         prop_StateT_lookAhead
  , testProperty "StateT notFollowedBy"     prop_StateT_notFollowedBy
  , testProperty "WriterT"                  prop_WriterT ]

instance Arbitrary (State String) where
  arbitrary = State <$> arbitrary <*> arbitrary <*> choose (1, 20)

-- Various instances of Stream

prop_byteStringL :: Word8 -> NonNegative Int -> Property
prop_byteStringL ch' n = parse (many (char ch)) "" (BL.pack s) === Right s
  where s = replicate (getNonNegative n) ch
        ch = byteToChar ch'

prop_byteStringS :: Word8 -> NonNegative Int -> Property
prop_byteStringS ch' n = parse (many (char ch)) "" (B.pack s) === Right s
  where s = replicate (getNonNegative n) ch
        ch = byteToChar ch'

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

prop_textL :: Char -> NonNegative Int -> Property
prop_textL ch n = parse (many (char ch)) "" (TL.pack s) === Right s
  where s = replicate (getNonNegative n) ch

prop_textS :: Char -> NonNegative Int -> Property
prop_textS ch n = parse (many (char ch)) "" (T.pack s) === Right s
  where s = replicate (getNonNegative n) ch

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
  | s0 == s1 = checkParser p (Right s0) s1
  | null s0  = checkParser p (posErr 0 s1 [uneCh (head s1), exEof]) s1
  | s0 `isPrefixOf` s1 =
      checkParser p (posErr s0l s1 [uneCh (s1 !! s0l), exEof]) s1
  | otherwise = checkParser p (Right s0) s0 .&&. checkParser p (Right s1) s1
    where p   = string s0 <|> string s1
          s0l = length s0

prop_alternative_2 :: Char -> Char -> Char -> Bool -> Property
prop_alternative_2 a b c l = checkParser p r s
  where p = char a <|> (char b >> char a)
        r | l         = Right a
          | a == b    = posErr 1 s [uneCh c, exEof]
          | a == c    = Right a
          | otherwise = posErr 1 s [uneCh c, exCh a]
        s = if l then [a] else [b,c]

prop_alternative_3 :: Property
prop_alternative_3 = checkParser p r s
  where p  = asum [empty, string ">>>", empty, return "foo"] <?> "bar"
        p' = bsum [empty, string ">>>", empty, return "foo"] <?> "bar"
        bsum = foldl (<|>) empty
        r = simpleParse p' s
        s = ">>"

prop_alternative_4 :: NonNegative Int -> NonNegative Int
                   -> NonNegative Int -> Property
prop_alternative_4 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> many (char 'b')
        r | null s = Right s
          | c > 0  = posErr (a + b) s $ [uneCh 'c', exCh 'b', exEof]
                     ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c

prop_alternative_5 :: NonNegative Int -> NonNegative Int
                   -> NonNegative Int -> Property
prop_alternative_5 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> some (char 'a') <*> some (char 'b')
        r | null s = posErr 0 s [uneEof, exCh 'a']
          | a == 0 = posErr 0 s [uneCh (head s), exCh 'a']
          | b == 0 = posErr a s $ [exCh 'a', exCh 'b'] ++
                     if c > 0 then [uneCh 'c'] else [uneEof]
          | c > 0 = posErr (a + b) s [uneCh 'c', exCh 'b', exEof]
          | otherwise = Right s
        s = abcRow a b c

prop_alternative_6 :: Bool -> Bool -> Bool -> Property
prop_alternative_6 a b c = checkParser p r s
  where p = f <$> optional (char 'a') <*> optional (char 'b')
        f x y = maybe "" (:[]) x ++ maybe "" (:[]) y
        r | c = posErr ab s $ [uneCh 'c', exEof] ++
                [exCh 'a' | not a && not b] ++ [exCh 'b' | not b]
          | otherwise = Right s
        s = abcRow a b c
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

prop_monad_left_id :: Integer -> Integer -> Property
prop_monad_left_id a b = (return a >>= f) !=! f a
  where f x = return $ x + b

prop_monad_right_id :: Integer -> Property
prop_monad_right_id a = (m >>= return) !=! m
  where m = return a

prop_monad_assoc :: Integer -> Integer -> Integer -> Property
prop_monad_assoc a b c = ((m >>= f) >>= g) !=! (m >>= (\x -> f x >>= g))
  where m = return a
        f x = return $ x + b
        g x = return $ x + c

-- MonadReader instance

prop_monad_reader_ask :: Integer -> Property
prop_monad_reader_ask a = runReader (runParserT ask "" "") a === Right a

prop_monad_reader_local :: Integer -> Integer -> Property
prop_monad_reader_local a b = runReader (runParserT p "" "") a === Right (a + b)
  where p = local (+ b) ask

-- MonadState instance

prop_monad_state_get :: Integer -> Property
prop_monad_state_get a = L.evalState (runParserT L.get "" "") a === Right a

prop_monad_state_put :: Integer -> Integer -> Property
prop_monad_state_put a b = L.execState (runParserT (L.put b) "" "") a === b

-- MonadCont instance

prop_monad_cont :: Integer -> Integer -> Property
prop_monad_cont a b = runCont (runParserT p "" "") id === Right (max a b)
  where p = do x <- callCC $ \e -> when (a > b) (e a) >> return b
               return x

-- MonadError instance

prop_monad_error_throw :: Integer -> Integer -> Property
prop_monad_error_throw a b = runExcept (runParserT p "" "") === Left a
  where p = throwError a >> return b

prop_monad_error_catch :: Integer -> Integer -> Property
prop_monad_error_catch a b =
  runExcept (runParserT p "" "") === Right (Right $ a + b)
  where p = (throwError a >> return b) `catchError` handler
        handler e = return $ e + b

-- Primitive combinators

prop_unexpected :: String -> Property
prop_unexpected m = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = unexpected m
        r = posErr 0 s $ if null m then [] else [uneSpec m]
        s = ""

prop_failure :: [Message] -> Property
prop_failure msgs = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = failure msgs
        r | null msgs = posErr 0 s []
          | otherwise = Left $ newErrorMessages msgs (initialPos "")
        s = ""

prop_label :: NonNegative Int -> NonNegative Int
           -> NonNegative Int -> String -> Property
prop_label a' b' c' l = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = (++) <$> many (char 'a') <*> (many (char 'b') <?> l)
        r | null s = Right s
          | c > 0 = posErr (a + b) s $ [uneCh 'c', exEof]
                    ++ [exCh 'a' | b == 0]
                    ++ [if b == 0 || null l
                        then exSpec l
                        else exSpec $ "rest of " ++ l]
          | otherwise = Right s
        s = abcRow a b c
        [a,b,c] = getNonNegative <$> [a',b',c']

prop_hidden_0 :: NonNegative Int -> NonNegative Int
              -> NonNegative Int -> Property
prop_hidden_0 a' b' c' = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = (++) <$> many (char 'a') <*> hidden (many (char 'b'))
        r | null s = Right s
          | c > 0  = posErr (a + b) s $ [uneCh 'c', exEof]
                     ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c
        [a,b,c] = getNonNegative <$> [a',b',c']

prop_hidden_1 :: NonEmptyList Char -> String -> Property
prop_hidden_1 c' s = checkParser' p r s
  where p :: MonadParsec s m Char => m (Maybe String)
        p = optional (hidden $ string c)
        r | null s           = Right Nothing
          | c == s           = Right (Just s)
          | c `isPrefixOf` s = posErr cn s [uneCh (s !! cn), exEof]
          | otherwise        = posErr 0 s [uneCh (head s), exEof]
        c = getNonEmpty c'
        cn = length c

prop_try :: Char -> Char -> Char -> Property
prop_try pre ch1 ch2 = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = try (sequence [char pre, char ch1])
          <|> sequence [char pre, char ch2]
        r = posErr 1 s [uneEof, exCh ch1, exCh ch2]
        s = [pre]

prop_lookAhead_0 :: Bool -> Bool -> Bool -> Property
prop_lookAhead_0 a b c = checkParser' p r s
  where p :: MonadParsec s m Char => m Char
        p = do
          l <- lookAhead (oneOf "ab" <?> "label")
          guard (l == h)
          char 'a'
        h = head s
        r | null s = posErr 0 s [uneEof, exSpec "label"]
          | s == "a" = Right 'a'
          | h == 'b' = posErr 0 s [uneCh 'b', exCh 'a']
          | h == 'c' = posErr 0 s [uneCh 'c', exSpec "label"]
          | otherwise  = posErr 1 s [uneCh (s !! 1), exEof]
        s = abcRow a b c

prop_lookAhead_1 :: String -> Property
prop_lookAhead_1 s = checkParser' p r s
  where p :: MonadParsec s m Char => m ()
        p = lookAhead (some letterChar) >> fail "failed"
        h = head s
        r | null s     = posErr 0 s [uneEof, exSpec "letter"]
          | isLetter h = posErr 0 s [msg "failed"]
          | otherwise  = posErr 0 s [uneCh h, exSpec "letter"]

prop_lookAhead_2 :: Bool -> Bool -> Bool -> Property
prop_lookAhead_2 a b c = checkParser' p r s
  where p :: MonadParsec s m Char => m Char
        p = lookAhead (some (char 'a')) >> char 'b'
        r | null s    = posErr 0 s [uneEof, exCh 'a']
          | a         = posErr 0 s [uneCh 'a', exCh 'b']
          | otherwise = posErr 0 s [uneCh (head s), exCh 'a']
        s = abcRow a b c

case_lookAhead_3 :: Assertion
case_lookAhead_3 = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = lookAhead (char 'a' *> fail emsg)
        r = posErr 1 s [msg emsg]
        emsg = "ops!"
        s = "abc"

prop_notFollowedBy_0 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
prop_notFollowedBy_0 a' b' c' = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = many (char 'a') <* notFollowedBy (char 'b') <* many (char 'c')
        r | b > 0     = posErr a s [uneCh 'b', exCh 'a']
          | otherwise = Right (replicate a 'a')
        s = abcRow a b c
        [a,b,c] = getNonNegative <$> [a',b',c']

prop_notFollowedBy_1 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
prop_notFollowedBy_1 a' b' c' = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = many (char 'a')
          <* (notFollowedBy . notFollowedBy) (char 'c')
          <* many (char 'c')
        r | b == 0 && c > 0 = Right (replicate a 'a')
          | b > 0           = posErr a s [uneCh 'b', exCh 'a']
          | otherwise       = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c
        [a,b,c] = getNonNegative <$> [a',b',c']

prop_notFollowedBy_2 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
prop_notFollowedBy_2 a' b' c' = checkParser' p r s
  where p :: MonadParsec s m Char => m String
        p = many (char 'a') <* notFollowedBy eof <* many anyChar
        r | b > 0 || c > 0 = Right (replicate a 'a')
          | otherwise      = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c
        [a,b,c] = getNonNegative <$> [a',b',c']

case_notFollowedBy_3a :: Assertion
case_notFollowedBy_3a = checkCase p r s
  where p :: MonadParsec s m Char => m ()
        p = notFollowedBy (char 'a' *> char 'c')
        r = Right ()
        s = "ab"

case_notFollowedBy_3b :: Assertion
case_notFollowedBy_3b = checkCase p r s
  where p :: MonadParsec s m Char => m ()
        p = notFollowedBy (char 'a' *> char 'd') <* char 'c'
        r = posErr 0 s [uneCh 'a', exCh 'c']
        s = "ab"

case_notFollowedBy_4a :: Assertion
case_notFollowedBy_4a = checkCase p r s
  where p :: MonadParsec s m Char => m ()
        p = notFollowedBy mzero
        r = Right ()
        s = "ab"

case_notFollowedBy_4b :: Assertion
case_notFollowedBy_4b = checkCase p r s
  where p :: MonadParsec s m Char => m ()
        p = notFollowedBy mzero <* char 'c'
        r = posErr 0 s [uneCh 'a', exCh 'c']
        s = "ab"

prop_withRecovery_0 :: NonNegative Int -> NonNegative Int
                    -> NonNegative Int -> Property
prop_withRecovery_0 a' b' c' = checkParser' p r s
  where
    p :: MonadParsec s m Char => m (Either ParseError String)
    p = let g = count' 1 3 . char in v <$>
      withRecovery (\e -> Left e <$ g 'b') (Right <$> g 'a') <*> g 'c'
    v (Right x) y = Right (x ++ y)
    v (Left  m) _ = Left m
    r | a == 0 && b == 0 && c == 0 = posErr 0 s [uneEof, exCh 'a']
      | a == 0 && b == 0 && c >  3 = posErr 0 s [uneCh 'c', exCh 'a']
      | a == 0 && b == 0           = posErr 0 s [uneCh 'c', exCh 'a']
      | a == 0 && b >  3           = posErr 3 s [uneCh 'b', exCh 'a', exCh 'c']
      | a == 0 &&           c == 0 = posErr b s [uneEof, exCh 'a', exCh 'c']
      | a == 0 &&           c >  3 = posErr (b + 3) s [uneCh 'c', exEof]
      | a == 0                     = Right (posErr 0 s [uneCh 'b', exCh 'a'])
      | a >  3                     = posErr 3 s [uneCh 'a', exCh 'c']
      |           b == 0 && c == 0 = posErr a s $ [uneEof, exCh 'c'] ++ ma
      |           b == 0 && c >  3 = posErr (a + 3) s [uneCh 'c', exEof]
      |           b == 0           = Right (Right s)
      | otherwise                  = posErr a s $ [uneCh 'b', exCh 'c'] ++ ma
    ma = [exCh 'a' | a < 3]
    s = abcRow a b c
    [a,b,c] = getNonNegative <$> [a',b',c']

case_withRecovery_1 :: Assertion
case_withRecovery_1 = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ return "bar") (return "foo")
        r = Right "foo"
        s = "abc"

case_withRecovery_2 :: Assertion
case_withRecovery_2 = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (\_ -> char 'a' *> mzero) (string "cba")
        r = posErr 0 s [uneCh 'a', exStr "cba"]
        s = "abc"

case_withRecovery_3a :: Assertion
case_withRecovery_3a = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ return "abd") (string "cba")
        r = Right "abd"
        s = "abc"

case_withRecovery_3b :: Assertion
case_withRecovery_3b = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ return "abd") (string "cba") <* char 'd'
        r = posErr 0 s [uneCh 'a', exStr "cba", exCh 'd']
        s = "abc"

case_withRecovery_4a :: Assertion
case_withRecovery_4a = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ string "bc") (char 'a' *> mzero)
        r = Right "bc"
        s = "abc"

case_withRecovery_4b :: Assertion
case_withRecovery_4b = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ string "bc")
          (char 'a' *> char 'd' *> pure "foo") <* char 'f'
        r = posErr 3 s [uneEof, exCh 'f']
        s = "abc"

case_withRecovery_5 :: Assertion
case_withRecovery_5 = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (\_ -> char 'b' *> fail emsg) (char 'a' *> fail emsg)
        r = posErr 1 s [msg emsg]
        emsg = "ops!"
        s = "abc"

case_withRecovery_6a :: Assertion
case_withRecovery_6a = checkCase p r s
  where p :: MonadParsec s m Char => m String
        p = withRecovery (const $ return "abd") (char 'a' *> mzero)
        r = Right "abd"
        s = "abc"

case_withRecovery_6b :: Assertion
case_withRecovery_6b = checkCase p r s
  where p :: MonadParsec s m Char => m Char
        p = withRecovery (const $ return 'g') (char 'a' *> char 'd') <* char 'f'
        r = posErr 1 s [uneCh 'b', exCh 'd', exCh 'f']
        s = "abc"

case_withRecovery_7 :: Assertion
case_withRecovery_7 = checkCase p r s
  where p :: MonadParsec s m Char => m Char
        p = withRecovery (const mzero) (char 'a' *> char 'd')
        r = posErr 1 s [uneCh 'b', exCh 'd']
        s = "abc"

case_eof :: Assertion
case_eof = checkCase eof (Right ()) ""

prop_token :: String -> Property
prop_token s = checkParser' p r s
  where p :: MonadParsec s m Char => m Char
        p = token testChar
        testChar x = if isLetter x
          then Right x
          else Left . pure . Unexpected . showToken $ x
        h = head s
        r | null s = posErr 0 s [uneEof]
          | isLetter h && length s == 1 = Right (head s)
          | isLetter h && length s > 1 = posErr 1 s [uneCh (s !! 1), exEof]
          | otherwise = posErr 0 s [uneCh h]

prop_tokens_0 :: String -> String -> Property
prop_tokens_0 a = checkString p a (==) (showToken a)
  where p = tokens (==) a

prop_tokens_1 :: String -> String -> String -> Property
prop_tokens_1 pre post post' =
  not (post `isPrefixOf` post') ==>
  (leftover === "" .||. leftover === s)
  where p = tokens (==) (pre ++ post)
        s = pre ++ post'
        st = stateFromInput s
        leftover = stateInput . fst $ runParser' p st

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

prop_state_tab :: Int -> Property
prop_state_tab w = p /=\ w
  where p = setTabWidth w >> getTabWidth

prop_state :: State String -> State String -> Property
prop_state s1 s2 = checkParser' p r s
  where p :: MonadParsec String m Char => m (State String)
        p = do
          st <- getParserState
          guard (st == State s (initialPos "") defaultTabWidth)
          setParserState s1
          updateParserState (f s2)
          liftM2 const getParserState (setInput "")
        f (State s1' pos w) (State s2' _ _) = State (max s1' s2' ) pos w
        r = Right (f s2 s1)
        s = ""

-- Running a parser

prop_runParser' :: State String -> String -> Property
prop_runParser' st s = runParser' p st === r
  where p = string s
        r = emulateStrParsing st s

prop_runParserT' :: State String -> String -> Property
prop_runParserT' st s = runIdentity (runParserT' p st) === r
  where p = string s
        r = emulateStrParsing st s

emulateStrParsing :: State String
                  -> String
                  -> (State String, Either ParseError String)
emulateStrParsing st@(State i pos t) s =
  if l == length s
  then (State (drop l i) (updatePosString t pos s) t, Right s)
  else let uneStuff = if null i then uneEof else uneStr (take (l + 1) i)
       in (st, Left $ newErrorMessages (exStr s : [uneStuff]) pos)
  where l = length $ takeWhile id $ zipWith (==) s i

-- Additional tests to check returned state on failure

prop_stOnFail_0 :: Positive Int -> Positive Int -> Property
prop_stOnFail_0 na' nb' = runParser' p (stateFromInput s) === (i, r)
  where i = let (Left x) = r in State "" (errorPos x) defaultTabWidth
        na = getPositive na'
        nb = getPositive nb'
        p = try (many (char 'a') <* many (char 'b') <* char 'c')
          <|> (many (char 'a') <* char 'c')
        r = posErr (na + nb) s [exCh 'b', exCh 'c', uneEof]
        s = replicate na 'a' ++ replicate nb 'b'

prop_stOnFail_1 :: Positive Int -> Positive Int -> Property
prop_stOnFail_1 na' t' = runParser' p (stateFromInput s) === (i, r)
  where i = let (Left x) = r in State "" (errorPos x) t
        na = getPositive na'
        t = getPositive t'
        p = many (char 'a') <* setTabWidth t <* fail myMsg
        r = posErr na s [msg myMsg]
        s = replicate na 'a'
        myMsg = "failing now!"

prop_stOnFail_2 :: String -> Char -> Property
prop_stOnFail_2 s' ch = runParser' p (stateFromInput s) === (i, r)
  where i = let (Left x) = r in State [ch] (errorPos x) defaultTabWidth
        r = posErr (length s') s [uneCh ch, exEof]
        p = string s' <* eof
        s = s' ++ [ch]

prop_stOnFail_3 :: String -> Property
prop_stOnFail_3 s = runParser' p (stateFromInput s) === (i, r)
  where i = let (Left x) = r in State s (errorPos x) defaultTabWidth
        r = posErr 0 s [if null s then uneEof else uneCh (head s)]
        p = notFollowedBy (string s)

stateFromInput :: s -> State s
stateFromInput s = State s (initialPos "") defaultTabWidth

-- ReaderT instance of MonadParsec

prop_ReaderT_try :: Char -> Char -> Char -> Property
prop_ReaderT_try pre ch1 ch2 = checkParser (runReaderT p (s1, s2)) r s
  where s1 = pre : [ch1]
        s2 = pre : [ch2]
        getS1 = asks fst
        getS2 = asks snd
        p = try (g =<< getS1) <|> (g =<< getS2)
        g = sequence . fmap char
        r = posErr 1 s [uneEof, exCh ch1, exCh ch2]
        s = [pre]

prop_ReaderT_notFollowedBy :: NonNegative Int -> NonNegative Int
                           -> NonNegative Int -> Property
prop_ReaderT_notFollowedBy a' b' c' = checkParser (runReaderT p 'a') r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char =<< ask) <* notFollowedBy eof <* many anyChar
        r | b > 0 || c > 0 = Right (replicate a 'a')
          | otherwise      = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c

-- StateT instance of MonadParsec

prop_StateT_alternative :: Integer -> Property
prop_StateT_alternative n =
  checkParser (L.evalStateT p 0) (Right n) "" .&&.
  checkParser (S.evalStateT p' 0) (Right n) ""
  where p  = L.put n >> ((L.modify (* 2) >>
                          void (string "xxx")) <|> return ()) >> L.get
        p' = S.put n >> ((S.modify (* 2) >>
                          void (string "xxx")) <|> return ()) >> S.get

prop_StateT_lookAhead :: Integer -> Property
prop_StateT_lookAhead n =
  checkParser (L.evalStateT p 0) (Right n) "" .&&.
  checkParser (S.evalStateT p' 0) (Right n) ""
  where p  = L.put n >> lookAhead (L.modify (* 2) >> eof) >> L.get
        p' = S.put n >> lookAhead (S.modify (* 2) >> eof) >> S.get

prop_StateT_notFollowedBy :: Integer -> Property
prop_StateT_notFollowedBy n = checkParser (L.runStateT p 0) r "abx" .&&.
                              checkParser (S.runStateT p' 0) r "abx"
  where p = do
          L.put n
          let notEof = notFollowedBy (L.modify (* 2) >> eof)
          some (try (anyChar <* notEof)) <* char 'x'
        p' = do
          S.put n
          let notEof = notFollowedBy (S.modify (* 2) >> eof)
          some (try (anyChar <* notEof)) <* char 'x'
        r = Right ("ab", n)

-- WriterT instance of MonadParsec

prop_WriterT :: String -> String -> Property
prop_WriterT pre post =
  checkParser (L.runWriterT p) r "abx" .&&.
  checkParser (S.runWriterT p') r "abx"
  where logged_letter  = letterChar >>= \x -> L.tell [x] >> return x
        logged_letter' = letterChar >>= \x -> L.tell [x] >> return x
        logged_eof     = eof >> L.tell "EOF"
        logged_eof'    = eof >> L.tell "EOF"
        p = do
          L.tell pre
          cs <- L.censor (fmap toUpper) $
                  some (try (logged_letter <* notFollowedBy logged_eof))
          L.tell post
          void logged_letter
          return cs
        p' = do
          L.tell pre
          cs <- L.censor (fmap toUpper) $
                  some (try (logged_letter' <* notFollowedBy logged_eof'))
          L.tell post
          void logged_letter'
          return cs
        r = Right ("ab", pre ++ "AB" ++ post ++ "x")
