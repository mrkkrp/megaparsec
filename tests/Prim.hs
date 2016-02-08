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

{-# OPTIONS -fno-warn-orphans #-}

module Prim (tests) where

import Control.Applicative
import Data.Char (isLetter, toUpper)
import Data.Foldable (asum)
import Data.List (isPrefixOf)
import Data.Maybe (maybeToList, fromMaybe)

import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck hiding (label)

import Text.Megaparsec.Char
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String

import Pos ()
import Error ()
import Util

tests :: Test
tests = testGroup "Primitive parser combinators"
        [ testProperty "ParsecT functor"                     prop_functor
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
        , testProperty "combinator notFollowedBy"       prop_notFollowedBy_0
        , testProperty "combinator notFollowedBy twice" prop_notFollowedBy_1
        , testProperty "combinator notFollowedBy eof"   prop_notFollowedBy_2
        , testProperty "combinator token"                    prop_token
        , testProperty "combinator tokens"                   prop_tokens
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
        , testProperty "IdentityT try"            prop_IdentityT_try
        , testProperty "IdentityT notFollowedBy"  prop_IdentityT_notFollowedBy
        , testProperty "ReaderT try"              prop_ReaderT_try
        , testProperty "ReaderT notFollowedBy"    prop_ReaderT_notFollowedBy
        , testProperty "StateT alternative (<|>)" prop_StateT_alternative
        , testProperty "StateT lookAhead"         prop_StateT_lookAhead
        , testProperty "StateT notFollowedBy"     prop_StateT_notFollowedBy
        , testProperty "WriterT"                  prop_WriterT ]

instance Arbitrary (State String) where
  arbitrary = State <$> arbitrary <*> arbitrary <*> choose (1, 20)

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
    where p   = try (string s0) <|> string s1
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
  where p  = asum [empty, try (string ">>>"), empty, return "foo"] <?> "bar"
        p' = bsum [empty, try (string ">>>"), empty, return "foo"] <?> "bar"
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
prop_unexpected m = checkParser p r s
  where p = unexpected m :: Parser ()
        r | null m    = posErr 0 s []
          | otherwise = posErr 0 s [uneSpec m]
        s = ""

prop_failure :: [Message] -> Property
prop_failure msgs = conjoin [ checkParser p r s
                            , checkParser (runIdentityT p_IdentityT) r s
                            , checkParser (runReaderT p_ReaderT ()) r s
                            , checkParser (L.evalStateT p_lStateT ()) r s
                            , checkParser (S.evalStateT p_sStateT ()) r s
                            , checkParser (L.runWriterT p_lWriterT) r s
                            , checkParser (S.runWriterT p_sWriterT) r s ]
  where p = failure msgs :: Parser ()
        p_IdentityT = failure msgs :: IdentityT Parser ()
        p_ReaderT   = failure msgs :: ReaderT () Parser ()
        p_lStateT   = failure msgs :: L.StateT () Parser ()
        p_sStateT   = failure msgs :: S.StateT () Parser ()
        p_lWriterT  = failure msgs :: L.WriterT [Integer] Parser ()
        p_sWriterT  = failure msgs :: S.WriterT [Integer] Parser ()
        r | null msgs = posErr 0 s []
          | otherwise = Left $ newErrorMessages msgs (initialPos "")
        s = ""

prop_label :: NonNegative Int -> NonNegative Int
           -> NonNegative Int -> String -> Property
prop_label a' b' c' l = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> (many (char 'b') <?> l)
        r | null s = Right s
          | c > 0 = posErr (a + b) s $ [uneCh 'c', exEof]
                    ++ [exCh 'a' | b == 0]
                    ++ [if b == 0 || null l
                        then exSpec l
                        else exSpec $ "rest of " ++ l]
          | otherwise = Right s
        s = abcRow a b c

prop_hidden_0 :: NonNegative Int -> NonNegative Int
              -> NonNegative Int -> Property
prop_hidden_0 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = (++) <$> many (char 'a') <*> hidden (many (char 'b'))
        r | null s = Right s
          | c > 0  = posErr (a + b) s $ [uneCh 'c', exEof]
                     ++ [exCh 'a' | b == 0]
          | otherwise = Right s
        s = abcRow a b c

prop_hidden_1 :: String -> NonEmptyList Char -> String -> Property
prop_hidden_1 a c' s = checkParser p r s
  where c = getNonEmpty c'
        p = fromMaybe a <$> optional (hidden $ string c)
        r | null s = Right a
          | c == s = Right s
          | head c /= head s = posErr 0 s [uneCh (head s), exEof]
          | otherwise = simpleParse (string c) s

prop_try :: String -> String -> String -> Property
prop_try pre s1' s2' = checkParser p r s
  where s1 = pre ++ s1'
        s2 = pre ++ s2'
        p = try (string s1) <|> string s2
        r | s == s1 || s == s2 = Right s
          | otherwise = posErr 0 s $ (if null s then uneEof else uneStr pre)
                        : [uneStr pre, exStr s1, exStr s2]
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

prop_notFollowedBy_0 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
prop_notFollowedBy_0 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* notFollowedBy (char 'b') <* many (char 'c')
        r | b > 0 = posErr a s [uneCh 'b', exCh 'a']
          | otherwise = Right (replicate a 'a')
        s = abcRow a b c

prop_notFollowedBy_1 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
prop_notFollowedBy_1 a' b' c' = checkParser p r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* f (char 'c') <* many (char 'c')
        f = notFollowedBy . notFollowedBy -- = 'lookAhead' in this case
        r | b == 0 && c > 0 = Right (replicate a 'a')
          | b > 0           = posErr a s [uneCh 'b', exCh 'a']
          | otherwise       = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c

prop_notFollowedBy_2 :: NonNegative Int -> NonNegative Int
                     -> NonNegative Int -> Property
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
  where p = token updatePosChar testChar
        testChar x = if isLetter x
                     then Right x
                     else Left . pure . Unexpected . showToken $ x
        h = head s
        r | null s = posErr 0 s [uneEof]
          | isLetter h && length s == 1 = Right (head s)
          | isLetter h && length s > 1 = posErr 1 s [uneCh (s !! 1), exEof]
          | otherwise = posErr 0 s [uneCh h]

prop_tokens :: String -> String -> Property
prop_tokens a = checkString p a (==) (showToken a)
  where p = tokens updatePosString (==) a

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
prop_state s1 s2 = runParser p "" "" === Right (f s2 s1)
  where f (State s1' pos w) (State s2' _ _) = State (max s1' s2' ) pos w
        p = do
          st <- getParserState
          guard (st == State "" (initialPos "") defaultTabWidth)
          setParserState s1
          updateParserState (f s2)
          getParserState

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

stateFromInput :: Stream s t => s -> State s
stateFromInput s = State s (initialPos "") defaultTabWidth

-- IdentityT instance of MonadParsec

prop_IdentityT_try :: String -> String -> String -> Property
prop_IdentityT_try pre s1' s2' = checkParser (runIdentityT p) r s
  where s1 = pre ++ s1'
        s2 = pre ++ s2'
        p = try (string s1) <|> string s2
        r | s == s1 || s == s2 = Right s
          | otherwise = posErr 0 s $ (if null s then uneEof else uneStr pre)
                        : [uneStr pre, exStr s1, exStr s2]
        s = pre

prop_IdentityT_notFollowedBy :: NonNegative Int -> NonNegative Int
                             -> NonNegative Int -> Property
prop_IdentityT_notFollowedBy a' b' c' = checkParser (runIdentityT p) r s
  where [a,b,c] = getNonNegative <$> [a',b',c']
        p = many (char 'a') <* notFollowedBy eof <* many anyChar
        r | b > 0 || c > 0 = Right (replicate a 'a')
          | otherwise      = posErr a s [uneEof, exCh 'a']
        s = abcRow a b c

-- ReaderT instance of MonadParsec

prop_ReaderT_try :: String -> String -> String -> Property
prop_ReaderT_try pre s1' s2' = checkParser (runReaderT p (s1', s2')) r s
  where s1 = pre ++ s1'
        s2 = pre ++ s2'
        getS1 = asks ((pre ++) . fst)
        getS2 = asks ((pre ++) . snd)
        p = try (string =<< getS1) <|> (string =<< getS2)
        r | s == s1 || s == s2 = Right s
          | otherwise = posErr 0 s $ (if null s then uneEof else uneStr pre)
                        : [uneStr pre, exStr s1, exStr s2]
        s = pre

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
