-- |
-- Module      :  Test.Hspec.Megaparsec
-- Copyright   :  © 2016–2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions for testing Megaparsec parsers with Hspec.

{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

module Test.Hspec.Megaparsec
  ( -- * Basic expectations
    shouldParse
  , parseSatisfies
  , shouldSucceedOn
  , shouldFailOn
    -- * Testing of error messages
  , shouldFailWith
    -- * Incremental parsing
  , failsLeaving
  , succeedsLeaving
  , initialState
    -- * Re-exports
  , module Text.Megaparsec.Error.Builder )
where

import Control.Monad (unless)
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec.Expectations
import Text.Megaparsec
import Text.Megaparsec.Error.Builder

----------------------------------------------------------------------------
-- Basic expectations

-- | Create an expectation by saying what the result should be.
--
-- > parse letterChar "" "x" `shouldParse` 'x'

shouldParse :: (Ord t, ShowToken t, ShowErrorComponent e, Eq a, Show a)
  => Either (ParseError t e) a
     -- ^ Result of parsing as returned by function like 'parse'
  -> a                 -- ^ Desired result
  -> Expectation
r `shouldParse` v = case r of
  Left e -> expectationFailure $ "expected: " ++ show v ++
    "\nbut parsing failed with error:\n" ++ showParseError e
  Right x -> unless (x == v) . expectationFailure $
    "expected: " ++ show v ++ "\nbut got: " ++ show x

-- | Create an expectation by saying that the parser should successfully
-- parse a value and that the value should satisfy some predicate.
--
-- > parse (many punctuationChar) "" "?!!" `parseSatisfies` ((== 3) . length)

parseSatisfies :: (Ord t, ShowToken t, ShowErrorComponent e, Show a)
  => Either (ParseError t e) a
     -- ^ Result of parsing as returned by function like 'parse'
  -> (a -> Bool)       -- ^ Predicate
  -> Expectation
r `parseSatisfies` p = case r of
  Left e -> expectationFailure $
    "expected a parsed value to check against the predicate" ++
    "\nbut parsing failed with error:\n" ++ showParseError e
  Right x -> unless (p x) . expectationFailure $
    "the value did not satisfy the predicate: " ++ show x

-- | Check that a parser fails on a given input.
--
-- > parse (char 'x') "" `shouldFailOn` "a"

shouldFailOn :: Show a
  => (s -> Either (ParseError t e) a)
     -- ^ Parser that takes stream and produces result or error message
  -> s                 -- ^ Input that the parser should fail on
  -> Expectation
p `shouldFailOn` s = shouldFail (p s)

-- | Check that a parser succeeds on a given input.
--
-- > parse (char 'x') "" `shouldSucceedOn` "x"

shouldSucceedOn :: (Ord t, ShowToken t, ShowErrorComponent e, Show a)
  => (s -> Either (ParseError t e) a)
     -- ^ Parser that takes stream and produces result or error message
  -> s                 -- ^ Input that the parser should succeed on
  -> Expectation
p `shouldSucceedOn` s = shouldSucceed (p s)

----------------------------------------------------------------------------
-- Testing of error messages

-- | Create an expectation that parser should fail producing certain
-- 'ParseError'. Use the 'err' function from this module to construct a
-- 'ParseError' to compare with.
--
-- > parse (char 'x') "" "b" `shouldFailWith` err posI (utok 'b' <> etok 'x')

shouldFailWith :: (Ord t, ShowToken t, ShowErrorComponent e, Show a)
  => Either (ParseError t e) a
  -> ParseError t e
  -> Expectation
r `shouldFailWith` e = case r of
  Left e' -> unless (e == e') . expectationFailure $
    "the parser is expected to fail with:\n" ++ showParseError e ++
    "but it failed with:\n" ++ showParseError e'
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v

----------------------------------------------------------------------------
-- Incremental parsing

-- | Check that a parser fails and leaves a certain part of input
-- unconsumed. Use it with functions like 'runParser'' and 'runParserT''
-- that support incremental parsing.
--
-- > runParser' (many (char 'x') <* eof) (initialState "xxa")
-- >   `failsLeaving` "a"
--
-- See also: 'initialState'.

failsLeaving :: (Show a, Eq s, Show s, Stream s)
  => (State s, Either (ParseError (Token s) e) a)
     -- ^ Parser that takes stream and produces result along with actual
     -- state information
  -> s                 -- ^ Part of input that should be left unconsumed
  -> Expectation
(st,r) `failsLeaving` s =
  shouldFail r >> checkUnconsumed s (stateInput st)

-- | Check that a parser succeeds and leaves certain part of input
-- unconsumed. Use it with functions like 'runParser'' and 'runParserT''
-- that support incremental parsing.
--
-- > runParser' (many (char 'x')) (initialState "xxa")
-- >   `succeedsLeaving` "a"
--
-- See also: 'initialState'.

succeedsLeaving :: ( ShowToken (Token s)
                   , ShowErrorComponent e
                   , Show a
                   , Eq s
                   , Show s
                   , Stream s )
  => (State s, Either (ParseError (Token s) e) a)
     -- ^ Parser that takes stream and produces result along with actual
     -- state information
  -> s                 -- ^ Part of input that should be left unconsumed
  -> Expectation
(st,r) `succeedsLeaving` s =
  shouldSucceed r >> checkUnconsumed s (stateInput st)

-- | Given input for parsing, construct initial state for parser (that is,
-- with empty file name, default tab width and position at 1 line and 1
-- column).

initialState :: s -> State s
initialState s = State
  { stateInput           = s
  , statePos             = initialPos "" :| []
#if MIN_VERSION_megaparsec(5,2,0)
  , stateTokensProcessed = 0
#endif
  , stateTabWidth        = defaultTabWidth }

----------------------------------------------------------------------------
-- Helpers

-- | Expectation that argument is result of a failed parser.

shouldFail :: Show a
  => Either (ParseError t e) a
  -> Expectation
shouldFail r = case r of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v

-- | Expectation that argument is result of a succeeded parser.

shouldSucceed :: (Ord t, ShowToken t, ShowErrorComponent e, Show a)
  => Either (ParseError t e) a
  -> Expectation
shouldSucceed r = case r of
  Left e -> expectationFailure $
    "the parser is expected to succeed, but it failed with:\n" ++
    showParseError e
  Right _ -> return ()

-- | Compare two streams for equality and in the case of mismatch report it.

checkUnconsumed :: (Eq s, Show s, Stream s)
  => s                 -- ^ Expected unconsumed input
  -> s                 -- ^ Actual unconsumed input
  -> Expectation
checkUnconsumed e a = unless (e == a) . expectationFailure $
  "the parser is expected to leave unconsumed input: " ++ show e ++
  "\nbut it left this: " ++ show a

-- | Render parse error in a way that is suitable for inserting it in a test
-- suite report.

showParseError :: (Ord t, ShowToken t, ShowErrorComponent e)
  => ParseError t e
  -> String
showParseError = unlines . fmap ("  " ++) . lines . parseErrorPretty
