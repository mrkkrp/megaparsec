-- |
-- Module      :  Test.Hspec.Megaparsec
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Utility functions for testing Megaparsec parsers with Hspec.
--
-- This version of the library should be used with Megaparsec 7.

{-# LANGUAGE ConstraintKinds     #-}
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
  , shouldFailWithM
    -- * Incremental parsing
  , failsLeaving
  , succeedsLeaving
  , initialState
  , initialPosState
    -- * Re-exports
  , module Text.Megaparsec.Error.Builder )
where

import Control.Monad (unless)
import Test.Hspec.Expectations
import Text.Megaparsec
import Text.Megaparsec.Error.Builder
import qualified Data.List.NonEmpty as NE

----------------------------------------------------------------------------
-- Basic expectations

-- | Create an expectation by saying what the result should be.
--
-- > parse letterChar "" "x" `shouldParse` 'x'

shouldParse
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     , Eq a
     )
  => Either (ParseErrorBundle s e) a
     -- ^ Result of parsing as returned by function like 'parse'
  -> a                 -- ^ Desired result
  -> Expectation
r `shouldParse` v = case r of
  Left e -> expectationFailure $ "expected: " ++ show v ++
    "\nbut parsing failed with error:\n" ++ showBundle e
  Right x -> unless (x == v) . expectationFailure $
    "expected: " ++ show v ++ "\nbut got: " ++ show x

-- | Create an expectation by saying that the parser should successfully
-- parse a value and that the value should satisfy some predicate.
--
-- > parse (many punctuationChar) "" "?!!" `parseSatisfies` ((== 3) . length)

parseSatisfies
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     , Eq a
     )
  => Either (ParseErrorBundle s e) a
     -- ^ Result of parsing as returned by function like 'parse'
  -> (a -> Bool)       -- ^ Predicate
  -> Expectation
r `parseSatisfies` p = case r of
  Left e -> expectationFailure $
    "expected a parsed value to check against the predicate" ++
    "\nbut parsing failed with error:\n" ++ showBundle e
  Right x -> unless (p x) . expectationFailure $
    "the value did not satisfy the predicate: " ++ show x

-- | Check that a parser fails on a given input.
--
-- > parse (char 'x') "" `shouldFailOn` "a"

shouldFailOn
  :: (HasCallStack, Show a)
  => (s -> Either (ParseErrorBundle s e) a)
     -- ^ Parser that takes stream and produces result or error message
  -> s                 -- ^ Input that the parser should fail on
  -> Expectation
p `shouldFailOn` s = shouldFail (p s)

-- | Check that a parser succeeds on a given input.
--
-- > parse (char 'x') "" `shouldSucceedOn` "x"

shouldSucceedOn
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     )
  => (s -> Either (ParseErrorBundle s e) a)
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

shouldFailWith
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     , Eq e
     )
  => Either (ParseErrorBundle s e) a -- ^ The result of parsing
  -> ParseError s e    -- ^ Expected parse errors
  -> Expectation
r `shouldFailWith` perr1 = r `shouldFailWithM` [perr1]

-- | Similar to 'shouldFailWith', but allows to check parsers that can
-- report more than one parse error at a time.
--
-- @since 2.0.0

shouldFailWithM
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     , Eq e
     )
  => Either (ParseErrorBundle s e) a -- ^ The result of parsing
  -> [ParseError s e]
     -- ^ Expected parse errors, the argument is a normal linked list (as
     -- opposed to the more correct 'NonEmpty' list) as a syntactical
     -- convenience for the user, passing empty list here will result in an
     -- error
  -> Expectation
r `shouldFailWithM` perrs1' = case r of
  Left e0 ->
    let e1 = e0 { bundleErrors = perrs1 }
        perrs0 = bundleErrors e0
        perrs1 = NE.fromList perrs1'
    in unless (perrs0 == perrs1) . expectationFailure $
       "the parser is expected to fail with:\n" ++ showBundle e1 ++
       "but it failed with:\n" ++ showBundle e0
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

failsLeaving
  :: ( HasCallStack
     , Show a
     , Eq s
     , Show s
     )
  => (State s, Either (ParseErrorBundle s e) a)
     -- ^ Parser that takes stream and produces result along with actual
     -- state information
  -> s                 -- ^ Part of input that should be left unconsumed
  -> Expectation
(st,r) `failsLeaving` s = do
  shouldFail r
  checkUnconsumed s (stateInput st)

-- | Check that a parser succeeds and leaves certain part of input
-- unconsumed. Use it with functions like 'runParser'' and 'runParserT''
-- that support incremental parsing.
--
-- > runParser' (many (char 'x')) (initialState "xxa")
-- >   `succeedsLeaving` "a"
--
-- See also: 'initialState'.

succeedsLeaving
  :: ( HasCallStack
     , Show a
     , Eq s
     , Show s
     , ShowErrorComponent e
     , Stream s
     )
  => (State s, Either (ParseErrorBundle s e) a)
     -- ^ Parser that takes stream and produces result along with actual
     -- state information
  -> s                 -- ^ Part of input that should be left unconsumed
  -> Expectation
(st,r) `succeedsLeaving` s = do
  shouldSucceed r
  checkUnconsumed s (stateInput st)

-- | Given input for parsing, construct initial state for parser.

initialState :: s -> State s
initialState s = State
  { stateInput  = s
  , stateOffset = 0
  , statePosState = initialPosState s
  }

-- | Given input for parsing, construct initial positional state.
--
-- @since 2.0.0

initialPosState :: s -> PosState s
initialPosState s = PosState
  { pstateInput = s
  , pstateOffset = 0
  , pstateSourcePos = initialPos ""
  , pstateTabWidth = defaultTabWidth
  , pstateLinePrefix = ""
  }

----------------------------------------------------------------------------
-- Helpers

-- | Expect that the argument is a result of a failed parser.

shouldFail
  :: (HasCallStack, Show a)
  => Either (ParseErrorBundle s e) a
  -> Expectation
shouldFail r = case r of
  Left _ -> return ()
  Right v -> expectationFailure $
    "the parser is expected to fail, but it parsed: " ++ show v

-- | Expectation that argument is result of a succeeded parser.

shouldSucceed
  :: ( HasCallStack
     , ShowErrorComponent e
     , Stream s
     , Show a
     )
  => Either (ParseErrorBundle s e) a
  -> Expectation
shouldSucceed r = case r of
  Left e -> expectationFailure $
    "the parser is expected to succeed, but it failed with:\n" ++
    showBundle e
  Right _ -> return ()

-- | Compare two streams for equality and in the case of mismatch report it.

checkUnconsumed
  :: ( HasCallStack
     , Eq s
     , Show s
     )
  => s                 -- ^ Expected unconsumed input
  -> s                 -- ^ Actual unconsumed input
  -> Expectation
checkUnconsumed e a = unless (e == a) . expectationFailure $
  "the parser is expected to leave unconsumed input: " ++ show e ++
  "\nbut it left this: " ++ show a

-- | Render a parse error bundle in a way that is suitable for inserting it
-- in a test suite report.

showBundle
  :: ( ShowErrorComponent e
     , Stream s
     )
  => ParseErrorBundle s e
  -> String
showBundle = unlines . fmap indent . lines . errorBundlePretty
  where
    indent x = if null x
      then x
      else "  " ++ x
