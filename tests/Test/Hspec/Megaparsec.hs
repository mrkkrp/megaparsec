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

{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE DeriveGeneric       #-}
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
    -- * Error message construction
    -- $errmsg
  , err
  , posI
  , posN
  , EC
  , utok
  , utoks
  , ulabel
  , ueof
  , etok
  , etoks
  , elabel
  , eeof
  , fancy
    -- * Incremental parsing
  , failsLeaving
  , succeedsLeaving
  , initialState )
where

import Control.Monad (unless)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import GHC.Generics
import Test.Hspec.Expectations
import Text.Megaparsec
import qualified Data.List.NonEmpty as NE
import qualified Data.Set           as E

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
-- Error message construction

-- $errmsg
--
-- When you wish to test error message on failure, the need to construct a
-- error message for comparison arises. These helpers allow to construct
-- virtually any sort of error message easily.

-- | Assemble a 'ParseErorr' from source position and @'EC' t e@ value. To
-- create source position, two helpers are available: 'posI' and 'posN'.
-- @'EC' t e@ is a monoid and can be built from primitives provided by this
-- module, see below.
--
-- @since 0.3.0

err
  :: NonEmpty SourcePos -- ^ 'ParseError' position
  -> EC t e             -- ^ Error components
  -> ParseError t e     -- ^ Resulting 'ParseError'
err pos (ETrivial us ps) = TrivialError pos us ps
err pos (EFancy   xs)    = FancyError   pos xs

-- | Initial source position with empty file name.
--
-- @since 0.3.0

posI :: NonEmpty SourcePos
posI = initialPos "" :| []

-- | @posN n s@ returns source position achieved by applying 'updatePos'
-- method corresponding to type of stream @s@ @n@ times.
--
-- @since 0.3.0

posN :: forall s n. (Stream s, Integral n)
  => n
  -> s
  -> NonEmpty SourcePos
posN n see = f (initialPos "") see n :| []
  where
    f p s !i =
      if i > 0
        then case uncons s of
          Nothing -> p
          Just (t,s') ->
            let p' = snd $ updatePos (Proxy :: Proxy s) defaultTabWidth p t
            in f p' s' (i - 1)
        else p

-- | Auxiliary type for construction of 'ParseError's. Note that it's a
-- monoid.
--
-- @since 0.3.0

data EC t e
  = ETrivial (Set (ErrorItem t)) (Set (ErrorItem t))
  | EFancy   (Set (ErrorFancy e))
  deriving (Eq, Data, Typeable, Generic)

instance (Ord t, Ord e) => Semigroup (EC t e) where
  (ETrivial u0 e0) <> (ETrivial u1 e1) =
    ETrivial (E.union u0 u1) (E.union e0 e1)
  (EFancy x0) <> (ETrivial _ _) = EFancy x0
  (ETrivial _ _) <> (EFancy x0) = EFancy x0
  (EFancy x0) <> (EFancy x1)    = EFancy (E.union x0 x1)

instance (Ord t, Ord e) => Monoid (EC t e) where
  mempty  = ETrivial E.empty E.empty
  mappend = (<>)

-- | Construct an “unexpected token” error component.
--
-- @since 0.3.0

utok :: (Ord t, Ord e) => t -> EC t e
utok = unexp . Tokens . nes

-- | Construct an “unexpected tokens” error component. Empty string produces
-- 'EndOfInput'.
--
-- @since 0.3.0

utoks :: (Ord t, Ord e) => [t] -> EC t e
utoks = unexp . canonicalizeTokens

-- | Construct an “unexpected label” error component. Do not use with empty
-- strings (for empty strings it's bottom).
--
-- @since 0.3.0

ulabel :: (Ord t, Ord e) => String -> EC t e
ulabel = unexp . Label . NE.fromList

-- | Construct an “unexpected end of input” error component.
--
-- @since 0.3.0

ueof :: (Ord t, Ord e) => EC t e
ueof = unexp EndOfInput

-- | Construct an “expected token” error component.
--
-- @since 0.3.0

etok :: (Ord t, Ord e) => t -> EC t e
etok = expe . Tokens . nes

-- | Construct an “expected tokens” error component. Empty string produces
-- 'EndOfInput'.
--
-- @since 0.3.0

etoks :: (Ord t, Ord e) => [t] -> EC t e
etoks = expe . canonicalizeTokens

-- | Construct an “expected label” error component. Do not use with empty
-- strings.
--
-- @since 0.3.0

elabel :: (Ord t, Ord e) => String -> EC t e
elabel = expe . Label . NE.fromList

-- | Construct an “expected end of input” error component.
--
-- @since 0.3.0

eeof :: (Ord t, Ord e) => EC t e
eeof = expe EndOfInput

-- | Construct a custom error component.
--
-- @since 0.3.0

fancy :: ErrorFancy e -> EC t e
fancy = EFancy . E.singleton

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

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

-- | Construct appropriate 'ErrorItem' representation for given token
-- stream. Empty string produces 'EndOfInput'.

canonicalizeTokens :: [t] -> ErrorItem t
canonicalizeTokens ts =
  case NE.nonEmpty ts of
    Nothing -> EndOfInput
    Just xs -> Tokens xs

-- | Lift an unexpected item into 'EC'.

unexp :: ErrorItem t -> EC t e
unexp u = ETrivial (E.singleton u) E.empty

-- | Lift an expected item into 'EC'.

expe :: ErrorItem t -> EC t e
expe p = ETrivial E.empty (E.singleton p)
