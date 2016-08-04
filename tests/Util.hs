--
-- QuickCheck tests for Megaparsec, utility functions for parser testing.
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

{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}
{-# OPTIONS -fno-warn-orphans #-}

module Util
  ( checkParser
  , checkParser'
  , checkCase
  , checkCase'
  , simpleParse
  , checkChar
  , checkString
  , updatePosString
  , (/=\)
  , (!=!)
  , abcRow
  , EC (..)
  , posErr
  , posErr'
  , utok
  , utoks
  , ulabel
  , ueof
  , etok
  , etoks
  , elabel
  , eeof
  , cstm )
where

import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (mapMaybe, maybeToList)
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S
import qualified Data.List.NonEmpty          as NE
import qualified Data.Set                    as E

import Test.QuickCheck
import Test.HUnit (Assertion, (@?=))

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), (<*))
#endif

-- | @checkParser p r s@ tries to run parser @p@ on input @s@ to parse
-- entire @s@. Result of the parsing is compared with expected result @r@,
-- it should match, otherwise the property doesn't hold and the test fails.

checkParser :: (Eq a, Show a)
  => Parser a          -- ^ Parser to test
  -> Either (ParseError Char Dec) a -- ^ Expected result of parsing
  -> String            -- ^ Input for the parser
  -> Property          -- ^ Resulting property
checkParser p r s = simpleParse p s === r

-- | A variant of 'checkParser' that runs given parser code with all
-- standard instances of 'MonadParsec'. Useful when testing primitive
-- combinators.

checkParser' :: (Eq a, Show a)
  => (forall m. MonadParsec Dec String m => m a) -- ^ Parser to test
  -> Either (ParseError Char Dec) a -- ^ Expected result of parsing
  -> String            -- ^ Input for the parser
  -> Property          -- ^ Resulting property
checkParser' p r s = conjoin
  [ checkParser p                   r s
  , checkParser (runIdentityT p)    r s
  , checkParser (runReaderT   p ()) r s
  , checkParser (L.evalStateT p ()) r s
  , checkParser (S.evalStateT p ()) r s
  , checkParser (evalWriterTL p)    r s
  , checkParser (evalWriterTS p)    r s ]

-- | Similar to 'checkParser', but produces HUnit's 'Assertion's instead.

checkCase :: (Eq a, Show a)
  => Parser a          -- ^ Parser to test
  -> Either (ParseError Char Dec) a -- ^ Expected result of parsing
  -> String            -- ^ Input for the parser
  -> Assertion         -- ^ Resulting assertion
checkCase p r s = simpleParse p s @?= r

-- | Similar to 'checkParser'', but produces HUnit's 'Assertion's instead.

checkCase' :: (Eq a, Show a)
  => (forall m. MonadParsec Dec String m => m a) -- ^ Parser to test
  -> Either (ParseError Char Dec) a -- ^ Expected result of parsing
  -> String            -- ^ Input for the parser
  -> Assertion         -- ^ Resulting assertion
checkCase' p r s = do
  parse p                   "" s @?= r
  parse (runIdentityT p)    "" s @?= r
  parse (runReaderT   p ()) "" s @?= r
  parse (L.evalStateT p ()) "" s @?= r
  parse (S.evalStateT p ()) "" s @?= r
  parse (evalWriterTL p)    "" s @?= r
  parse (evalWriterTS p)    "" s @?= r

evalWriterTL :: Monad m => L.WriterT [Int] m a -> m a
evalWriterTL = liftM fst . L.runWriterT
evalWriterTS :: Monad m => S.WriterT [Int] m a -> m a
evalWriterTS = liftM fst . S.runWriterT

-- | @simpleParse p s@ runs parser @p@ on input @s@ and returns corresponding
-- result of type @Either ParseError a@, where @a@ is type of parsed
-- value. This parser tries to parser end of file too and name of input file
-- is always empty string.

simpleParse :: Parser a -> String -> Either (ParseError Char Dec) a
simpleParse p = parse (p <* eof) ""

-- | @checkChar p test label s@ runs parser @p@ on input @s@ and checks if
-- the parser correctly parses single character that satisfies @test@. The
-- character may be labelled, in this case @label@ is used to check quality
-- of error messages.

checkChar
  :: Parser Char       -- ^ Parser to run
  -> (Char -> Bool)    -- ^ Predicate to test parsed char
  -> Maybe (ErrorItem Char) -- ^ Representation to use in error messages
  -> String            -- ^ Input stream
  -> Property          -- ^ Resulting property
checkChar p f rep' s = checkParser p r s
  where h = head s
        rep = Expected <$> maybeToList rep'
        r | null s = posErr 0 s (ueof : rep)
          | length s == 1 && f h = Right h
          | not (f h) = posErr 0 s (utok h : rep)
          | otherwise = posErr 1 s [utok (s !! 1), eeof]

-- | @checkString p a test label s@ runs parser @p@ on input @s@ and checks if
-- the result is equal to @a@ and also quality of error messages. @test@ is
-- used to compare tokens. @label@ is used as expected representation of
-- parser's result in error messages.

checkString
  :: Parser String     -- ^ Parser to run
  -> String            -- ^ Expected result
  -> (Char -> Char -> Bool) -- ^ Function used to compare tokens
  -> String            -- ^ Input stream
  -> Property
checkString p a' test s' = checkParser p (w a' 0 s') s'
  where w [] _ []    = Right s'
        w [] i (s:_) = posErr i s' [utok s, eeof]
        w _  0 []    = posErr 0 s' [ueof, etoks a']
        w _  i []    = posErr 0 s' [utoks (take i s'), etoks a']
        w (a:as) i (s:ss)
          | test a s  = w as i' ss
          | otherwise = posErr 0 s' [utoks (take i' s'), etoks a']
            where i'  = succ i

-- | A helper function that is used to advance 'SourcePos' given a 'String'.

updatePosString
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Initial position
  -> String            -- ^ 'String' — collection of tokens to process
  -> SourcePos         -- ^ Final position
updatePosString w = foldl' f
  where f p t = snd (defaultUpdatePos w p t)

infix 4 /=\   -- preserve whitespace on automatic trim

-- | @p /=\\ x@ runs parser @p@ on empty input and compares its result
-- (which should be successful) with @x@. Succeeds when the result is equal
-- to @x@, prints counterexample on failure.

(/=\) :: (Eq a, Show a) => Parser a -> a -> Property
p /=\ x = simpleParse p "" === Right x

infix 4 !=!

-- | @n !=! m@ represents property that holds when results of running @n@
-- and @m@ parsers are identical. This is useful when checking monad laws
-- for example.

(!=!) :: (Eq a, Show a) => Parser a -> Parser a -> Property
n !=! m = simpleParse n "" === simpleParse m ""

-- | @abcRow a b c@ generates string consisting of character “a” repeated
-- @a@ times, character “b” repeated @b@ times, and finally character “c”
-- repeated @c@ times.

abcRow :: Enum a => a -> a -> a -> String
abcRow a b c = f a 'a' ++ f b 'b' ++ f c 'c'
  where f x = replicate (fromEnum x)

-- | A component of parse error, useful for fast and dirty construction of
-- parse errors with 'posErr' and other helpers.

data EC
  = Unexpected (ErrorItem Char)
  | Expected   (ErrorItem Char)
  | Custom     Dec

#if !MIN_VERSION_QuickCheck(2,9,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList . getNonEmpty <$> arbitrary
#endif

instance Arbitrary t => Arbitrary (ErrorItem t) where
  arbitrary = oneof
    [ Tokens <$> arbitrary
    , Label  <$> arbitrary
    , return EndOfInput ]

instance Arbitrary Pos where
  arbitrary = unsafePos <$> (getSmall <$> arbitrary `suchThat` (> 0))

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> shortString
    <*> (unsafePos <$> choose (1, 1000))
    <*> (unsafePos <$> choose (1,  100))

instance Arbitrary Dec where
  arbitrary = oneof
    [ DecFail        <$> shortString
    , DecIndentation <$> arbitrary <*> arbitrary <*> arbitrary ]

instance (Arbitrary t, Ord t, Arbitrary e, Ord e)
    => Arbitrary (ParseError t e) where
  arbitrary = ParseError
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

shortString :: Gen String
shortString = sized $ \n -> do
  k <- choose (0, n `div` 2)
  vectorOf k arbitrary

-- | @posErr pos s ms@ is an easy way to model result of parser that fails.
-- @pos@ is how many tokens (characters) has been consumed before failure.
-- @s@ is input of the parser. @ms@ is a list, collection of 'Message's. See
-- 'utok', 'utoks', 'ulabel', 'ueof', 'etok', 'etoks', 'elabel', and 'eeof'
-- for easy ways to create error messages.

posErr
  :: Int               -- ^ How many tokens to drop from beginning of steam
  -> String            -- ^ The input stream (just a 'String' here)
  -> [EC]              -- ^ Collection of error components
  -> Either (ParseError Char Dec) a -- ^ 'ParseError' inside of 'Left'
posErr i s = posErr' (pos :| [])
  where pos = updatePosString defaultTabWidth (initialPos "") (take i s)

-- | The same as 'posErr', but 'SourcePos' should be provided directly.

posErr'
  :: NonEmpty SourcePos -- ^ Position of the error
  -> [EC]              -- ^ Collection of error components
  -> Either (ParseError Char Dec) a -- ^ 'ParseError' inside of 'Left'
posErr' pos ecs = Left ParseError
  { errorPos        = pos
  , errorUnexpected = E.fromList (mapMaybe getUnexpected ecs)
  , errorExpected   = E.fromList (mapMaybe getExpected   ecs)
  , errorCustom     = E.fromList (mapMaybe getCustom     ecs) }
  where
    getUnexpected (Unexpected x) = Just x
    getUnexpected _              = Nothing
    getExpected   (Expected   x) = Just x
    getExpected   _              = Nothing
    getCustom     (Custom     x) = Just x
    getCustom     _              = Nothing

-- | Construct “unexpected token” error component.

utok :: Char -> EC
utok = Unexpected . Tokens . nes

-- | Construct “unexpected steam” error component. This function respects
-- some conventions described in 'canonicalizeStream'.

utoks :: String -> EC
utoks = Unexpected . canonicalizeStream

-- | Construct “unexpected label” error component. Do not use with empty
-- strings.

ulabel :: String -> EC
ulabel = Unexpected . Label . NE.fromList

-- | Construct “unexpected end of input” error component.

ueof :: EC
ueof = Unexpected EndOfInput

-- | Construct “expecting token” error component.

etok :: Char -> EC
etok = Expected . Tokens . nes

-- | Construct “expecting stream” error component. This function respects
-- some conventions described in 'canonicalizeStream'.

etoks :: String -> EC
etoks = Expected . canonicalizeStream

-- | Construct “expecting label” error component. Do not use with empty
-- strings.

elabel :: String -> EC
elabel = Expected . Label . NE.fromList

-- | Construct “expecting end of input” component.

eeof :: EC
eeof = Expected EndOfInput

-- | Construct error component consisting of custom data.

cstm :: Dec -> EC
cstm = Custom

-- | Construct appropriate 'MessageItem' representation for given token
-- stream. Empty string produces 'EndOfInput', single token — a 'Token', and
-- in other cases the 'TokenStream' constructor is used.

canonicalizeStream :: String -> ErrorItem Char
canonicalizeStream stream =
  case NE.nonEmpty stream of
    Nothing      -> EndOfInput
    Just xs      -> Tokens xs

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}
