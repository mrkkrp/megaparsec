-- |
-- Module      :  Test.Hspec.Megaparsec.AdHoc
-- Copyright   :  © 2019 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Ad-hoc helpers used in Megaparsec's test suite.

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Hspec.Megaparsec.AdHoc
  ( -- * Types
    Parser
    -- * Helpers to run parsers
  , prs
  , prs'
  , prs_
  , grs
  , grs'
    -- * Other
  , nes
  , abcRow
  , rightOrder
  , scaleDown
  , getTabWidth
  , setTabWidth
  , strSourcePos
    -- * Char and byte conversion
  , toChar
  , fromChar
    -- * Proxies
  , sproxy
  , bproxy
  , blproxy
  , tproxy
  , tlproxy )
where

import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Char (chr, ord)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Void
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.QuickCheck
import Text.Megaparsec
import qualified Control.Monad.RWS.Lazy      as L
import qualified Control.Monad.RWS.Strict    as S
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S
import qualified Data.ByteString             as B
import qualified Data.ByteString.Lazy        as BL
import qualified Data.List.NonEmpty          as NE
import qualified Data.Set                    as E
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL

----------------------------------------------------------------------------
-- Types

-- | The type of parser that consumes a 'String'.

type Parser = Parsec Void String

----------------------------------------------------------------------------
-- Helpers to run parsers

-- | Apply parser to given input. This is a specialized version of 'parse'
-- that assumes empty file name.

prs
  :: Parser a
     -- ^ Parser to run
  -> String
     -- ^ Input for the parser
  -> Either (ParseErrorBundle String Void) a
     -- ^ Result of parsing
prs p = parse p ""

-- | Just like 'prs', but allows to inspect the final state of the parser.

prs'
  :: Parser a
     -- ^ Parser to run
  -> String
     -- ^ Input for the parser
  -> (State String, Either (ParseErrorBundle String Void) a)
     -- ^ Result of parsing
prs' p s = runParser' p (initialState s)

-- | Just like 'prs', but forces the parser to consume all input by adding
-- 'eof':
--
-- > prs_ p = parse (p <* eof) ""

prs_
  :: Parser a
     -- ^ Parser to run
  -> String
     -- ^ Input for the parser
  -> Either (ParseErrorBundle String Void) a
     -- ^ Result of parsing
prs_ p = parse (p <* eof) ""

-- | Just like 'prs', but interprets given parser as various monads (tries
-- all supported monads transformers in turn).

grs
  :: (forall m. MonadParsec Void String m => m a) -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> (Either (ParseErrorBundle String Void) a -> Expectation)
    -- ^ How to check result of parsing
  -> Expectation
grs p s r = do
  r (prs p s)
  r (prs (runIdentityT p)    s)
  r (prs (runReaderT   p ()) s)
  r (prs (L.evalStateT p ()) s)
  r (prs (S.evalStateT p ()) s)
  r (prs (evalWriterTL p)    s)
  r (prs (evalWriterTS p)    s)
  r (prs (evalRWSTL    p)    s)
  r (prs (evalRWSTS    p)    s)

-- | 'grs'' to 'grs' is as 'prs'' to 'prs'.

grs'
  :: (forall m. MonadParsec Void String m => m a) -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> ((State String, Either (ParseErrorBundle String Void) a) -> Expectation)
    -- ^ How to check result of parsing
  -> Expectation
grs' p s r = do
  r (prs' p s)
  r (prs' (runIdentityT p)    s)
  r (prs' (runReaderT   p ()) s)
  r (prs' (L.evalStateT p ()) s)
  r (prs' (S.evalStateT p ()) s)
  r (prs' (evalWriterTL p)    s)
  r (prs' (evalWriterTS p)    s)
  r (prs' (evalRWSTL    p)    s)
  r (prs' (evalRWSTS    p)    s)

evalWriterTL :: Monad m => L.WriterT [Int] m a -> m a
evalWriterTL = fmap fst . L.runWriterT
evalWriterTS :: Monad m => S.WriterT [Int] m a -> m a
evalWriterTS = fmap fst . S.runWriterT

evalRWSTL :: Monad m => L.RWST () [Int] () m a -> m a
evalRWSTL m = do
  (a,_,_) <- L.runRWST m () ()
  return a

evalRWSTS :: Monad m => S.RWST () [Int] () m a -> m a
evalRWSTS m = do
  (a,_,_) <- S.runRWST m () ()
  return a

----------------------------------------------------------------------------
-- Other

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []

-- | @abcRow a b c@ generates string consisting of character “a” repeated
-- @a@ times, character “b” repeated @b@ times, and character “c” repeated
-- @c@ times.

abcRow :: Int -> Int -> Int -> String
abcRow a b c = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

-- | Check that the given parser returns the list in the right order.

rightOrder
  :: Parser String     -- ^ The parser to test
  -> String            -- ^ Input for the parser
  -> String            -- ^ Expected result
  -> Spec
rightOrder p s s' =
  it "produces the list in the right order" $
    prs_ p s `shouldParse` s'

-- | Get tab width from 'PosState'. Use with care only for testing.

getTabWidth :: MonadParsec e s m => m Pos
getTabWidth = pstateTabWidth . statePosState <$> getParserState

-- | Set tab width in 'PosState'. Use with care only for testing.

setTabWidth :: MonadParsec e s m => Pos -> m ()
setTabWidth w = updateParserState $ \st ->
  let pst = statePosState st
  in st { statePosState = pst { pstateTabWidth = w } }

-- | Scale down.

scaleDown :: Gen a -> Gen a
scaleDown = scale (`div` 4)

-- | A helper function that is used to advance 'SourcePos' given a 'String'.

strSourcePos :: Pos -> SourcePos -> String -> SourcePos
strSourcePos tabWidth ipos input =
  let (_, pst') = reachOffset maxBound pstate in pstateSourcePos pst'
  where
    pstate = PosState
      { pstateInput = input
      , pstateOffset = 0
      , pstateSourcePos = ipos
      , pstateTabWidth = tabWidth
      , pstateLinePrefix = ""
      }

----------------------------------------------------------------------------
-- Char and byte conversion

-- | Convert a byte to char.

toChar :: Word8 -> Char
toChar = chr . fromIntegral

-- | Covert a char to byte.

fromChar :: Char -> Maybe Word8
fromChar x = let p = ord x in
  if p > 0xff
    then Nothing
    else Just (fromIntegral p)

----------------------------------------------------------------------------
-- Proxies

sproxy :: Proxy String
sproxy = Proxy

bproxy :: Proxy B.ByteString
bproxy = Proxy

blproxy :: Proxy BL.ByteString
blproxy = Proxy

tproxy :: Proxy T.Text
tproxy = Proxy

tlproxy :: Proxy TL.Text
tlproxy = Proxy

----------------------------------------------------------------------------
-- Arbitrary instances

instance Arbitrary Void where
  arbitrary = error "Arbitrary Void"

instance Arbitrary Pos where
  arbitrary = mkPos <$> (getSmall . getPositive <$> arbitrary)

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> scaleDown arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary t => Arbitrary (ErrorItem t) where
  arbitrary = oneof
    [ Tokens <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , Label  <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , return EndOfInput ]

instance Arbitrary (ErrorFancy a) where
  arbitrary = oneof
    [ ErrorFail <$> scaleDown arbitrary
    , ErrorIndentation <$> arbitrary <*> arbitrary <*> arbitrary ]

instance (Arbitrary (Token s), Ord (Token s), Arbitrary e, Ord e)
    => Arbitrary (ParseError s e) where
  arbitrary = oneof
    [ TrivialError
      <$> (getNonNegative <$> arbitrary)
      <*> arbitrary
      <*> (E.fromList <$> scaleDown arbitrary)
    , FancyError
      <$> (getNonNegative <$> arbitrary)
      <*> (E.fromList <$> scaleDown arbitrary) ]

instance Arbitrary s => Arbitrary (State s) where
  arbitrary = do
    input  <- scaleDown arbitrary
    offset <- choose (1, 10000)
    pstate :: PosState s <- arbitrary
    return State
      { stateInput = input
      , stateOffset = offset
      , statePosState = pstate
        { pstateInput = input
        , pstateOffset = offset
        }
      }

instance Arbitrary s => Arbitrary (PosState s) where
  arbitrary = PosState
    <$> arbitrary
    <*> choose (1, 10000)
    <*> arbitrary
    <*> (mkPos <$> choose (1, 20))
    <*> scaleDown arbitrary

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TL.Text where
  arbitrary = TL.pack <$> arbitrary

instance Arbitrary B.ByteString where
  arbitrary = B.pack <$> arbitrary

instance Arbitrary BL.ByteString where
  arbitrary = BL.pack <$> arbitrary

#if MIN_VERSION_QuickCheck(2,10,0)
instance Arbitrary a => Arbitrary (NonEmpty a) where
  arbitrary = NE.fromList <$> (arbitrary `suchThat` (not . null))
#endif
