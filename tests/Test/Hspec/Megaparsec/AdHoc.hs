{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Test.Hspec.Megaparsec.AdHoc
  ( -- * Helpers to run parsers
    prs
  , prs'
  , prs_
  , grs
  , grs'
    -- * Working with source position
  , updatePosString
  , pos1
  , nes
    -- * Other
  , abcRow
  , toFirstMismatch
  , Parser )
where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Identity
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import qualified Control.Monad.RWS.Lazy      as L
import qualified Control.Monad.RWS.Strict    as S
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Helpers to run parsers

-- | Apply parser to given input. This is a specialized version of 'parse'
-- that assumes empty file name.

prs
  :: Parsec Dec String a -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> Either (ParseError Char Dec) a -- ^ Result of parsing
prs p = parse p ""
{-# INLINE prs #-}

-- | Just like 'prs', but allows to inspect final state of the parser.

prs'
  :: Parsec Dec String a -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> (State String, Either (ParseError Char Dec) a) -- ^ Result of parsing
prs' p s = runParser' p (initialState s)
{-# INLINE prs' #-}

-- | Just like 'prs', but forces the parser to consume all input by adding
-- 'eof':
--
-- > prs_ p = parse (p <* eof) ""

prs_
  :: Parsec Dec String a -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> Either (ParseError Char Dec) a -- ^ Result of parsing
prs_ p = parse (p <* eof) ""
{-# INLINE prs_ #-}

-- | Just like 'prs', but interprets given parser as various monads (tries
-- all supported monads transformers in turn).

grs
  :: (forall m. MonadParsec Dec String m => m a) -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> (Either (ParseError Char Dec) a -> Expectation)
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

-- | 'grs'' to 'grs' as 'prs'' to 'prs'.

grs'
  :: (forall m. MonadParsec Dec String m => m a) -- ^ Parser to run
  -> String            -- ^ Input for the parser
  -> ((State String, Either (ParseError Char Dec) a) -> Expectation)
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
evalWriterTL = liftM fst . L.runWriterT
evalWriterTS :: Monad m => S.WriterT [Int] m a -> m a
evalWriterTS = liftM fst . S.runWriterT

evalRWSTL :: Monad m => L.RWST () [Int] () m a -> m a
evalRWSTL m = do
  (a,_,_) <- L.runRWST m () ()
  return a

evalRWSTS :: Monad m => S.RWST () [Int] () m a -> m a
evalRWSTS m = do
  (a,_,_) <- S.runRWST m () ()
  return a

----------------------------------------------------------------------------
-- Working with source position

-- | A helper function that is used to advance 'SourcePos' given a 'String'.

updatePosString
  :: Pos               -- ^ Tab width
  -> SourcePos         -- ^ Initial position
  -> String            -- ^ 'String' — collection of tokens to process
  -> SourcePos         -- ^ Final position
updatePosString w = foldl' f
  where f p t = snd (defaultUpdatePos w p t)

-- | Position with minimal value.

pos1 :: Pos
pos1 = unsafePos 1

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

----------------------------------------------------------------------------
-- Other

-- | @abcRow a b c@ generates string consisting of character “a” repeated
-- @a@ times, character “b” repeated @b@ times, and character “c” repeated
-- @c@ times.

abcRow :: Int -> Int -> Int -> String
abcRow a b c = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

-- | Given a comparing function, get prefix of one string till first
-- mismatch with another string (including first mismatching character).

toFirstMismatch
  :: (Char -> Char -> Bool) -- ^ Comparing function
  -> String            -- ^ First string
  -> String            -- ^ Second string
  -> String            -- ^ Resulting prefix
toFirstMismatch f str s = take (n + 1) s
  where n = length (takeWhile (uncurry f) (zip str s))

-- | The type of parser that consumes a 'String'.

type Parser = Parsec Dec String
