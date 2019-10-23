-- |
-- Module      :  Text.Megaparsec.Internal
-- Copyright   :  © 2015–present Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Internal definitions. Versioning rules do not apply here. Please do not
-- rely on these unless you really know what you're doing.
--
-- @since 6.5.0

{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Text.Megaparsec.Internal.ParsecT
  ( -- * Data types
    Hints (..)
  , Reply (..)
  , Consumption (..)
  , Result (..)
  , ParsecT (..)
    -- * Helper functions
  , toHints
  , withHints
  , accHints
  , refreshLastHint
  , runParsecT
  , withParsecT
  , pBind
  , pFail )
where

import Control.Monad
import Control.Monad.Trans
import Data.Set (Set)
import Text.Megaparsec.Error
import Text.Megaparsec.State
import Text.Megaparsec.Stream
import qualified Data.Set            as E

----------------------------------------------------------------------------
-- Data types

-- | 'Hints' represent a collection of 'ErrorItem's to be included into
-- 'ParseError' (when it's a 'TrivialError') as “expected” message items
-- when a parser fails without consuming input right after successful parser
-- that produced the hints.
--
-- For example, without hints you could get:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- 1:2:
-- unexpected 'a'
-- expecting end of input
--
-- We're getting better error messages with the help of hints:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- 1:2:
-- unexpected 'a'
-- expecting 'r' or end of input

newtype Hints t = Hints [Set (ErrorItem t)]
  deriving (Semigroup, Monoid)

-- | All information available after parsing. This includes consumption of
-- input, success (with returned value) or failure (with parse error), and
-- parser state at the end of parsing.
--
-- See also: 'Consumption', 'Result'.

data Reply e s a = Reply (State s) Consumption (Result s e a)

-- | Whether the input has been consumed or not.
--
-- See also: 'Result', 'Reply'.

data Consumption
  = Consumed -- ^ Some part of input stream was consumed
  | Virgin   -- ^ No input was consumed

-- | Whether the parser has failed or not. On success we include the
-- resulting value, on failure we include a 'ParseError'.
--
-- See also: 'Consumption', 'Reply'.

data Result s e a
  = OK a                   -- ^ Parser succeeded
  | Error (ParseError s e) -- ^ Parser failed

-- | @'ParsecT' e s m a@ is a parser with custom data component of error
-- @e@, stream type @s@, underlying monad @m@ and return type @a@.

newtype ParsecT e s m a = ParsecT
  { unParser
      :: forall b. State s
      -> (a -> State s   -> Hints (Token s) -> m b) -- consumed-OK
      -> (ParseError s e -> State s         -> m b) -- consumed-error
      -> (a -> State s   -> Hints (Token s) -> m b) -- empty-OK
      -> (ParseError s e -> State s         -> m b) -- empty-error
      -> m b }

instance Functor (ParsecT e s m) where
  fmap = pMap

pMap :: (a -> b) -> ParsecT e s m a -> ParsecT e s m b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

-- | 'pure' returns a parser that __succeeds__ without consuming input.

instance Stream s => Applicative (ParsecT e s m) where
  pure     = pPure
  (<*>)    = pAp
  p1 *> p2 = p1 `pBind` const p2
  p1 <* p2 = p1 `pBind` \x1 -> void p2 `pBind` \_ -> pure x1

pPure :: a -> ParsecT e s m a
pPure x = ParsecT $ \s _ _ eok _ -> eok x s mempty
{-# INLINE pPure #-}

pAp :: Stream s
  => ParsecT e s m (a -> b)
  -> ParsecT e s m a
  -> ParsecT e s m b
pAp m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser k s' (cok . x) cerr
        (accHints hs (cok . x)) (withHints hs cerr)
      meok x s' hs = unParser k s' (cok . x) cerr
        (accHints hs (eok . x)) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr
{-# INLINE pAp #-}

pBind :: Stream s
  => ParsecT e s m a
  -> (a -> ParsecT e s m b)
  -> ParsecT e s m b
pBind m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser (k x) s' cok cerr
        (accHints hs cok) (withHints hs cerr)
      meok x s' hs = unParser (k x) s' cok cerr
        (accHints hs eok) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr
{-# INLINE pBind #-}

pFail :: String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ o _) _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
  in eerr (FancyError o d) s
{-# INLINE pFail #-}

instance MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s _ _ eok _ ->
    amb >>= \a -> eok a s mempty

----------------------------------------------------------------------------
-- Helper functions

-- | Convert 'ParseError' record to 'Hints'.

toHints
  :: Stream s
  => Int               -- ^ Current offset in input stream
  -> ParseError s e    -- ^ Parse error to convert
  -> Hints (Token s)
toHints streamPos = \case
  TrivialError errOffset _ ps ->
    -- NOTE This is important to check here that the error indeed has
    -- happened at the same position as current position of stream because
    -- there might have been backtracking with 'try' and in that case we
    -- must not convert such a parse error to hints.
    if streamPos == errOffset
      then Hints (if E.null ps then [] else [ps])
      else mempty
  FancyError _ _ -> mempty
{-# INLINE toHints #-}

-- | @'withHints' hs c@ makes “error” continuation @c@ use given hints @hs@.
--
-- Note that if resulting continuation gets 'ParseError' that has custom
-- data in it, hints are ignored.

withHints
  :: Stream s
  => Hints (Token s)   -- ^ Hints to use
  -> (ParseError s e -> State s -> m b) -- ^ Continuation to influence
  -> ParseError s e    -- ^ First argument of resulting continuation
  -> State s           -- ^ Second argument of resulting continuation
  -> m b
withHints (Hints ps') c e =
  case e of
    TrivialError pos us ps -> c (TrivialError pos us (E.unions (ps : ps')))
    _ -> c e
{-# INLINE withHints #-}

-- | @'accHints' hs c@ results in “OK” continuation that will add given
-- hints @hs@ to third argument of original continuation @c@.

accHints
  :: Hints t           -- ^ 'Hints' to add
  -> (a -> State s -> Hints t -> m b) -- ^ An “OK” continuation to alter
  -> (a -> State s -> Hints t -> m b) -- ^ Altered “OK” continuation
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)
{-# INLINE accHints #-}

-- | Replace the most recent group of hints (if any) with the given
-- 'ErrorItem' (or delete it if 'Nothing' is given). This is used in the
-- 'label' primitive.

refreshLastHint :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshLastHint (Hints [])     _        = Hints []
refreshLastHint (Hints (_:xs)) Nothing  = Hints xs
refreshLastHint (Hints (_:xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

-- | Low-level unpacking of the 'ParsecT' type.

runParsecT :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> State s       -- ^ Initial state
  -> m (Reply e s a)
runParsecT p s = unParser p s cok cerr eok eerr
  where
    cok a s' _  = return $ Reply s' Consumed (OK a)
    cerr err s' = return $ Reply s' Consumed (Error err)
    eok a s' _  = return $ Reply s' Virgin   (OK a)
    eerr err s' = return $ Reply s' Virgin   (Error err)

-- | Transform any custom errors thrown by the parser using the given
-- function. Similar in function and purpose to @withExceptT@.
--
-- @since 7.0.0

withParsecT :: (Monad m, Ord e')
  => (e -> e')
  -> ParsecT e s m a
  -> ParsecT e' s m a
withParsecT f p =
  ParsecT $ \s cok cerr eok eerr ->
    unParser p s cok (cerr . mapParseError f) eok (eerr . mapParseError f)
{-# INLINE withParsecT #-}
