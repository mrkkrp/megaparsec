-- |
-- Module      :  Text.Megaparsec.Internal
-- Copyright   :  © 2015–2018 Megaparsec contributors
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

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,11,0)
{-# OPTIONS -Wno-noncanonical-monoid-instances #-}
#endif

module Text.Megaparsec.Internal
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
  , runParsecT )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.String (IsString (..))
import Text.Megaparsec.Class
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import Text.Megaparsec.Stream
import qualified Control.Monad.Fail  as Fail
import qualified Data.List.NonEmpty  as NE
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

data Reply e s a = Reply (State s) Consumption (Result (Token s) e a)

-- | This data structure represents an aspect of result of parser's work.
--
-- See also: 'Result', 'Reply'.

data Consumption
  = Consumed -- ^ Some part of input stream was consumed
  | Virgin   -- ^ No input was consumed

-- | This data structure represents an aspect of result of parser's work.
--
-- See also: 'Consumption', 'Reply'.

data Result t e a
  = OK a                   -- ^ Parser succeeded
  | Error (ParseError t e) -- ^ Parser failed

-- | @'ParsecT' e s m a@ is a parser with custom data component of error
-- @e@, stream type @s@, underlying monad @m@ and return type @a@.

newtype ParsecT e s m a = ParsecT
  { unParser
      :: forall b. State s
      -> (a -> State s   -> Hints (Token s) -> m b) -- consumed-OK
      -> (ParseError (Token s) e -> State s -> m b) -- consumed-error
      -> (a -> State s   -> Hints (Token s) -> m b) -- empty-OK
      -> (ParseError (Token s) e -> State s -> m b) -- empty-error
      -> m b }

-- | @since 5.3.0

instance (Stream s, Semigroup a) => Semigroup (ParsecT e s m a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}
  sconcat = fmap sconcat . sequence
  {-# INLINE sconcat #-}

-- | @since 5.3.0

instance (Stream s, Monoid a) => Monoid (ParsecT e s m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
#if MIN_VERSION_base(4,11,0)
  mappend = (<>)
#else
  mappend = liftA2 mappend
#endif
  {-# INLINE mappend #-}
  mconcat = fmap mconcat . sequence
  {-# INLINE mconcat #-}

-- | @since 6.3.0

instance (a ~ Tokens s, IsString a, Eq a, Stream s, Ord e)
    => IsString (ParsecT e s m a) where
  fromString s = tokens (==) (fromString s)

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
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

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

-- | 'empty' is a parser that __fails__ without consuming input.

instance (Ord e, Stream s) => Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus

-- | 'return' returns a parser that __succeeds__ without consuming input.

instance Stream s => Monad (ParsecT e s m) where
  return = pure
  (>>=)  = pBind
  fail   = Fail.fail

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

instance Stream s => Fail.MonadFail (ParsecT e s m) where
  fail = pFail

pFail :: String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ pos _ _) _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
  in eerr (FancyError pos d) s
{-# INLINE pFail #-}

instance (Stream s, MonadIO m) => MonadIO (ParsecT e s m) where
  liftIO = lift . liftIO

instance (Stream s, MonadReader r m) => MonadReader r (ParsecT e s m) where
  ask       = lift ask
  local f p = mkPT $ \s -> local f (runParsecT p s)

instance (Stream s, MonadState st m) => MonadState st (ParsecT e s m) where
  get = lift get
  put = lift . put

instance (Stream s, MonadCont m) => MonadCont (ParsecT e s m) where
  callCC f = mkPT $ \s ->
    callCC $ \c ->
      runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s
    where pack s a = Reply s Virgin (OK a)

instance (Stream s, MonadError e' m) => MonadError e' (ParsecT e s m) where
  throwError = lift . throwError
  p `catchError` h = mkPT $ \s ->
    runParsecT p s `catchError` \e ->
      runParsecT (h e) s

mkPT :: Monad m => (State s -> m (Reply e s a)) -> ParsecT e s m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
  (Reply s' consumption result) <- k s
  case consumption of
    Consumed ->
      case result of
        OK    x -> cok x s' mempty
        Error e -> cerr e s'
    Virgin ->
      case result of
        OK    x -> eok x s' mempty
        Error e -> eerr e s'

-- | 'mzero' is a parser that __fails__ without consuming input.

instance (Ord e, Stream s) => MonadPlus (ParsecT e s m) where
  mzero = pZero
  mplus = pPlus

pZero :: ParsecT e s m a
pZero = ParsecT $ \s@(State _ pos _ _) _ _ _ eerr ->
  eerr (TrivialError pos Nothing E.empty) s
{-# INLINE pZero #-}

pPlus :: (Ord e, Stream s)
  => ParsecT e s m a
  -> ParsecT e s m a
  -> ParsecT e s m a
pPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs  = eok x s' (toHints (statePos s') err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

-- | From two states, return the one with the greater number of processed
-- tokens. If the numbers of processed tokens are equal, prefer the second
-- state.

longestMatch :: State s -> State s -> State s
longestMatch s1@(State _ _ tp1 _) s2@(State _ _ tp2 _) =
  case tp1 `compare` tp2 of
    LT -> s2
    EQ -> s2
    GT -> s1
{-# INLINE longestMatch #-}

-- | @since 6.0.0

instance (Stream s, MonadFix m) => MonadFix (ParsecT e s m) where
  mfix f = mkPT $ \s -> mfix $ \(~(Reply _ _ result)) -> do
    let
      a = case result of
        OK a' -> a'
        Error _ -> error "mfix ParsecT"
    runParsecT (f a) s

instance MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s _ _ eok _ ->
    amb >>= \a -> eok a s mempty

instance (Ord e, Stream s) => MonadParsec e s (ParsecT e s m) where
  failure           = pFailure
  fancyFailure      = pFancyFailure
  label             = pLabel
  try               = pTry
  lookAhead         = pLookAhead
  notFollowedBy     = pNotFollowedBy
  withRecovery      = pWithRecovery
  observing         = pObserving
  eof               = pEof
  token             = pToken
  tokens            = pTokens
  takeWhileP        = pTakeWhileP
  takeWhile1P       = pTakeWhile1P
  takeP             = pTakeP
  getParserState    = pGetParserState
  updateParserState = pUpdateParserState

pFailure
  :: Maybe (ErrorItem (Token s))
  -> Set (ErrorItem (Token s))
  -> ParsecT e s m a
pFailure us ps = ParsecT $ \s@(State _ pos _ _) _ _ _ eerr ->
  eerr (TrivialError pos us ps) s
{-# INLINE pFailure #-}

pFancyFailure
  :: Set (ErrorFancy e)
  -> ParsecT e s m a
pFancyFailure xs = ParsecT $ \s@(State _ pos _ _) _ _ _ eerr ->
  eerr (FancyError pos xs) s
{-# INLINE pFancyFailure #-}

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT $ \s cok cerr eok eerr ->
  let el = Label <$> NE.nonEmpty l
      cl = Label . (NE.fromList "the rest of " <>) <$> NE.nonEmpty l
      cok' x s' hs = cok x s' (refreshLastHint hs cl)
      eok' x s' hs = eok x s' (refreshLastHint hs el)
      eerr'    err = eerr $
        case err of
          (TrivialError pos us _) ->
            TrivialError pos us (maybe E.empty E.singleton el)
          _ -> err
  in unParser p s cok' cerr eok' eerr'
{-# INLINE pLabel #-}

pTry :: ParsecT e s m a -> ParsecT e s m a
pTry p = ParsecT $ \s cok _ eok eerr ->
  let eerr' err _ = eerr err s
  in unParser p s cok eerr' eok eerr'
{-# INLINE pTry #-}

pLookAhead :: ParsecT e s m a -> ParsecT e s m a
pLookAhead p = ParsecT $ \s _ cerr eok eerr ->
  let eok' a _ _ = eok a s mempty
  in unParser p s eok' cerr eok' eerr
{-# INLINE pLookAhead #-}

pNotFollowedBy :: Stream s => ParsecT e s m a -> ParsecT e s m ()
pNotFollowedBy p = ParsecT $ \s@(State input pos _ _) _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (take1_ input)
      unexpect u = TrivialError pos (pure u) E.empty
      cok' _ _ _ = eerr (unexpect what) s
      cerr'  _ _ = eok () s mempty
      eok' _ _ _ = eerr (unexpect what) s
      eerr'  _ _ = eok () s mempty
  in unParser p s cok' cerr' eok' eerr'
{-# INLINE pNotFollowedBy #-}

pWithRecovery
  :: (ParseError (Token s) e -> ParsecT e s m a)
  -> ParsecT e s m a
  -> ParsecT e s m a
pWithRecovery r p = ParsecT $ \s cok cerr eok eerr ->
  let mcerr err ms =
        let rcok x s' _ = cok x s' mempty
            rcerr   _ _ = cerr err ms
            reok x s' _ = eok x s' (toHints (statePos s') err)
            reerr   _ _ = cerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints (statePos s') err)
            rcerr   _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints (statePos s') err)
            reerr   _ _ = eerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
  in unParser p s cok mcerr eok meerr
{-# INLINE pWithRecovery #-}

pObserving
  :: ParsecT e s m a
  -> ParsecT e s m (Either (ParseError (Token s) e) a)
pObserving p = ParsecT $ \s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints (statePos s') err)
  in unParser p s (cok . Right) cerr' (eok . Right) eerr'
{-# INLINE pObserving #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input pos tp w) _ _ eok eerr ->
  case take1_ input of
    Nothing    -> eok () s mempty
    Just (x,_) ->
      let !apos = positionAt1 (Proxy :: Proxy s) pos x
          us    = (pure . Tokens . nes) x
          ps    = E.singleton EndOfInput
      in eerr (TrivialError apos us ps)
          (State input apos tp w)
{-# INLINE pEof #-}

pToken :: forall e s m a. Stream s
  => (Token s -> Maybe a)
  -> Set (ErrorItem (Token s))
  -> ParsecT e s m a
pToken test ps = ParsecT $ \s@(State input pos tp w) cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
      in eerr (TrivialError pos us ps) s
    Just (c,cs) ->
      case test c of
        Nothing ->
          let !apos = positionAt1 (Proxy :: Proxy s) pos c
              us    = (Just . Tokens . nes) c
          in eerr (TrivialError apos us ps)
                  (State input apos tp w)
        Just x ->
          let !npos = advance1 (Proxy :: Proxy s) w pos c
              newstate = State cs npos (tp + 1) w
          in cok x newstate mempty
{-# INLINE pToken #-}

pTokens :: forall e s m. Stream s
  => (Tokens s -> Tokens s -> Bool)
  -> Tokens s
  -> ParsecT e s m (Tokens s)
pTokens f tts = ParsecT $ \s@(State input pos tp w) cok _ eok eerr ->
  let pxy = Proxy :: Proxy s
      unexpect pos' u =
        let us = pure u
            ps = (E.singleton . Tokens . NE.fromList . chunkToTokens pxy) tts
        in TrivialError pos' us ps
      len = chunkLength pxy tts
  in case takeN_ len input of
    Nothing ->
      eerr (unexpect pos EndOfInput) s
    Just (tts', input') ->
      if f tts tts'
        then let !npos = advanceN pxy w pos tts'
                 st    = State input' npos (tp + len) w
             in if chunkEmpty pxy tts
                  then eok tts' st mempty
                  else cok tts' st mempty
        else let !apos = positionAtN pxy pos tts'
                 ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
             in eerr (unexpect apos ps) (State input apos tp w)
{-# INLINE pTokens #-}

pTakeWhileP :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT $ \(State input pos tp w) cok _ eok _ ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      !npos = advanceN pxy w pos ts
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  in if chunkEmpty pxy ts
       then eok ts (State input' npos (tp + len) w) hs
       else cok ts (State input' npos (tp + len) w) hs
{-# INLINE pTakeWhileP #-}

pTakeWhile1P :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT $ \(State input pos tp w) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      el = Label <$> (ml >>= NE.nonEmpty)
      hs =
        case el of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton) l
  in if chunkEmpty pxy ts
       then let !apos = positionAtN pxy pos ts
                us    = pure $
                  case take1_ input of
                    Nothing -> EndOfInput
                    Just (t,_) -> Tokens (nes t)
                ps    = maybe E.empty E.singleton el
            in eerr (TrivialError apos us ps)
                    (State input apos tp w)
       else let !npos = advanceN pxy w pos ts
            in cok ts (State input' npos (tp + len) w) hs
{-# INLINE pTakeWhile1P #-}

pTakeP :: forall e s m. Stream s
  => Maybe String
  -> Int
  -> ParsecT e s m (Tokens s)
pTakeP ml n = ParsecT $ \s@(State input pos tp w) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      el = Label <$> (ml >>= NE.nonEmpty)
      ps = maybe E.empty E.singleton el
  in case takeN_ n input of
       Nothing ->
         eerr (TrivialError pos (pure EndOfInput) ps) s
       Just (ts, input') ->
         let len   = chunkLength pxy ts
             !apos = positionAtN pxy pos ts
             !npos = advanceN pxy w pos ts
         in if len /= n
           then eerr (TrivialError npos (pure EndOfInput) ps)
                     (State input apos tp w)
           else cok ts (State input' npos (tp + len) w) mempty
{-# INLINE pTakeP #-}

pGetParserState :: ParsecT e s m (State s)
pGetParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s -> State s) -> ParsecT e s m ()
pUpdateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE pUpdateParserState #-}

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

----------------------------------------------------------------------------
-- Helper functions

-- | Convert 'ParseError' record into 'Hints'.

toHints
  :: SourcePos         -- ^ Current position in input stream
  -> ParseError t e    -- ^ Parse error to convert
  -> Hints t
toHints streamPos = \case
  TrivialError errPos _ ps ->
    -- NOTE This is important to check here that the error indeed has
    -- happened at the same position as current position of stream because
    -- there might have been backtracking with 'try' and in that case we
    -- must not convert such a parse error to hints.
    if streamPos == errPos
      then Hints (if E.null ps then [] else [ps])
      else mempty
  FancyError _ _ -> mempty
{-# INLINE toHints #-}

-- | @'withHints' hs c@ makes “error” continuation @c@ use given hints @hs@.
--
-- Note that if resulting continuation gets 'ParseError' that has custom
-- data in it, hints are ignored.

withHints :: Ord (Token s)
  => Hints (Token s)   -- ^ Hints to use
  -> (ParseError (Token s) e -> State s -> m b) -- ^ Continuation to influence
  -> ParseError (Token s) e -- ^ First argument of resulting continuation
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
-- 'ErrorItem' (or delete it if 'Nothing' is given). This is used in 'label'
-- primitive.

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
