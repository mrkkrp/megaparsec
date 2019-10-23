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
{-# OPTIONS_GHC -Wno-orphans            #-}

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
  , runParsecT
  , withParsecT )
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
import Text.Megaparsec.Internal.Monad()
import Text.Megaparsec.Internal.ParsecT
import Text.Megaparsec.State
import Text.Megaparsec.Stream
import qualified Data.List.NonEmpty  as NE
import qualified Data.Set            as E

-- | Whether the parser has failed or not. On success we include the
-- resulting value, on failure we include a 'ParseError'.
--
-- See also: 'Consumption', 'Reply'.

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
  mappend = (<>)
  {-# INLINE mappend #-}
  mconcat = fmap mconcat . sequence
  {-# INLINE mconcat #-}

-- | @since 6.3.0

instance (a ~ Tokens s, IsString a, Eq a, Stream s, Ord e)
    => IsString (ParsecT e s m a) where
  fromString s = tokens (==) (fromString s)

-- | 'empty' is a parser that __fails__ without consuming input.

instance (Ord e, Stream s) => Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus

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
pZero = ParsecT $ \s@(State _ o _) _ _ _ eerr ->
  eerr (TrivialError o Nothing E.empty) s
{-# INLINE pZero #-}

pPlus :: (Ord e, Stream s)
  => ParsecT e s m a
  -> ParsecT e s m a
  -> ParsecT e s m a
pPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs  = eok x s' (toHints (stateOffset s') err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

-- | From two states, return the one with the greater number of processed
-- tokens. If the numbers of processed tokens are equal, prefer the second
-- state.

longestMatch :: State s -> State s -> State s
longestMatch s1@(State _ o1 _) s2@(State _ o2 _) =
  case o1 `compare` o2 of
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
pFailure us ps = ParsecT $ \s@(State _ o _) _ _ _ eerr ->
  eerr (TrivialError o us ps) s
{-# INLINE pFailure #-}

pFancyFailure
  :: Set (ErrorFancy e)
  -> ParsecT e s m a
pFancyFailure xs = ParsecT $ \s@(State _ o _) _ _ _ eerr ->
  eerr (FancyError o xs) s
{-# INLINE pFancyFailure #-}

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT $ \s cok cerr eok eerr ->
  let el = Label <$> NE.nonEmpty l
      cok' x s' hs =
        case el of
          Nothing -> cok x s' (refreshLastHint hs Nothing)
          Just  _ -> cok x s' hs
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
pNotFollowedBy p = ParsecT $ \s@(State input o _) _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (take1_ input)
      unexpect u = TrivialError o (pure u) E.empty
      cok' _ _ _ = eerr (unexpect what) s
      cerr'  _ _ = eok () s mempty
      eok' _ _ _ = eerr (unexpect what) s
      eerr'  _ _ = eok () s mempty
  in unParser p s cok' cerr' eok' eerr'
{-# INLINE pNotFollowedBy #-}

pWithRecovery
  :: Stream s
  => (ParseError s e -> ParsecT e s m a)
  -> ParsecT e s m a
  -> ParsecT e s m a
pWithRecovery r p = ParsecT $ \s cok cerr eok eerr ->
  let mcerr err ms =
        let rcok x s' _ = cok x s' mempty
            rcerr   _ _ = cerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr   _ _ = cerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints (stateOffset s') err)
            rcerr   _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr   _ _ = eerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
  in unParser p s cok mcerr eok meerr
{-# INLINE pWithRecovery #-}

pObserving
  :: Stream s
  => ParsecT e s m a
  -> ParsecT e s m (Either (ParseError s e) a)
pObserving p = ParsecT $ \s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints (stateOffset s') err)
  in unParser p s (cok . Right) cerr' (eok . Right) eerr'
{-# INLINE pObserving #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input o pst) _ _ eok eerr ->
  case take1_ input of
    Nothing    -> eok () s mempty
    Just (x,_) ->
      let us = (pure . Tokens . nes) x
          ps = E.singleton EndOfInput
      in eerr (TrivialError o us ps)
          (State input o pst)
{-# INLINE pEof #-}

pToken :: forall e s m a. Stream s
  => (Token s -> Maybe a)
  -> Set (ErrorItem (Token s))
  -> ParsecT e s m a
pToken test ps = ParsecT $ \s@(State input o pst) cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
      in eerr (TrivialError o us ps) s
    Just (c,cs) ->
      case test c of
        Nothing ->
          let us = (Just . Tokens . nes) c
          in eerr (TrivialError o us ps)
                  (State input o pst)
        Just x ->
          cok x (State cs (o + 1) pst) mempty
{-# INLINE pToken #-}

pTokens :: forall e s m. Stream s
  => (Tokens s -> Tokens s -> Bool)
  -> Tokens s
  -> ParsecT e s m (Tokens s)
pTokens f tts = ParsecT $ \s@(State input o pst) cok _ eok eerr ->
  let pxy = Proxy :: Proxy s
      unexpect pos' u =
        let us = pure u
            ps = (E.singleton . Tokens . NE.fromList . chunkToTokens pxy) tts
        in TrivialError pos' us ps
      len = chunkLength pxy tts
  in case takeN_ len input of
    Nothing ->
      eerr (unexpect o EndOfInput) s
    Just (tts', input') ->
      if f tts tts'
        then let st = State input' (o + len) pst
             in if chunkEmpty pxy tts
                  then eok tts' st mempty
                  else cok tts' st mempty
        else let ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
             in eerr (unexpect o ps) (State input o pst)
{-# INLINE pTokens #-}

pTakeWhileP :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT $ \(State input o pst) cok _ eok _ ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  in if chunkEmpty pxy ts
       then eok ts (State input' (o + len) pst) hs
       else cok ts (State input' (o + len) pst) hs
{-# INLINE pTakeWhileP #-}

pTakeWhile1P :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT $ \(State input o pst) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      el = Label <$> (ml >>= NE.nonEmpty)
      hs =
        case el of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton) l
  in if chunkEmpty pxy ts
       then let us = pure $
                  case take1_ input of
                    Nothing -> EndOfInput
                    Just (t,_) -> Tokens (nes t)
                ps    = maybe E.empty E.singleton el
            in eerr (TrivialError o us ps)
                    (State input o pst)
       else cok ts (State input' (o + len) pst) hs
{-# INLINE pTakeWhile1P #-}

pTakeP :: forall e s m. Stream s
  => Maybe String
  -> Int
  -> ParsecT e s m (Tokens s)
pTakeP ml n = ParsecT $ \s@(State input o pst) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      el = Label <$> (ml >>= NE.nonEmpty)
      ps = maybe E.empty E.singleton el
  in case takeN_ n input of
       Nothing ->
         eerr (TrivialError o (pure EndOfInput) ps) s
       Just (ts, input') ->
         let len = chunkLength pxy ts
         in if len /= n
           then eerr (TrivialError (o + len) (pure EndOfInput) ps)
                     (State input o pst)
           else cok ts (State input' (o + len) pst) mempty
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
