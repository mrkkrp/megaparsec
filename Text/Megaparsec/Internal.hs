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
{-# LANGUAGE BangPatterns               #-}

module Text.Megaparsec.Internal
  ( -- * Data types
    Hints (..)
  , Reply (..)
  , Consumption (..)
  , Result (..)
  , ParsecT (..)
    -- ** Memoization-related types
  , Memo (..)
  , Frame (..)
  , MemoIndex
    -- * Helper functions
  , toHints
  , withHints
  , accHints
  , refreshLastHint
  , runParsecT
  , withParsecT
    -- ** Memoization-related helpers
  , mrecord
  , mlookup
  , mcommit
  , mcommitted
  , mpush
  , mpop )
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
import Text.Megaparsec.State
import Text.Megaparsec.Stream
import qualified Control.Monad.Fail              as Fail
import qualified Data.IntMap.Strict              as IM
import qualified Data.List.NonEmpty              as NE
import qualified Data.Set                        as E
import qualified Text.Megaparsec.Internal.TagMap as TM

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

-- | The index used for the memo table, consisting of an offset and a parser tag.
type MemoIndex e s m a = (Int, TM.Tag (ParsecT e s m) a)

-- | A backtracking frame
data Frame = Frame {
  -- | Whether the current frame is committed
  frameCommitted :: !Bool,
  -- | The offset at which we entered this frame
  frameOffset :: !Int
  }

-- | The memoization state
data Memo e s m = Memo {
  -- | The memoization table
  table :: !(IM.IntMap (TM.TagMap (ParsecT e s m))),
  -- | The backtracking stack
  frames :: ![Frame]
  }

zmemo :: Memo e s m
zmemo = Memo IM.empty []

memomap :: forall e s m e' s' m'.
     (forall a. ParsecT e s m a -> ParsecT e' s' m' a)
  -> Memo e  s  m
  -> Memo e' s' m'
memomap f m = m { table = TM.tmap f <$> table m }

-- | @'ParsecT' e s m a@ is a parser with custom data component of error
-- @e@, stream type @s@, underlying monad @m@ and return type @a@.

newtype ParsecT e s m a = ParsecT
  { unParser
      :: forall b. State s
      -> Memo e s m
      -> (a -> State s   -> Hints (Token s) -> Memo e s m -> m b) -- consumed-OK
      -> (ParseError s e -> State s         -> Memo e s m -> m b) -- consumed-error
      -> (a -> State s   -> Hints (Token s) -> Memo e s m -> m b) -- empty-OK
      -> (ParseError s e -> State s         -> Memo e s m -> m b) -- empty-error
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
  mappend = (<>)
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
pMap f p = ParsecT $ \s mm cok cerr eok eerr ->
  unParser p s mm (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

-- | 'pure' returns a parser that __succeeds__ without consuming input.

instance Stream s => Applicative (ParsecT e s m) where
  pure     = pPure
  (<*>)    = pAp
  p1 *> p2 = p1 `pBind` const p2
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

pPure :: a -> ParsecT e s m a
pPure x = ParsecT $ \s mm  _ _ eok _ -> eok x s mempty mm
{-# INLINE pPure #-}

pAp :: Stream s
  => ParsecT e s m (a -> b)
  -> ParsecT e s m a
  -> ParsecT e s m b
pAp m k = ParsecT $ \s mm cok cerr eok eerr ->
  let mcok x s' hs mm' = unParser k s' mm' (cok . x) cerr
        (accHints hs (cok . x)) (withHints hs cerr)
      meok x s' hs mm' = unParser k s' mm' (cok . x) cerr
        (accHints hs (eok . x)) (withHints hs eerr)
  in unParser m s mm mcok cerr meok eerr
{-# INLINE pAp #-}

-- | 'empty' is a parser that __fails__ without consuming input.

instance (Ord e, Stream s) => Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus

-- | 'return' returns a parser that __succeeds__ without consuming input.

instance Stream s => Monad (ParsecT e s m) where
  return = pure
  (>>=)  = pBind
#if !(MIN_VERSION_base(4,13,0))
  fail   = Fail.fail
#endif

pBind :: Stream s
  => ParsecT e s m a
  -> (a -> ParsecT e s m b)
  -> ParsecT e s m b
pBind m k = ParsecT $ \s mm cok cerr eok eerr ->
  let mcok x s' hs mm' = unParser (k x) s' mm' cok cerr
        (accHints hs cok) (withHints hs cerr)
      mcerr err s' mm' = cerr err s' mm'
      meok x s' hs mm' = unParser (k x) s' mm' cok cerr
        (accHints hs eok) (withHints hs eerr)
      meerr err s' mm' = eerr err s' mm'
  in unParser m s mm mcok mcerr meok meerr
{-# INLINE pBind #-}

instance Stream s => Fail.MonadFail (ParsecT e s m) where
  fail = pFail

pFail :: String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ o _) mm _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
  in eerr (FancyError o d) s mm
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
mkPT k = ParsecT $ \s mm cok cerr eok eerr -> do
  (Reply s' consumption result) <- k s
  case consumption of
    Consumed ->
      case result of
        OK    x -> cok x s' mempty mm
        Error e -> cerr e s' mm
    Virgin ->
      case result of
        OK    x -> eok x s' mempty mm
        Error e -> eerr e s' mm

-- | 'mzero' is a parser that __fails__ without consuming input.

instance (Ord e, Stream s) => MonadPlus (ParsecT e s m) where
  mzero = pZero
  mplus = pPlus

pZero :: ParsecT e s m a
pZero = ParsecT $ \s@(State _ o _) mm _ _ _ eerr ->
  eerr (TrivialError o Nothing E.empty) s mm
{-# INLINE pZero #-}

pPlus :: (Ord e, Stream s)
  => ParsecT e s m a
  -> ParsecT e s m a
  -> ParsecT e s m a
pPlus m n = ParsecT $ \s m0 cok cerr eok eerr ->
  let meerr err ms mm =
        let ncerr err' s' mm' = cerr (err' <> err) (longestMatch ms s') mm'
            neok x s' hs  mm' = eok x s' (toHints (stateOffset s') err <> hs) mm'
            neerr err' s' mm' = eerr (err' <> err) (longestMatch ms s') mm'
        in unParser n s mm cok ncerr neok neerr
  in unParser m s m0 cok cerr eok meerr
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

instance MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s m  _ _ eok _ ->
    amb >>= \a -> eok a s mempty m

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
  commit            = pCommit
  memo              = pMemo
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
pFailure us ps = ParsecT $ \s@(State _ o _) mm _ _ _ eerr ->
  eerr (TrivialError o us ps) s mm
{-# INLINE pFailure #-}

pFancyFailure
  :: Set (ErrorFancy e)
  -> ParsecT e s m a
pFancyFailure xs = ParsecT $ \s@(State _ o _) mm _ _ _ eerr ->
  eerr (FancyError o xs) s mm
{-# INLINE pFancyFailure #-}

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT $ \s mm cok cerr eok eerr ->
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
  in unParser p s mm cok' cerr eok' eerr'
{-# INLINE pLabel #-}

-- | Look up a parser in the memo table.
mlookup :: Memo e s m -> MemoIndex e s m a -> Maybe (ParsecT e s m a)
mlookup m (o, t) = TM.lookup t <=< IM.lookup o $ table m

-- | Record a memoized parser in the memo table.
mrecord :: Memo e s m -> MemoIndex e s m a -> ParsecT e s m a -> Memo e s m
mrecord m (o, t) p = m { table = IM.alter go o $ table m }
  where
    go Nothing   = Just (TM.singleton t p)
    go (Just rm) = Just (TM.insert t p rm)

-- | Push a backtracking frame
mpush :: Int -> Memo e s m -> Memo e s m
mpush !o (Memo tab frs) = Memo tab (Frame False o : frs)

-- | Pop a backtracking frame
mpop :: Memo e s m -> Memo e s m
mpop (Memo _   []) = error "cant pop from empty frame"
mpop (Memo tab [Frame _ o]) = Memo (snd $ IM.split (o - 1) tab) []
mpop (Memo tab (_:frs)) = Memo tab frs

-- | Commit to a backtracking frame
mcommit :: Memo e s m -> Memo e s m
mcommit (Memo _   []) = error "illegal"
mcommit (Memo tab (fr:frs)) = Memo tab (fr { frameCommitted = True } : frs)

-- | Is the current backtracking frame committed?
mcommitted :: Memo e s m -> Bool
mcommitted (Memo _ []) = error "illegal"
mcommitted (Memo _ (fr:_)) = frameCommitted fr

pMemo :: forall e s m a. ParsecT e s m a -> ParsecT e s m a
pMemo !p = ParsecT $ \s@(State _ !o _) !mm cok cerr eok eerr ->
  let !midx = (o, TM.makeTag p)
      mcok x s' hs !m =
        let k :: ParsecT e s m a
            !k = ParsecT $ \_ km  kcok _ _ _ -> kcok x s' hs km
            !m' = mrecord m midx k
        in cok x s' hs m'
      mcerr err s' !m =
        let k :: ParsecT e s m a
            !k = ParsecT $ \_ km  _ kcerr _ _ -> kcerr err s' km
            !m' = mrecord m midx k
        in cerr err s' m'
      meok x s' hs !m =
        let k :: ParsecT e s m a
            !k = ParsecT $ \_ km  _ _ keok _ -> keok x s' hs km
            !m' = mrecord m midx k
        in eok x s' hs m'
      meerr err s' !m =
        let k :: ParsecT e s m a
            !k = ParsecT $ \_ km  _ _ _ keerr -> keerr err s' km
            !m' = mrecord m midx k
        in eerr err s' m'
  in case mlookup mm midx of
    Just p' -> unParser p' s mm  cok  cerr  eok  eerr
    Nothing -> unParser p  s mm mcok mcerr meok meerr
{-# NOINLINE pMemo #-}

pCommit :: ParsecT e s m ()
pCommit = ParsecT $ \s mm _ _ eok _ -> eok () s mempty (mcommit mm)
{-# INLINE pCommit #-}

pTry :: ParsecT e s m a -> ParsecT e s m a
pTry p = ParsecT $ \s@(State _ o _) mm cok cerr eok eerr ->
  let cok' a s' hs mm' = cok a s' hs (mpop mm')
      cerr' err s' mm' =
        if mcommitted mm'
          then cerr err s' (mpop mm')
          else eerr err s  (mpop mm')
      eok' a s' hs mm' = eok a s' hs (mpop mm')
      eerr' err _ mm' = eerr err s (mpop mm')
  in unParser p s (mpush o mm) cok' cerr' eok' eerr'
{-# INLINE pTry #-}

pLookAhead :: ParsecT e s m a -> ParsecT e s m a
pLookAhead p = ParsecT $ \s mm _ cerr eok eerr ->
  let eok' a _ _ mm' = eok a s mempty mm'
  in unParser p s mm eok' cerr eok' eerr
{-# INLINE pLookAhead #-}

pNotFollowedBy :: Stream s => ParsecT e s m a -> ParsecT e s m ()
pNotFollowedBy p = ParsecT $ \s@(State input o _) mm _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (take1_ input)
      unexpect u = TrivialError o (pure u) E.empty
      cok' _ _ _ mm' = eerr (unexpect what) s mm'
      cerr'  _ _ mm' = eok () s mempty mm'
      eok' _ _ _ mm' = eerr (unexpect what) s mm'
      eerr'  _ _ mm' = eok () s mempty mm'
  in unParser p s mm cok' cerr' eok' eerr'
{-# INLINE pNotFollowedBy #-}

pWithRecovery
  :: Stream s
  => (ParseError s e -> ParsecT e s m a)
  -> ParsecT e s m a
  -> ParsecT e s m a
pWithRecovery r p = ParsecT $ \s m  cok cerr eok eerr ->
  let mcerr err ms mm =
        let rcok x s' _ mm' = cok x s' mempty mm'
            rcerr   _ _ mm' = cerr err ms mm'
            reok x s' _ mm' = eok x s' (toHints (stateOffset s') err) mm'
            reerr   _ _ mm' = cerr err ms mm'
        in unParser (r err) ms mm rcok rcerr reok reerr
      meerr err ms mm =
        let rcok x s' _ mm' = cok x s' (toHints (stateOffset s') err) mm'
            rcerr   _ _ mm' = eerr err ms mm'
            reok x s' _ mm' = eok x s' (toHints (stateOffset s') err) mm'
            reerr   _ _ mm' = eerr err ms mm'
        in unParser (r err) ms mm rcok rcerr reok reerr
  in unParser p s m cok mcerr eok meerr
{-# INLINE pWithRecovery #-}

pObserving
  :: Stream s
  => ParsecT e s m a
  -> ParsecT e s m (Either (ParseError s e) a)
pObserving p = ParsecT $ \s mm cok _ eok _ ->
  let cok' x s' hs mm' = cok (Right x) s' hs mm'
      cerr' err s' mm' = cok (Left err) s' mempty mm'
      eok' x s' hs mm' = eok (Right x) s' hs mm'
      eerr' err s' mm' = eok (Left err) s' (toHints (stateOffset s') err) mm'
  in unParser p s mm cok' cerr' eok' eerr'
{-# INLINE pObserving #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input o pst) mm _ _ eok eerr ->
  case take1_ input of
    Nothing    -> eok () s mempty mm
    Just (x,_) ->
      let us = (pure . Tokens . nes) x
          ps = E.singleton EndOfInput
      in eerr (TrivialError o us ps)
          (State input o pst) mm
{-# INLINE pEof #-}

pToken :: forall e s m a. Stream s
  => (Token s -> Maybe a)
  -> Set (ErrorItem (Token s))
  -> ParsecT e s m a
pToken test ps = ParsecT $ \s@(State input o pst) mm cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
      in eerr (TrivialError o us ps) s mm
    Just (c,cs) ->
      case test c of
        Nothing ->
          let us = (Just . Tokens . nes) c
          in eerr (TrivialError o us ps) s mm
        Just x ->
          cok x (State cs (o + 1) pst) mempty mm
{-# INLINE pToken #-}

pTokens :: forall e s m. Stream s
  => (Tokens s -> Tokens s -> Bool)
  -> Tokens s
  -> ParsecT e s m (Tokens s)
pTokens f tts = ParsecT $ \s@(State input o pst) mm cok _ eok eerr ->
  let pxy = Proxy :: Proxy s
      unexpect pos' u =
        let us = pure u
            ps = (E.singleton . Tokens . NE.fromList . chunkToTokens pxy) tts
        in TrivialError pos' us ps
      len = chunkLength pxy tts
  in case takeN_ len input of
    Nothing ->
      eerr (unexpect o EndOfInput) s mm
    Just (tts', input') ->
      if f tts tts'
        then let st = State input' (o + len) pst
             in if chunkEmpty pxy tts
                  then eok tts' st mempty mm
                  else cok tts' st mempty mm
        else let ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
             in eerr (unexpect o ps) (State input o pst) mm
{-# INLINE pTokens #-}

pTakeWhileP :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT $ \(State input o pst) mm cok _ eok _ ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  in if chunkEmpty pxy ts
       then eok ts (State input' (o + len) pst) hs mm
       else cok ts (State input' (o + len) pst) hs mm
{-# INLINE pTakeWhileP #-}

pTakeWhile1P :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT $ \(State input o pst) mm cok _ _ eerr ->
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
                    (State input o pst) mm
       else cok ts (State input' (o + len) pst) hs mm
{-# INLINE pTakeWhile1P #-}

pTakeP :: forall e s m. Stream s
  => Maybe String
  -> Int
  -> ParsecT e s m (Tokens s)
pTakeP ml n = ParsecT $ \s@(State input o pst) mm cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      el = Label <$> (ml >>= NE.nonEmpty)
      ps = maybe E.empty E.singleton el
  in case takeN_ n input of
       Nothing ->
         eerr (TrivialError o (pure EndOfInput) ps) s mm
       Just (ts, input') ->
         let len = chunkLength pxy ts
         in if len /= n
           then eerr (TrivialError (o + len) (pure EndOfInput) ps)
                     (State input o pst) mm
           else cok ts (State input' (o + len) pst) mempty mm
{-# INLINE pTakeP #-}

pGetParserState :: ParsecT e s m (State s)
pGetParserState = ParsecT $ \s mm _ _ eok _ -> eok s s mempty mm
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s -> State s) -> ParsecT e s m ()
pUpdateParserState f = ParsecT $ \s mm _ _ eok _ -> eok () (f s) mempty mm
{-# INLINE pUpdateParserState #-}

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

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
runParsecT p s = unParser p s zmemo cok cerr eok eerr
  where
    cok a s' _  _ = return $ Reply s' Consumed (OK a)
    cerr err s' _ = return $ Reply s' Consumed (Error err)
    eok a s' _  _ = return $ Reply s' Virgin   (OK a)
    eerr err s' _ = return $ Reply s' Virgin   (Error err)

-- | Transform any custom errors thrown by the parser using the given
-- function. Similar in function and purpose to @withExceptT@.
--
-- @since 7.0.0

withParsecT :: forall e e' s m a. (Monad m, Ord e')
  => (e -> e')
  -> ParsecT e s m a
  -> ParsecT e' s m a
withParsecT f = go
  where
    go :: forall v. ParsecT e s m v -> ParsecT e' s m v
    go p = ParsecT $ \s _ cok cerr eok eerr ->
      let mapmemo :: Memo e s m -> Memo e' s m
          mapmemo = memomap go
          cok' a s' hs mm' = cok a s' hs (mapmemo mm')
          cerr' err s' mm' = cerr (mapParseError f err) s' (mapmemo mm')
          eok' a s' hs mm' = eok a s' hs (mapmemo mm')
          eerr' err s' mm' = eerr (mapParseError f err) s' (mapmemo mm')
      in unParser p s zmemo cok' cerr' eok' eerr'
{-# INLINE withParsecT #-}
