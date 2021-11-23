{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Text.Megaparsec.Internal
  ( -- * Data types
    Hints (..),
    Reply (..),
    Consumption (..),
    Result (..),
    ParsecT (..),

    -- * Helper functions
    toHints,
    withHints,
    accHints,
    refreshLastHint,
    runParsecT,
    withParsecT,
  )
where

import Control.Applicative
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import qualified Control.Monad.Fail as Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as E
import Data.String (IsString (..))
import Text.Megaparsec.Class
import Text.Megaparsec.Error
import Text.Megaparsec.State
import Text.Megaparsec.Stream

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

instance Semigroup (Hints t) where
  Hints xs <> Hints ys = Hints $ xs <> ys

instance Monoid (Hints t) where
  mempty = Hints mempty

-- | All information available after parsing. This includes consumption of
-- input, success (with the returned value) or failure (with the parse
-- error), and parser state at the end of parsing.
--
-- See also: 'Consumption', 'Result'.
data Reply e s a = Reply (State s e) Consumption (Result s e a)

-- | Whether the input has been consumed or not.
--
-- See also: 'Result', 'Reply'.
data Consumption
  = -- | Some part of input stream was consumed
    Consumed
  | -- | No input was consumed
    Virgin

-- | Whether the parser has failed or not. On success we include the
-- resulting value, on failure we include a 'ParseError'.
--
-- See also: 'Consumption', 'Reply'.
data Result s e a
  = -- | Parser succeeded
    OK a
  | -- | Parser failed
    Error (ParseError s e)

-- | @'ParsecT' e s m a@ is a parser with custom data component of error
-- @e@, stream type @s@, underlying monad @m@ and return type @a@.
newtype ParsecT e s m a = ParsecT
  { unParser ::
      forall b.
      State s e ->
      (a -> State s e -> Hints (Token s) -> m b) -> -- consumed-OK
      (ParseError s e -> State s e -> m b) -> -- consumed-error
      (a -> State s e -> Hints (Token s) -> m b) -> -- empty-OK
      (ParseError s e -> State s e -> m b) -> -- empty-error
      m b
  }

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
instance
  (a ~ Tokens s, IsString a, Eq a, Stream s, Ord e) =>
  IsString (ParsecT e s m a)
  where
  fromString s = tokens (==) (fromString s)

instance Functor (ParsecT e s m) where
  fmap = pMap

pMap :: (a -> b) -> ParsecT e s m a -> ParsecT e s m b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

-- | 'pure' returns a parser that __succeeds__ without consuming input.
instance Stream s => Applicative (ParsecT e s m) where
  pure = pPure
  (<*>) = pAp
  p1 *> p2 = p1 `pBind` const p2
  p1 <* p2 = do x1 <- p1; void p2; return x1

pPure :: a -> ParsecT e s m a
pPure x = ParsecT $ \s _ _ eok _ -> eok x s mempty
{-# INLINE pPure #-}

pAp ::
  Stream s =>
  ParsecT e s m (a -> b) ->
  ParsecT e s m a ->
  ParsecT e s m b
pAp m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs =
        unParser
          k
          s'
          (cok . x)
          cerr
          (accHints hs (cok . x))
          (withHints hs cerr)
      meok x s' hs =
        unParser
          k
          s'
          (cok . x)
          cerr
          (accHints hs (eok . x))
          (withHints hs eerr)
   in unParser m s mcok cerr meok eerr
{-# INLINE pAp #-}

-- | 'empty' is a parser that __fails__ without consuming input.
instance (Ord e, Stream s) => Alternative (ParsecT e s m) where
  empty = mzero
  (<|>) = mplus

-- | 'return' returns a parser that __succeeds__ without consuming input.
instance Stream s => Monad (ParsecT e s m) where
  return = pure
  (>>=) = pBind

pBind ::
  Stream s =>
  ParsecT e s m a ->
  (a -> ParsecT e s m b) ->
  ParsecT e s m b
pBind m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs =
        unParser
          (k x)
          s'
          cok
          cerr
          (accHints hs cok)
          (withHints hs cerr)
      meok x s' hs =
        unParser
          (k x)
          s'
          cok
          cerr
          (accHints hs eok)
          (withHints hs eerr)
   in unParser m s mcok cerr meok eerr
{-# INLINE pBind #-}

instance Stream s => Fail.MonadFail (ParsecT e s m) where
  fail = pFail

pFail :: String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ o _ _) _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
   in eerr (FancyError o d) s
{-# INLINE pFail #-}

instance (Stream s, MonadIO m) => MonadIO (ParsecT e s m) where
  liftIO = lift . liftIO

instance (Stream s, MonadReader r m) => MonadReader r (ParsecT e s m) where
  ask = lift ask
  local f p = mkPT $ \s -> local f (runParsecT p s)

instance (Stream s, MonadState st m) => MonadState st (ParsecT e s m) where
  get = lift get
  put = lift . put

instance (Stream s, MonadCont m) => MonadCont (ParsecT e s m) where
  callCC f = mkPT $ \s ->
    callCC $ \c ->
      runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s
    where
      pack s a = Reply s Virgin (OK a)

instance (Stream s, MonadError e' m) => MonadError e' (ParsecT e s m) where
  throwError = lift . throwError
  p `catchError` h = mkPT $ \s ->
    runParsecT p s `catchError` \e ->
      runParsecT (h e) s

mkPT :: Monad m => (State s e -> m (Reply e s a)) -> ParsecT e s m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
  (Reply s' consumption result) <- k s
  case consumption of
    Consumed ->
      case result of
        OK x -> cok x s' mempty
        Error e -> cerr e s'
    Virgin ->
      case result of
        OK x -> eok x s' mempty
        Error e -> eerr e s'

-- | 'mzero' is a parser that __fails__ without consuming input.
--
-- __Note__: strictly speaking, this instance is unlawful. The right
-- identity law does not hold, e.g. in general this is not true:
--
-- > v >> mzero = mero
--
-- However the following holds:
--
-- > try v >> mzero = mzero
instance (Ord e, Stream s) => MonadPlus (ParsecT e s m) where
  mzero = pZero
  mplus = pPlus

pZero :: ParsecT e s m a
pZero = ParsecT $ \s@(State _ o _ _) _ _ _ eerr ->
  eerr (TrivialError o Nothing E.empty) s
{-# INLINE pZero #-}

pPlus ::
  (Ord e, Stream s) =>
  ParsecT e s m a ->
  ParsecT e s m a ->
  ParsecT e s m a
pPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs = eok x s' (toHints (stateOffset s') err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
         in unParser n s cok ncerr neok neerr
   in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

-- | From two states, return the one with the greater number of processed
-- tokens. If the numbers of processed tokens are equal, prefer the second
-- state.
longestMatch :: State s e -> State s e -> State s e
longestMatch s1@(State _ o1 _ _) s2@(State _ o2 _ _) =
  case o1 `compare` o2 of
    LT -> s2
    EQ -> s2
    GT -> s1
{-# INLINE longestMatch #-}

-- | @since 6.0.0
instance (Stream s, MonadFix m) => MonadFix (ParsecT e s m) where
  mfix f = mkPT $ \s -> mfix $ \(~(Reply _ _ result)) -> do
    let a = case result of
          OK a' -> a'
          Error _ -> error "mfix ParsecT"
    runParsecT (f a) s

instance Stream s => MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s _ _ eok _ ->
    amb >>= \a -> eok a s mempty

instance (Ord e, Stream s) => MonadParsec e s (ParsecT e s m) where
  parseError = pParseError
  label = pLabel
  try = pTry
  lookAhead = pLookAhead
  notFollowedBy = pNotFollowedBy
  withRecovery = pWithRecovery
  observing = pObserving
  eof = pEof
  token = pToken
  tokens = pTokens
  takeWhileP = pTakeWhileP
  takeWhile1P = pTakeWhile1P
  takeP = pTakeP
  getParserState = pGetParserState
  updateParserState = pUpdateParserState

pParseError ::
  ParseError s e ->
  ParsecT e s m a
pParseError e = ParsecT $ \s _ _ _ eerr -> eerr e s
{-# INLINE pParseError #-}

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT $ \s cok cerr eok eerr ->
  let el = Label <$> NE.nonEmpty l
      cok' x s' hs =
        case el of
          Nothing -> cok x s' (refreshLastHint hs Nothing)
          Just _ -> cok x s' hs
      eok' x s' hs = eok x s' (refreshLastHint hs el)
      eerr' err = eerr $
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
pNotFollowedBy p = ParsecT $ \s@(State input o _ _) _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (take1_ input)
      unexpect u = TrivialError o (pure u) E.empty
      cok' _ _ _ = eerr (unexpect what) s
      cerr' _ _ = eok () s mempty
      eok' _ _ _ = eerr (unexpect what) s
      eerr' _ _ = eok () s mempty
   in unParser p s cok' cerr' eok' eerr'
{-# INLINE pNotFollowedBy #-}

pWithRecovery ::
  Stream s =>
  (ParseError s e -> ParsecT e s m a) ->
  ParsecT e s m a ->
  ParsecT e s m a
pWithRecovery r p = ParsecT $ \s cok cerr eok eerr ->
  let mcerr err ms =
        let rcok x s' _ = cok x s' mempty
            rcerr _ _ = cerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr _ _ = cerr err ms
         in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints (stateOffset s') err)
            rcerr _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints (stateOffset s') err)
            reerr _ _ = eerr err ms
         in unParser (r err) ms rcok rcerr reok reerr
   in unParser p s cok mcerr eok meerr
{-# INLINE pWithRecovery #-}

pObserving ::
  Stream s =>
  ParsecT e s m a ->
  ParsecT e s m (Either (ParseError s e) a)
pObserving p = ParsecT $ \s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints (stateOffset s') err)
   in unParser p s (cok . Right) cerr' (eok . Right) eerr'
{-# INLINE pObserving #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input o pst de) _ _ eok eerr ->
  case take1_ input of
    Nothing -> eok () s mempty
    Just (x, _) ->
      let us = (pure . Tokens . nes) x
          ps = E.singleton EndOfInput
       in eerr
            (TrivialError o us ps)
            (State input o pst de)
{-# INLINE pEof #-}

pToken ::
  forall e s m a.
  Stream s =>
  (Token s -> Maybe a) ->
  Set (ErrorItem (Token s)) ->
  ParsecT e s m a
pToken test ps = ParsecT $ \s@(State input o pst de) cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
       in eerr (TrivialError o us ps) s
    Just (c, cs) ->
      case test c of
        Nothing ->
          let us = (Just . Tokens . nes) c
           in eerr
                (TrivialError o us ps)
                (State input o pst de)
        Just x ->
          cok x (State cs (o + 1) pst de) mempty
{-# INLINE pToken #-}

pTokens ::
  forall e s m.
  Stream s =>
  (Tokens s -> Tokens s -> Bool) ->
  Tokens s ->
  ParsecT e s m (Tokens s)
pTokens f tts = ParsecT $ \s@(State input o pst de) cok _ eok eerr ->
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
            then
              let st = State input' (o + len) pst de
               in if chunkEmpty pxy tts
                    then eok tts' st mempty
                    else cok tts' st mempty
            else
              let ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
               in eerr (unexpect o ps) (State input o pst de)
{-# INLINE pTokens #-}

pTakeWhileP ::
  forall e s m.
  Stream s =>
  Maybe String ->
  (Token s -> Bool) ->
  ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT $ \(State input o pst de) cok _ eok _ ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
   in if chunkEmpty pxy ts
        then eok ts (State input' (o + len) pst de) hs
        else cok ts (State input' (o + len) pst de) hs
{-# INLINE pTakeWhileP #-}

pTakeWhile1P ::
  forall e s m.
  Stream s =>
  Maybe String ->
  (Token s -> Bool) ->
  ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT $ \(State input o pst de) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      len = chunkLength pxy ts
      el = Label <$> (ml >>= NE.nonEmpty)
      hs =
        case el of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton) l
   in if chunkEmpty pxy ts
        then
          let us = pure $
                case take1_ input of
                  Nothing -> EndOfInput
                  Just (t, _) -> Tokens (nes t)
              ps = maybe E.empty E.singleton el
           in eerr
                (TrivialError o us ps)
                (State input o pst de)
        else cok ts (State input' (o + len) pst de) hs
{-# INLINE pTakeWhile1P #-}

pTakeP ::
  forall e s m.
  Stream s =>
  Maybe String ->
  Int ->
  ParsecT e s m (Tokens s)
pTakeP ml n = ParsecT $ \s@(State input o pst de) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      el = Label <$> (ml >>= NE.nonEmpty)
      ps = maybe E.empty E.singleton el
   in case takeN_ n input of
        Nothing ->
          eerr (TrivialError o (pure EndOfInput) ps) s
        Just (ts, input') ->
          let len = chunkLength pxy ts
           in if len /= n
                then
                  eerr
                    (TrivialError (o + len) (pure EndOfInput) ps)
                    (State input o pst de)
                else cok ts (State input' (o + len) pst de) mempty
{-# INLINE pTakeP #-}

pGetParserState :: ParsecT e s m (State s e)
pGetParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s e -> State s e) -> ParsecT e s m ()
pUpdateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE pUpdateParserState #-}

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

----------------------------------------------------------------------------
-- Helper functions

-- | Convert a 'ParseError' record into 'Hints'.
toHints ::
  Stream s =>
  -- | Current offset in input stream
  Int ->
  -- | Parse error to convert
  ParseError s e ->
  Hints (Token s)
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
-- __Note__ that if resulting continuation gets 'ParseError' that has custom
-- data in it, hints are ignored.
withHints ::
  Stream s =>
  -- | Hints to use
  Hints (Token s) ->
  -- | Continuation to influence
  (ParseError s e -> State s e -> m b) ->
  -- | First argument of resulting continuation
  ParseError s e ->
  -- | Second argument of resulting continuation
  State s e ->
  m b
withHints (Hints ps') c e =
  case e of
    TrivialError pos us ps -> c (TrivialError pos us (E.unions (ps : ps')))
    _ -> c e
{-# INLINE withHints #-}

-- | @'accHints' hs c@ results in “OK” continuation that will add given
-- hints @hs@ to third argument of original continuation @c@.
accHints ::
  -- | 'Hints' to add
  Hints t ->
  -- | An “OK” continuation to alter
  (a -> State s e -> Hints t -> m b) ->
  -- | Altered “OK” continuation
  (a -> State s e -> Hints t -> m b)
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)
{-# INLINE accHints #-}

-- | Replace the most recent group of hints (if any) with the given
-- 'ErrorItem' (or delete it if 'Nothing' is given). This is used in the
-- 'label' primitive.
refreshLastHint :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshLastHint (Hints []) _ = Hints []
refreshLastHint (Hints (_ : xs)) Nothing = Hints xs
refreshLastHint (Hints (_ : xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

-- | Low-level unpacking of the 'ParsecT' type.
runParsecT ::
  Monad m =>
  -- | Parser to run
  ParsecT e s m a ->
  -- | Initial state
  State s e ->
  m (Reply e s a)
runParsecT p s = unParser p s cok cerr eok eerr
  where
    cok a s' _ = return $ Reply s' Consumed (OK a)
    cerr err s' = return $ Reply s' Consumed (Error err)
    eok a s' _ = return $ Reply s' Virgin (OK a)
    eerr err s' = return $ Reply s' Virgin (Error err)

-- | Transform any custom errors thrown by the parser using the given
-- function. Similar in function and purpose to @withExceptT@.
--
-- __Note__ that the inner parser will start with an empty collection of
-- “delayed” 'ParseError's. Any delayed 'ParseError's produced in the inner
-- parser will be lifted by applying the provided function and added to the
-- collection of delayed parse errors of the outer parser.
--
-- @since 7.0.0
withParsecT ::
  forall e e' s m a.
  (Monad m, Ord e') =>
  (e -> e') ->
  -- | Inner parser
  ParsecT e s m a ->
  -- | Outer parser
  ParsecT e' s m a
withParsecT f p =
  ParsecT $ \s cok cerr eok eerr ->
    let s' =
          s
            { stateParseErrors = []
            }
        adjustState :: State s e -> State s e'
        adjustState st =
          st
            { stateParseErrors =
                (mapParseError f <$> stateParseErrors st)
                  ++ stateParseErrors s
            }
        cok' x st hs = cok x (adjustState st) hs
        cerr' e st = cerr (mapParseError f e) (adjustState st)
        eok' x st hs = eok x (adjustState st) hs
        eerr' e st = eerr (mapParseError f e) (adjustState st)
     in unParser p s' cok' cerr' eok' eerr'
{-# INLINE withParsecT #-}
