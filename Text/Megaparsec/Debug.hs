{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Module      :  Text.Megaparsec.Debug
-- Copyright   :  © 2015–present Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Debugging helpers.
--
-- @since 7.0.0
module Text.Megaparsec.Debug
  ( MonadParsecDbg (..),
    dbg',
  )
where

import Control.Monad.Identity (IdentityT, mapIdentityT)
import qualified Control.Monad.Trans.RWS.Lazy as L
import qualified Control.Monad.Trans.RWS.Strict as S
import qualified Control.Monad.Trans.Reader as L
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S
import Data.Bifunctor (Bifunctor (first))
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Proxy
import qualified Data.Set as E
import Debug.Trace
import Text.Megaparsec.Class (MonadParsec)
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- | Type class describing parser monads that can trace during evaluation.
--
-- @since 9.3.0
class (MonadParsec e s m) => MonadParsecDbg e s m where
  -- | @'dbg' label p@ parser works exactly like @p@, but when it's evaluated
  -- it prints information useful for debugging. The @label@ is only used to
  -- refer to this parser in the debugging output. This combinator uses the
  -- 'trace' function from "Debug.Trace" under the hood.
  --
  -- Typical usage is to wrap every sub-parser in misbehaving parser with
  -- 'dbg' assigning meaningful labels. Then give it a shot and go through the
  -- print-out. As of current version, this combinator prints all available
  -- information except for /hints/, which are probably only interesting to
  -- the maintainer of Megaparsec itself and may be quite verbose to output in
  -- general. Let me know if you would like to be able to see hints in the
  -- debugging output.
  --
  -- The output itself is pretty self-explanatory, although the following
  -- abbreviations should be clarified (they are derived from the low-level
  -- source code):
  --
  --     * @COK@—“consumed OK”. The parser consumed input and succeeded.
  --     * @CERR@—“consumed error”. The parser consumed input and failed.
  --     * @EOK@—“empty OK”. The parser succeeded without consuming input.
  --     * @EERR@—“empty error”. The parser failed without consuming input.
  --
  -- __Note__: up until the version /9.3.0/ this was a non-polymorphic
  -- function that worked only in 'ParsecT'. It was first introduced in the
  -- version /7.0.0/.
  dbg ::
    (Show a) =>
    -- | Debugging label
    String ->
    -- | Parser to debug
    m a ->
    -- | Parser that prints debugging messages
    m a

-- | @dbg (p :: StateT st m)@ prints state __after__ running @p@:
--
-- >>> p = modify succ >> dbg "a" (single 'a' >> modify succ)
-- >>> parseTest (runStateT p 0) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (STATE: 2)
-- ((),2)
instance
  (Show st, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (L.StateT st m)
  where
  dbg str sma = L.StateT $ \s ->
    dbgWithComment "STATE" str $ L.runStateT sma s

-- | @dbg (p :: StateT st m)@ prints state __after__ running @p@:
--
-- >>> p = modify succ >> dbg "a" (single 'a' >> modify succ)
-- >>> parseTest (runStateT p 0) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (STATE: 2)
-- ((),2)
instance
  (Show st, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (S.StateT st m)
  where
  dbg str sma = S.StateT $ \s ->
    dbgWithComment "STATE" str $ S.runStateT sma s

instance
  (MonadParsecDbg e s m) =>
  MonadParsecDbg e s (L.ReaderT r m)
  where
  dbg = L.mapReaderT . dbg

-- | @dbg (p :: WriterT st m)@ prints __only__ log produced by @p@:
--
-- >>> p = tell [0] >> dbg "a" (single 'a' >> tell [1])
-- >>> parseTest (runWriterT p) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (LOG: [1])
-- ((),[0,1])
instance
  (Monoid w, Show w, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (L.WriterT w m)
  where
  dbg str wma = L.WriterT $ dbgWithComment "LOG" str $ L.runWriterT wma

-- | @dbg (p :: WriterT st m)@ prints __only__ log produced by @p@:
--
-- >>> p = tell [0] >> dbg "a" (single 'a' >> tell [1])
-- >>> parseTest (runWriterT p) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (LOG: [1])
-- ((),[0,1])
instance
  (Monoid w, Show w, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (S.WriterT w m)
  where
  dbg str wma = S.WriterT $ dbgWithComment "LOG" str $ S.runWriterT wma

-- | @RWST@ works like @StateT@ inside a @WriterT@: subparser's log and its
-- final state is printed:
--
-- >>> p = tell [0] >> modify succ >> dbg "a" (single 'a' >> tell [1] >> modify succ)
-- >>> parseTest (runRWST p () 0) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (STATE: 2) (LOG: [1])
-- ((),2,[0,1])
instance
  (Monoid w, Show w, Show st, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (L.RWST r w st m)
  where
  dbg str sma = L.RWST $ \r s -> do
    let smth =
          (\(a, st, w) -> ShowComment "LOG" (ShowComment "STATE" (a, st), w))
            <$> L.runRWST sma r s
    ((a, st), w) <- first unComment . unComment <$> dbg str smth
    pure (a, st, w)

-- | @RWST@ works like @StateT@ inside a @WriterT@: subparser's log and its
-- final state is printed:
--
-- >>> p = tell [0] >> modify succ >> dbg "a" (single 'a' >> tell [1] >> modify succ)
-- >>> parseTest (runRWST p () 0) "a"
-- a> IN: 'a'
-- a> MATCH (COK): 'a'
-- a> VALUE: () (STATE: 2) (LOG: [1])
-- ((),2,[0,1])
instance
  (Monoid w, Show w, Show st, MonadParsecDbg e s m) =>
  MonadParsecDbg e s (S.RWST r w st m)
  where
  dbg str sma = S.RWST $ \r s -> do
    let smth =
          (\(a, st, w) -> ShowComment "LOG" (ShowComment "STATE" (a, st), w))
            <$> S.runRWST sma r s
    ((a, st), w) <- first unComment . unComment <$> dbg str smth
    pure (a, st, w)

instance (MonadParsecDbg e s m) => MonadParsecDbg e s (IdentityT m) where
  dbg = mapIdentityT . dbg

-- | @'dbgWithComment' label_a label_c m@ traces the first component of the
-- result produced by @m@ with @label_a@ and the second component with
-- @label_b@.
dbgWithComment ::
  (MonadParsecDbg e s m, Show a, Show c) =>
  -- | Debugging label (for @a@)
  String ->
  -- | Extra component label (for @c@)
  String ->
  -- | Parser to debug
  m (a, c) ->
  -- | Parser that prints debugging messages
  m (a, c)
dbgWithComment lbl str ma =
  unComment <$> dbg str (ShowComment lbl <$> ma)

-- | A wrapper with a special show instance:
--
-- >>> show (ShowComment "STATE" ("Hello, world!", 42))
-- Hello, world! (STATE: 42)
data ShowComment c a = ShowComment String (a, c)

unComment :: ShowComment c a -> (a, c)
unComment (ShowComment _ val) = val

instance (Show c, Show a) => Show (ShowComment c a) where
  show (ShowComment lbl (a, c)) = show a ++ " (" ++ lbl ++ ": " ++ show c ++ ")"

instance
  (VisualStream s, ShowErrorComponent e) =>
  MonadParsecDbg e s (ParsecT e s m)
  where
  dbg lbl p = ParsecT $ \s cok cerr eok eerr ->
    let l = dbgLog lbl
        unfold = streamTake 40
        cok' x s' hs =
          flip trace (cok x s' hs) $
            l (DbgIn (unfold (stateInput s)))
              ++ l (DbgCOK (streamTake (streamDelta s s') (stateInput s)) x hs)
        cerr' err s' =
          flip trace (cerr err s') $
            l (DbgIn (unfold (stateInput s)))
              ++ l (DbgCERR (streamTake (streamDelta s s') (stateInput s)) err)
        eok' x s' hs =
          flip trace (eok x s' hs) $
            l (DbgIn (unfold (stateInput s)))
              ++ l (DbgEOK (streamTake (streamDelta s s') (stateInput s)) x hs)
        eerr' err s' =
          flip trace (eerr err s') $
            l (DbgIn (unfold (stateInput s)))
              ++ l (DbgEERR (streamTake (streamDelta s s') (stateInput s)) err)
     in unParser p s cok' cerr' eok' eerr'

-- | A single piece of info to be rendered with 'dbgLog'.
data DbgItem s e a
  = DbgIn [Token s]
  | DbgCOK [Token s] a (Hints (Token s))
  | DbgCERR [Token s] (ParseError s e)
  | DbgEOK [Token s] a (Hints (Token s))
  | DbgEERR [Token s] (ParseError s e)

-- | Render a single piece of debugging info.
dbgLog ::
  forall s e a.
  (VisualStream s, ShowErrorComponent e, Show a) =>
  -- | Debugging label
  String ->
  -- | Information to render
  DbgItem s e a ->
  -- | Rendered result
  String
dbgLog lbl item = prefix msg
  where
    prefix = unlines . fmap ((lbl ++ "> ") ++) . lines
    pxy = Proxy :: Proxy s
    showHints hs = "[" ++ List.intercalate "," (showErrorItem pxy <$> E.toAscList hs) ++ "]"
    msg = case item of
      DbgIn ts ->
        "IN: " ++ showStream pxy ts
      DbgCOK ts a (Hints hs) ->
        "MATCH (COK): "
          ++ showStream pxy ts
          ++ "\nVALUE: "
          ++ show a
          ++ "\nHINTS: "
          ++ showHints hs
      DbgCERR ts e ->
        "MATCH (CERR): " ++ showStream pxy ts ++ "\nERROR:\n" ++ parseErrorPretty e
      DbgEOK ts a (Hints hs) ->
        "MATCH (EOK): "
          ++ showStream pxy ts
          ++ "\nVALUE: "
          ++ show a
          ++ "\nHINTS: "
          ++ showHints hs
      DbgEERR ts e ->
        "MATCH (EERR): " ++ showStream pxy ts ++ "\nERROR:\n" ++ parseErrorPretty e

-- | Pretty-print a list of tokens.
showStream :: (VisualStream s) => Proxy s -> [Token s] -> String
showStream pxy ts =
  case NE.nonEmpty ts of
    Nothing -> "<EMPTY>"
    Just ne ->
      let (h, r) = splitAt 40 (showTokens pxy ne)
       in if null r then h else h ++ " <…>"

-- | Calculate number of consumed tokens given 'State' of parser before and
-- after parsing.
streamDelta ::
  -- | State of parser before consumption
  State s e ->
  -- | State of parser after consumption
  State s e ->
  -- | Number of consumed tokens
  Int
streamDelta s0 s1 = stateOffset s1 - stateOffset s0

-- | Extract a given number of tokens from the stream.
streamTake :: forall s. (Stream s) => Int -> s -> [Token s]
streamTake n s =
  case fst <$> takeN_ n s of
    Nothing -> []
    Just chk -> chunkToTokens (Proxy :: Proxy s) chk

-- | Just like 'dbg', but doesn't require the return value of the parser to
-- be 'Show'-able.
--
-- @since 9.1.0
dbg' ::
  (MonadParsecDbg e s m) =>
  -- | Debugging label
  String ->
  -- | Parser to debug
  m a ->
  -- | Parser that prints debugging messages
  m a
dbg' lbl p = unBlind <$> dbg lbl (Blind <$> p)

-- | A wrapper type with a dummy 'Show' instance.
newtype Blind x = Blind {unBlind :: x}

instance Show (Blind x) where
  show _ = "NOT SHOWN"
