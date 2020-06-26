{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  ( dbg,
  )
where

import qualified Data.List.NonEmpty as NE
import Data.Proxy
import Debug.Trace
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.State
import Text.Megaparsec.Stream

-- | @'dbg' label p@ parser works exactly like @p@, but when it's evaluated
-- it also prints information useful for debugging. The @label@ is only used
-- to refer to this parser in the debugging output. This combinator uses the
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
-- Finally, it's not possible to lift this function into some monad
-- transformers without introducing surprising behavior (e.g. unexpected
-- state backtracking) or adding otherwise redundant constraints (e.g.
-- 'Show' instance for state), so this helper is only available for
-- 'ParsecT' monad, not any instance of 'Text.Megaparsec.MonadParsec' in
-- general.
dbg ::
  forall e s m a.
  ( VisualStream s,
    ShowErrorComponent e,
    Show a
  ) =>
  -- | Debugging label
  String ->
  -- | Parser to debug
  ParsecT e s m a ->
  -- | Parser that prints debugging messages
  ParsecT e s m a
dbg lbl p = ParsecT $ \s cok cerr eok eerr ->
  let l = dbgLog lbl :: DbgItem s e a -> String
      unfold = streamTake 40
      cok' x s' hs =
        flip trace (cok x s' hs) $
          l (DbgIn (unfold (stateInput s)))
            ++ l (DbgCOK (streamTake (streamDelta s s') (stateInput s)) x)
      cerr' err s' =
        flip trace (cerr err s') $
          l (DbgIn (unfold (stateInput s)))
            ++ l (DbgCERR (streamTake (streamDelta s s') (stateInput s)) err)
      eok' x s' hs =
        flip trace (eok x s' hs) $
          l (DbgIn (unfold (stateInput s)))
            ++ l (DbgEOK (streamTake (streamDelta s s') (stateInput s)) x)
      eerr' err s' =
        flip trace (eerr err s') $
          l (DbgIn (unfold (stateInput s)))
            ++ l (DbgEERR (streamTake (streamDelta s s') (stateInput s)) err)
   in unParser p s cok' cerr' eok' eerr'

-- | A single piece of info to be rendered with 'dbgLog'.
data DbgItem s e a
  = DbgIn [Token s]
  | DbgCOK [Token s] a
  | DbgCERR [Token s] (ParseError s e)
  | DbgEOK [Token s] a
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
    msg = case item of
      DbgIn ts ->
        "IN: " ++ showStream pxy ts
      DbgCOK ts a ->
        "MATCH (COK): " ++ showStream pxy ts ++ "\nVALUE: " ++ show a
      DbgCERR ts e ->
        "MATCH (CERR): " ++ showStream pxy ts ++ "\nERROR:\n" ++ parseErrorPretty e
      DbgEOK ts a ->
        "MATCH (EOK): " ++ showStream pxy ts ++ "\nVALUE: " ++ show a
      DbgEERR ts e ->
        "MATCH (EERR): " ++ showStream pxy ts ++ "\nERROR:\n" ++ parseErrorPretty e

-- | Pretty-print a list of tokens.
showStream :: VisualStream s => Proxy s -> [Token s] -> String
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
streamTake :: forall s. Stream s => Int -> s -> [Token s]
streamTake n s =
  case fst <$> takeN_ n s of
    Nothing -> []
    Just chk -> chunkToTokens (Proxy :: Proxy s) chk
