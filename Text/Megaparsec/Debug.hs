-- |
-- Module      :  Text.Megaparsec.Debug
-- Copyright   :  © 2015–2018 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Debugging helpers.
--
-- @since 7.0.0

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Text.Megaparsec.Debug
  ( dbg )
where

import qualified Data.List.NonEmpty       as NE
import           Debug.Trace
import           Text.Megaparsec.Error
import           Text.Megaparsec.Internal
import           Text.Megaparsec.State
import           Text.Megaparsec.Stream

import           Data.MonoTraversable

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

dbg :: forall e s m a.
  ( Stream s
  , ShowErrorComponent e
  , Show a )
  => String            -- ^ Debugging label
  -> ParsecT e s m a   -- ^ Parser to debug
  -> ParsecT e s m a   -- ^ Parser that prints debugging messages
dbg lbl p = ParsecT $ \st cok cerr eok eerr ->
  let l = dbgLog lbl :: DbgItem s e a -> String
      unfold = streamTake @s 40
      cok' x s' hs = flip trace (cok x s' hs) $
        l (DbgIn (unfold (stateInput st))) ++
        l (DbgCOK (streamTake @s (streamDelta @s st s') (stateInput st)) x)
      cerr' err s' = flip trace (cerr err s') $
        l (DbgIn (unfold (stateInput st))) ++
        l (DbgCERR (streamTake @s (streamDelta @s st s') (stateInput st)) err)
      eok' x s' hs = flip trace (eok x s' hs) $
        l (DbgIn (unfold (stateInput st))) ++
        l (DbgEOK (streamTake @s (streamDelta @s st s') (stateInput st)) x)
      eerr' err s' = flip trace (eerr err s') $
        l (DbgIn (unfold (stateInput st))) ++
        l (DbgEERR (streamTake @s (streamDelta @s st s') (stateInput st)) err)
  in unParser p st cok' cerr' eok' eerr'

-- | A single piece of info to be rendered with 'dbgLog'.

data DbgItem s e a
  = DbgIn   [Element (Tokens s)]
  | DbgCOK  [Element (Tokens s)] a
  | DbgCERR [Element (Tokens s)] (ParseError s e)
  | DbgEOK  [Element (Tokens s)] a
  | DbgEERR [Element (Tokens s)] (ParseError s e)

-- | Render a single piece of debugging info.

dbgLog
  :: forall s e a. (Stream s, ShowErrorComponent e, Show a)
  => String            -- ^ Debugging label
  -> DbgItem s e a     -- ^ Information to render
  -> String            -- ^ Rendered result
dbgLog lbl item = prefix msg
  where
    prefix = unlines . fmap ((lbl ++ "> ") ++) . lines
    msg = case item of
      DbgIn   ts   ->
        "IN: " ++ showStream @s ts
      DbgCOK  ts a ->
        "MATCH (COK): " ++ showStream @s ts ++ "\nVALUE: " ++ show a
      DbgCERR ts e ->
        "MATCH (CERR): " ++ showStream @s ts ++ "\nERROR:\n" ++ parseErrorPretty e
      DbgEOK  ts a ->
        "MATCH (EOK): " ++ showStream @s ts ++ "\nVALUE: " ++ show a
      DbgEERR ts e ->
        "MATCH (EERR): " ++ showStream @s ts ++ "\nERROR:\n" ++ parseErrorPretty e

-- | Pretty-print a list of tokens.

showStream :: forall s . Stream s => [Element (Tokens s)] -> String
showStream ts =
  case NE.nonEmpty ts of
    Nothing -> "<EMPTY>"
    Just ne ->
      let (h, r) = splitAt 40 (prettyTokens @s ne)
      in if null r then h else h ++ " <…>"

-- | Calculate number of consumed tokens given 'State' of parser before and
-- after parsing.

streamDelta
  :: State (Tokens s)           -- ^ State of parser before consumption
  -> State (Tokens s)           -- ^ State of parser after consumption
  -> Int               -- ^ Number of consumed tokens
streamDelta s0 s1 = stateOffset s1 - stateOffset s0

-- | Extract a given number of tokens from the stream.

streamTake :: forall s. Stream s => Int -> Tokens s -> [Element (Tokens s)]
streamTake n s =
  case fst <$> takeN_ @s n s of
    Nothing  -> []
    Just chk -> otoList chk
