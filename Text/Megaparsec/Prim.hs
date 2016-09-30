-- |
-- Module      :  Text.Megaparsec.Prim
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The primitive parser combinators.

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# OPTIONS_HADDOCK not-home            #-}

module Text.Megaparsec.Prim
  ( -- * Data types
    State (..)
  , Stream (..)
  , Parsec
  , ParsecT
    -- * Primitive combinators
  , MonadParsec (..)
  , (<?>)
  , unexpected
    -- * Parser state combinators
  , getInput
  , setInput
  , getPosition
  , setPosition
  , pushPosition
  , popPosition
  , getTabWidth
  , setTabWidth
  , setParserState
    -- * Running parser
  , runParser
  , runParser'
  , runParserT
  , runParserT'
  , parse
  , parseMaybe
  , parseTest
    -- * Debugging
  , dbg )
where

import Control.DeepSeq
import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (state)
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Data.Data (Data)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid hiding ((<>))
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import Debug.Trace
import GHC.Generics
import Prelude hiding (all)
import Test.QuickCheck hiding (Result (..), label)
import qualified Control.Applicative               as A
import qualified Control.Monad.Fail                as Fail
import qualified Control.Monad.Trans.Reader        as L
import qualified Control.Monad.Trans.State.Lazy    as L
import qualified Control.Monad.Trans.State.Strict  as S
import qualified Control.Monad.Trans.Writer.Lazy   as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Data.ByteString.Char8             as B
import qualified Data.ByteString.Lazy.Char8        as BL
import qualified Data.List.NonEmpty                as NE
import qualified Data.Set                          as E
import qualified Data.Text                         as T
import qualified Data.Text.Lazy                    as TL

import Text.Megaparsec.Error
import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Data types

-- | This is Megaparsec's state, it's parametrized over stream type @s@.

data State s = State
  { stateInput    :: s
  , statePos      :: NonEmpty SourcePos
  , stateTabWidth :: Pos }
  deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (State s)

instance Arbitrary a => Arbitrary (State a) where
  arbitrary = State
    <$> arbitrary
    <*>
#if !MIN_VERSION_QuickCheck(2,9,0)
      (NE.fromList . getNonEmpty <$> arbitrary)
#else
      arbitrary
#endif
    <*> (unsafePos <$> choose (1, 20))

-- | All information available after parsing. This includes consumption of
-- input, success (with returned value) or failure (with parse error), and
-- parser state at the end of parsing.
--
-- See also: 'Consumption', 'Result'.

data Reply e s a = Reply (State s) Consumption (Result (Token s) e a)

-- | This data structure represents an aspect of result of parser's
-- work.
--
-- See also: 'Result', 'Reply'.

data Consumption
  = Consumed -- ^ Some part of input stream was consumed
  | Virgin   -- ^ No input was consumed

-- | This data structure represents an aspect of result of parser's
-- work.
--
-- See also: 'Consumption', 'Reply'.

data Result t e a
  = OK a                   -- ^ Parser succeeded
  | Error (ParseError t e) -- ^ Parser failed

-- | 'Hints' represent collection of strings to be included into
-- 'ParserError' as “expected” message items when a parser fails without
-- consuming input right after successful parser that produced the hints.
--
-- For example, without hints you could get:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- 1:2:
-- unexpected 'a'
-- expecting end of input
--
-- We're getting better error messages with help of hints:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- 1:2:
-- unexpected 'a'
-- expecting 'r' or end of input

newtype Hints t = Hints [Set (ErrorItem t)] deriving (Semigroup, Monoid)

-- | Convert 'ParseError' record into 'Hints'.

toHints :: ParseError t e -> Hints t
toHints err = Hints hints
  where hints = if E.null msgs then [] else [msgs]
        msgs  = errorExpected err
{-# INLINE toHints #-}

-- | @withHints hs c@ makes “error” continuation @c@ use given hints @hs@.
--
-- Note that if resulting continuation gets 'ParseError' that has only
-- custom data in it (no “unexpected” or “expected” items), hints are
-- ignored.

withHints :: Ord (Token s)
  => Hints (Token s)   -- ^ Hints to use
  -> (ParseError (Token s) e -> State s -> m b) -- ^ Continuation to influence
  -> ParseError (Token s) e -- ^ First argument of resulting continuation
  -> State s           -- ^ Second argument of resulting continuation
  -> m b
withHints (Hints ps') c e@(ParseError pos us ps xs) =
  if E.null us && E.null ps && not (E.null xs)
    then c e
    else c (ParseError pos us (E.unions (ps : ps')) xs)
{-# INLINE withHints #-}

-- | @accHints hs c@ results in “OK” continuation that will add given hints
-- @hs@ to third argument of original continuation @c@.

accHints
  :: Hints t           -- ^ 'Hints' to add
  -> (a -> State s -> Hints t -> m b) -- ^ An “OK” continuation to alter
  -> a                 -- ^ First argument of resulting continuation
  -> State s           -- ^ Second argument of resulting continuation
  -> Hints t           -- ^ Third argument of resulting continuation
  -> m b
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)
{-# INLINE accHints #-}

-- | Replace most recent group of hints (if any) with given 'ErrorItem' (or
-- delete it if 'Nothing' is given). This is used in 'label' primitive.

refreshLastHint :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshLastHint (Hints [])     _        = Hints []
refreshLastHint (Hints (_:xs)) Nothing  = Hints xs
refreshLastHint (Hints (_:xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

-- | An instance of @Stream s@ has stream type @s@. Token type is determined
-- by the stream and can be found via 'Token' type function.

class Ord (Token s) => Stream s where

  -- | Type of token in stream.
  --
  -- @since 5.0.0

  type Token s :: *

  -- | Get next token from the stream. If the stream is empty, return
  -- 'Nothing'.

  uncons :: s -> Maybe (Token s, s)

  -- | Update position in stream given tab width, current position, and
  -- current token. The result is a tuple where the first element will be
  -- used to report parse errors for current token, while the second element
  -- is the incremented position that will be stored in parser's state.
  --
  -- When you work with streams where elements do not contain information
  -- about their position in input, result is usually consists of the third
  -- argument unchanged and incremented position calculated with respect to
  -- current token. This is how default instances of 'Stream' work (they use
  -- 'defaultUpdatePos', which may be a good starting point for your own
  -- position-advancing function).
  --
  -- When you wish to deal with stream of tokens where every token “knows”
  -- its start and end position in input (for example, you have produced the
  -- stream with Happy\/Alex), then the best strategy is to use the start
  -- position as actual element position and provide the end position of the
  -- token as incremented one.
  --
  -- @since 5.0.0

  updatePos
    :: Proxy s -- ^ Proxy clarifying stream type ('Token' is not injective)
    -> Pos             -- ^ Tab width
    -> SourcePos       -- ^ Current position
    -> Token s         -- ^ Current token
    -> (SourcePos, SourcePos) -- ^ Actual position and incremented position

instance Stream String where
  type Token String = Char
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream B.ByteString where
  type Token B.ByteString = Char
  uncons = B.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream BL.ByteString where
  type Token BL.ByteString = Char
  uncons = BL.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream T.Text where
  type Token T.Text = Char
  uncons = T.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

instance Stream TL.Text where
  type Token TL.Text = Char
  uncons = TL.uncons
  {-# INLINE uncons #-}
  updatePos = const defaultUpdatePos
  {-# INLINE updatePos #-}

-- If you're reading this, you may be interested in how Megaparsec works on
-- lower level. That's quite simple. 'ParsecT' is a wrapper around function
-- that takes five arguments:
--
--     * State. It includes input stream, position in input stream and
--     current value of tab width.
--
--     * “Consumed-OK” continuation (cok). This is a function that takes
--     three arguments: result of parsing, state after parsing, and hints
--     (see their description above). This continuation is called when
--     something has been consumed during parsing and result is OK (no error
--     occurred).
--
--     * “Consumed-error” continuation (cerr). This function is called when
--     some part of input stream has been consumed and parsing resulted in
--     an error. This continuation takes 'ParseError' and state information
--     at the time error occurred.
--
--     * “Empty-OK” continuation (eok). The function takes the same
--     arguments as “consumed-OK” continuation. “Empty-OK” is called when no
--     input has been consumed and no error occurred.
--
--     * “Empty-error” continuation (eerr). The function is called when no
--     input has been consumed, but nonetheless parsing resulted in an
--     error. Just like “consumed-error”, the continuation takes
--     'ParseError' record and state information.
--
-- You call specific continuation when you want to proceed in that specific
-- branch of control flow.

-- | @Parsec@ is non-transformer variant of more general 'ParsecT'
-- monad transformer.

type Parsec e s = ParsecT e s Identity

-- | @ParsecT e s m a@ is a parser with custom data component of error @e@,
-- stream type @s@, underlying monad @m@ and return type @a@.

newtype ParsecT e s m a = ParsecT
  { unParser
      :: forall b. State s
      -> (a -> State s   -> Hints (Token s) -> m b) -- consumed-OK
      -> (ParseError (Token s) e -> State s -> m b) -- consumed-error
      -> (a -> State s   -> Hints (Token s) -> m b) -- empty-OK
      -> (ParseError (Token s) e -> State s -> m b) -- empty-error
      -> m b }

instance Functor (ParsecT e s m) where
  fmap = pMap

pMap :: (a -> b) -> ParsecT e s m a -> ParsecT e s m b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

instance (ErrorComponent e, Stream s) => A.Applicative (ParsecT e s m) where
  pure     = pPure
  (<*>)    = pAp
  p1 *> p2 = p1 `pBind` const p2
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

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

instance (ErrorComponent e, Stream s) => A.Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus
  many p = reverse <$> manyAcc p

manyAcc :: ParsecT e s m a -> ParsecT e s m [a]
manyAcc p = ParsecT $ \s cok cerr eok _ ->
  let errToHints c err _ = c (toHints err)
      walk xs x s' _ =
        unParser p s'
        (seq xs $ walk $ x:xs)       -- consumed-OK
        cerr                         -- consumed-error
        manyErr                      -- empty-OK
        (errToHints $ cok (x:xs) s') -- empty-error
  in unParser p s (walk []) cerr manyErr (errToHints $ eok [] s)

manyErr :: a
manyErr = error $
  "Text.Megaparsec.Prim.many: combinator 'many' is applied to a parser"
  ++ " that may consume no input."

instance (ErrorComponent e, Stream s)
    => Monad (ParsecT e s m) where
  return = pure
  (>>=)  = pBind
  fail   = Fail.fail

pPure :: a -> ParsecT e s m a
pPure x = ParsecT $ \s _ _ eok _ -> eok x s mempty
{-# INLINE pPure #-}

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

instance (ErrorComponent e, Stream s)
    => Fail.MonadFail (ParsecT e s m) where
  fail = pFail

pFail :: ErrorComponent e => String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ pos _) _ _ _ eerr ->
  eerr (ParseError pos E.empty E.empty d) s
  where d = E.singleton (representFail msg)
{-# INLINE pFail #-}

-- | Low-level creation of the 'ParsecT' type.

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

instance (ErrorComponent e, Stream s, MonadIO m)
    => MonadIO (ParsecT e s m) where
  liftIO = lift . liftIO

instance (ErrorComponent e, Stream s, MonadReader r m)
    => MonadReader r (ParsecT e s m) where
  ask       = lift ask
  local f p = mkPT $ \s -> local f (runParsecT p s)

instance (ErrorComponent e, Stream s, MonadState st m)
    => MonadState st (ParsecT e s m) where
  get = lift get
  put = lift . put

instance (ErrorComponent e, Stream s, MonadCont m)
    => MonadCont (ParsecT e s m) where
  callCC f = mkPT $ \s ->
    callCC $ \c ->
      runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s
    where pack s a = Reply s Virgin (OK a)

instance (ErrorComponent e, Stream s, MonadError e' m)
    => MonadError e' (ParsecT e s m) where
  throwError = lift . throwError
  p `catchError` h = mkPT $ \s ->
    runParsecT p s `catchError` \e ->
      runParsecT (h e) s

instance (ErrorComponent e, Stream s)
    => MonadPlus (ParsecT e s m) where
  mzero = pZero
  mplus = pPlus

pZero :: ParsecT e s m a
pZero = ParsecT $ \s@(State _ pos _) _ _ _ eerr ->
  eerr (ParseError pos E.empty E.empty E.empty) s
{-# INLINE pZero #-}

pPlus :: (ErrorComponent e, Stream s)
  => ParsecT e s m a
  -> ParsecT e s m a
  -> ParsecT e s m a
pPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err ms =
        let ncerr err' s' = cerr (err' <> err) (longestMatch ms s')
            neok x s' hs  = eok x s' (toHints err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

-- | From two states, return the one with greater textual position. If the
-- positions are equal, prefer the latter state.

longestMatch :: State s -> State s -> State s
longestMatch s1@(State _ pos1 _) s2@(State _ pos2 _) =
  case pos1 `compare` pos2 of
    LT -> s2
    EQ -> s2
    GT -> s1
{-# INLINE longestMatch #-}

instance MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s _ _ eok _ ->
    amb >>= \a -> eok a s mempty

----------------------------------------------------------------------------
-- Primitive combinators

-- | Type class describing parsers independent of input type.

class (ErrorComponent e, Stream s, A.Alternative m, MonadPlus m)
    => MonadParsec e s m | m -> e s where

  -- | The most general way to stop parsing and report 'ParseError'.
  --
  -- 'unexpected' is defined in terms of this function:
  --
  -- > unexpected item = failure (Set.singleton item) Set.empty Set.empty
  --
  -- @since 4.2.0

  failure
    :: Set (ErrorItem (Token s)) -- ^ Unexpected items
    -> Set (ErrorItem (Token s)) -- ^ Expected items
    -> Set e                     -- ^ Custom data
    -> m a

  -- | The parser @label name p@ behaves as parser @p@, but whenever the
  -- parser @p@ fails /without consuming any input/, it replaces names of
  -- “expected” tokens with the name @name@.

  label :: String -> m a -> m a

  -- | @hidden p@ behaves just like parser @p@, but it doesn't show any
  -- “expected” tokens in error message when @p@ fails.

  hidden :: m a -> m a
  hidden = label ""

  -- | The parser @try p@ behaves like parser @p@, except that it backtracks
  -- parser state when @p@ fails (either consuming input or not).
  --
  -- This combinator is used whenever arbitrary look ahead is needed. Since
  -- it pretends that it hasn't consumed any input when @p@ fails, the
  -- ('A.<|>') combinator will try its second alternative even when the
  -- first parser failed while consuming input.
  --
  -- For example, here is a parser that is supposed to parse word “let” or
  -- “lexical”:
  --
  -- >>> parseTest (string "let" <|> string "lexical") "lexical"
  -- 1:1:
  -- unexpected "lex"
  -- expecting "let"
  --
  -- What happens here? First parser consumes “le” and fails (because it
  -- doesn't see a “t”). The second parser, however, isn't tried, since the
  -- first parser has already consumed some input! @try@ fixes this behavior
  -- and allows backtracking to work:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "lexical"
  -- "lexical"
  --
  -- @try@ also improves error messages in case of overlapping alternatives,
  -- because Megaparsec's hint system can be used:
  --
  -- >>> parseTest (try (string "let") <|> string "lexical") "le"
  -- 1:1:
  -- unexpected "le"
  -- expecting "let" or "lexical"
  --
  -- Please note that as of Megaparsec 4.4.0, 'string' backtracks
  -- automatically (see 'tokens'), so it does not need 'try'. However, the
  -- examples above demonstrate the idea behind 'try' so well that it was
  -- decided to keep them.

  try :: m a -> m a

  -- | If @p@ in @lookAhead p@ succeeds (either consuming input or not) the
  -- whole parser behaves like @p@ succeeded without consuming anything
  -- (parser state is not updated as well). If @p@ fails, @lookAhead@ has no
  -- effect, i.e. it will fail consuming input if @p@ fails consuming input.
  -- Combine with 'try' if this is undesirable.

  lookAhead :: m a -> m a

  -- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
  -- /never consumes/ any input and /never modifies/ parser state. It can be
  -- used to implement the “longest match” rule.

  notFollowedBy :: m a -> m ()

  -- | @withRecovery r p@ allows continue parsing even if parser @p@ fails.
  -- In this case @r@ is called with actual 'ParseError' as its argument.
  -- Typical usage is to return value signifying failure to parse this
  -- particular object and to consume some part of input up to start of next
  -- object.
  --
  -- Note that if @r@ fails, original error message is reported as if
  -- without 'withRecovery'. In no way recovering parser @r@ can influence
  -- error messages.
  --
  -- @since 4.4.0

  withRecovery
    :: (ParseError (Token s) e -> m a) -- ^ How to recover from failure
    -> m a             -- ^ Original parser
    -> m a             -- ^ Parser that can recover from failures

  -- | This parser only succeeds at the end of the input.

  eof :: m ()

  -- | The parser @token test mrep@ accepts a token @t@ with result @x@ when
  -- the function @test t@ returns @'Right' x@. @mrep@ may provide
  -- representation of the token to report in error messages when input
  -- stream in empty.
  --
  -- This is the most primitive combinator for accepting tokens. For
  -- example, the 'Text.Megaparsec.Char.satisfy' parser is implemented as:
  --
  -- > satisfy f = token testChar Nothing
  -- >   where
  -- >     testChar x =
  -- >       if f x
  -- >         then Right x
  -- >         else Left (Set.singleton (Tokens (x:|[])), Set.empty, Set.empty)

  token
    :: (Token s -> Either ( Set (ErrorItem (Token s))
                          , Set (ErrorItem (Token s))
                          , Set e ) a)
       -- ^ Matching function for the token to parse, it allows to construct
       -- arbitrary error message on failure as well; sets in three-tuple
       -- are: unexpected items, expected items, and custom data pieces
    -> Maybe (Token s) -- ^ Token to report when input stream is empty
    -> m a

  -- | The parser @tokens test@ parses list of tokens and returns it.
  -- Supplied predicate @test@ is used to check equality of given and parsed
  -- tokens.
  --
  -- This can be used for example to write 'Text.Megaparsec.Char.string':
  --
  -- > string = tokens (==)
  --
  -- Note that beginning from Megaparsec 4.4.0, this is an auto-backtracking
  -- primitive, which means that if it fails, it never consumes any
  -- input. This is done to make its consumption model match how error
  -- messages for this primitive are reported (which becomes an important
  -- thing as user gets more control with primitives like 'withRecovery'):
  --
  -- >>> parseTest (string "abc") "abd"
  -- 1:1:
  -- unexpected "abd"
  -- expecting "abc"
  --
  -- This means, in particular, that it's no longer necessary to use 'try'
  -- with 'tokens'-based parsers, such as 'Text.Megaparsec.Char.string' and
  -- 'Text.Megaparsec.Char.string''. This feature /does not/ affect
  -- performance in any way.

  tokens
    :: (Token s -> Token s -> Bool)
       -- ^ Predicate to check equality of tokens
    -> [Token s]
       -- ^ List of tokens to parse
    -> m [Token s]

  -- | Returns the full parser state as a 'State' record.

  getParserState :: m (State s)

  -- | @updateParserState f@ applies function @f@ to the parser state.

  updateParserState :: (State s -> State s) -> m ()

instance (ErrorComponent e, Stream s) => MonadParsec e s (ParsecT e s m) where
  failure           = pFailure
  label             = pLabel
  try               = pTry
  lookAhead         = pLookAhead
  notFollowedBy     = pNotFollowedBy
  withRecovery      = pWithRecovery
  eof               = pEof
  token             = pToken
  tokens            = pTokens
  getParserState    = pGetParserState
  updateParserState = pUpdateParserState

pFailure
  :: Set (ErrorItem (Token s))
  -> Set (ErrorItem (Token s))
  -> Set e
  -> ParsecT e s m a
pFailure us ps xs = ParsecT $ \s@(State _ pos _) _ _ _ eerr ->
  eerr (ParseError pos us ps xs) s
{-# INLINE pFailure #-}

pLabel :: String -> ParsecT e s m a -> ParsecT e s m a
pLabel l p = ParsecT $ \s cok cerr eok eerr ->
  let el = Label <$> NE.nonEmpty l
      cl = Label . (NE.fromList "rest of " <>) <$> NE.nonEmpty l
      cok' x s' hs = cok x s' (refreshLastHint hs cl)
      eok' x s' hs = eok x s' (refreshLastHint hs el)
      eerr'    err = eerr err
        { errorExpected = maybe E.empty E.singleton el }
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
pNotFollowedBy p = ParsecT $ \s@(State input pos _) _ _ eok eerr ->
  let what = maybe EndOfInput (Tokens . nes . fst) (uncons input)
      unexpect u = ParseError pos (E.singleton u) E.empty E.empty
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
            reok x s' _ = eok x s' (toHints err)
            reerr   _ _ = cerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
      meerr err ms =
        let rcok x s' _ = cok x s' (toHints err)
            rcerr   _ _ = eerr err ms
            reok x s' _ = eok x s' (toHints err)
            reerr   _ _ = eerr err ms
        in unParser (r err) ms rcok rcerr reok reerr
  in unParser p s cok mcerr eok meerr
{-# INLINE pWithRecovery #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input (pos:|z) w) _ _ eok eerr ->
  case uncons input of
    Nothing    -> eok () s mempty
    Just (x,_) ->
      let !apos = fst (updatePos (Proxy :: Proxy s) w pos x)
      in eerr ParseError
          { errorPos        = apos:|z
          , errorUnexpected = (E.singleton . Tokens . nes) x
          , errorExpected   = E.singleton EndOfInput
          , errorCustom     = E.empty }
          (State input (apos:|z) w)
{-# INLINE pEof #-}

pToken :: forall e s m a. Stream s
  => (Token s -> Either ( Set (ErrorItem (Token s))
                        , Set (ErrorItem (Token s))
                        , Set e ) a)
  -> Maybe (Token s)
  -> ParsecT e s m a
pToken test mtoken = ParsecT $ \s@(State input (pos:|z) w) cok _ _ eerr ->
  case uncons input of
    Nothing -> eerr ParseError
      { errorPos        = pos:|z
      , errorUnexpected = E.singleton EndOfInput
      , errorExpected   = maybe E.empty (E.singleton . Tokens . nes) mtoken
      , errorCustom     = E.empty } s
    Just (c,cs) ->
      let (apos, npos) = updatePos (Proxy :: Proxy s) w pos c
      in case test c of
        Left (us, ps, xs) ->
          apos `seq` eerr
            (ParseError (apos:|z) us ps xs)
            (State input (apos:|z) w)
        Right x ->
          let newstate = State cs (npos:|z) w
          in npos `seq` cok x newstate mempty
{-# INLINE pToken #-}

pTokens :: forall e s m. Stream s
  => (Token s -> Token s -> Bool)
  -> [Token s]
  -> ParsecT e s m [Token s]
pTokens _ [] = ParsecT $ \s _ _ eok _ -> eok [] s mempty
pTokens test tts = ParsecT $ \s@(State input (pos:|z) w) cok _ _ eerr ->
  let updatePos' = updatePos (Proxy :: Proxy s) w
      toTokens   = Tokens . NE.fromList . reverse
      unexpect pos' u = ParseError
        { errorPos        = pos'
        , errorUnexpected = E.singleton u
        , errorExpected   = (E.singleton . Tokens . NE.fromList) tts
        , errorCustom     = E.empty }
      go _ [] is rs =
        let ris   = reverse is
            !npos = foldl' (\p t -> snd (updatePos' p t)) pos ris
        in cok ris (State rs (npos:|z) w) mempty
      go apos (t:ts) is rs =
        case uncons rs of
          Nothing ->
            apos `seq` eerr
              (unexpect (apos:|z) (toTokens is))
              (State input (apos:|z) w)
          Just (x,xs) ->
            if test t x
              then go apos ts (x:is) xs
              else apos `seq` eerr
                     (unexpect (apos:|z) . toTokens $ x:is)
                     (State input (apos:|z) w)
  in case uncons input of
       Nothing ->
         eerr (unexpect (pos:|z) EndOfInput) s
       Just (x,xs) ->
         let t:ts = tts
             apos = fst (updatePos' pos x)
         in if test t x
              then go apos ts [x] xs
              else apos `seq` eerr
                     (unexpect (apos:|z) $ Tokens (nes x))
                     (State input (apos:|z) w)
{-# INLINE pTokens #-}

pGetParserState :: ParsecT e s m (State s)
pGetParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s -> State s) -> ParsecT e s m ()
pUpdateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE pUpdateParserState #-}

-- | A synonym for 'label' in form of an operator.

infix 0 <?>

(<?>) :: MonadParsec e s m => m a -> String -> m a
(<?>) = flip label

-- | The parser @unexpected item@ always fails with an error message telling
-- about unexpected item @item@ without consuming any input.

unexpected :: MonadParsec e s m => ErrorItem (Token s) -> m a
unexpected item = failure (E.singleton item) E.empty E.empty
{-# INLINE unexpected #-}

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

----------------------------------------------------------------------------
-- Parser state combinators

-- | Return the current input.

getInput :: MonadParsec e s m => m s
getInput = stateInput <$> getParserState

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- 'setInput' functions can for example be used to deal with include files.

setInput :: MonadParsec e s m => s -> m ()
setInput s = updateParserState (\(State _ pos w) -> State s pos w)

-- | Return the current source position.
--
-- See also: 'setPosition', 'pushPosition', 'popPosition', and 'SourcePos'.

getPosition :: MonadParsec e s m => m SourcePos
getPosition = NE.head . statePos <$> getParserState

-- | @setPosition pos@ sets the current source position to @pos@.
--
-- See also: 'getPosition', 'pushPosition', 'popPosition', and 'SourcePos'.

setPosition :: MonadParsec e s m => SourcePos -> m ()
setPosition pos = updateParserState $ \(State s (_:|z) w) ->
  State s (pos:|z) w

-- | Push given position into stack of positions and continue parsing
-- working with this position. Useful for working with include files and the
-- like.
--
-- See also: 'getPosition', 'setPosition', 'popPosition', and 'SourcePos'.
--
-- @since 5.0.0

pushPosition :: MonadParsec e s m => SourcePos -> m ()
pushPosition pos = updateParserState $ \(State s z w) ->
  State s (NE.cons pos z) w

-- | Pop a position from stack of positions unless it only contains one
-- element (in that case stack of positions remains the same). This is how
-- to return to previous source file after 'pushPosition'.
--
-- See also: 'getPosition', 'setPosition', 'pushPosition', and 'SourcePos'.
--
-- @since 5.0.0

popPosition :: MonadParsec e s m => m ()
popPosition = updateParserState $ \(State s z w) ->
  case snd (NE.uncons z) of
    Nothing -> State s z w
    Just z' -> State s z' w

-- | Return tab width. Default tab width is equal to 'defaultTabWidth'. You
-- can set different tab width with help of 'setTabWidth'.

getTabWidth :: MonadParsec e s m => m Pos
getTabWidth = stateTabWidth <$> getParserState

-- | Set tab width. If argument of the function is not positive number,
-- 'defaultTabWidth' will be used.

setTabWidth :: MonadParsec e s m => Pos -> m ()
setTabWidth w = updateParserState (\(State s pos _) -> State s pos w)

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: MonadParsec e s m => State s -> m ()
setParserState st = updateParserState (const st)

----------------------------------------------------------------------------
-- Running a parser

-- | @parse p file input@ runs parser @p@ over 'Identity' (see 'runParserT'
-- if you're using the 'ParsecT' monad transformer; 'parse' itself is just a
-- synonym for 'runParser'). It returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right'). 'parseErrorPretty' can be used to turn
-- 'ParseError' into the string representation of the error message. See
-- "Text.Megaparsec.Error" if you need to do more advanced error analysis.
--
-- > main = case (parse numbers "" "11,2,43") of
-- >          Left err -> putStr (parseErrorPretty err)
-- >          Right xs -> print (sum xs)
-- >
-- > numbers = integer `sepBy` char ','

parse
  :: Parsec e s a -- ^ Parser to run
  -> String       -- ^ Name of source file
  -> s            -- ^ Input for parser
  -> Either (ParseError (Token s) e) a
parse = runParser

-- | @parseMaybe p input@ runs parser @p@ on @input@ and returns result
-- inside 'Just' on success and 'Nothing' on failure. This function also
-- parses 'eof', so if the parser doesn't consume all of its input, it will
-- fail.
--
-- The function is supposed to be useful for lightweight parsing, where
-- error messages (and thus file name) are not important and entire input
-- should be parsed. For example it can be used when parsing of single
-- number according to specification of its format is desired.

parseMaybe :: (ErrorComponent e, Stream s) => Parsec e s a -> s -> Maybe a
parseMaybe p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

-- | The expression @parseTest p input@ applies a parser @p@ against input
-- @input@ and prints the result to stdout. Useful for testing.

parseTest :: ( ShowErrorComponent e
             , Ord (Token s)
             , ShowToken (Token s)
             , Show a )
  => Parsec e s a -- ^ Parser to run
  -> s            -- ^ Input for parser
  -> IO ()
parseTest p input =
  case parse p "" input of
    Left  e -> putStr (parseErrorPretty e)
    Right x -> print x

-- | @runParser p file input@ runs parser @p@ on the input list of tokens
-- @input@, obtained from source @file@. The @file@ is only used in error
-- messages and may be the empty string. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- > parseFromFile p file = runParser p file <$> readFile file

runParser
  :: Parsec e s a -- ^ Parser to run
  -> String     -- ^ Name of source file
  -> s          -- ^ Input for parser
  -> Either (ParseError (Token s) e) a
runParser p name s = snd $ runParser' p (initialState name s)

-- | The function is similar to 'runParser' with the difference that it
-- accepts and returns parser state. This allows to specify arbitrary
-- textual position at the beginning of parsing, for example. This is the
-- most general way to run a parser over the 'Identity' monad.
--
-- @since 4.2.0

runParser'
  :: Parsec e s a -- ^ Parser to run
  -> State s    -- ^ Initial state
  -> (State s, Either (ParseError (Token s) e) a)
runParser' p = runIdentity . runParserT' p

-- | @runParserT p file input@ runs parser @p@ on the input list of tokens
-- @input@, obtained from source @file@. The @file@ is only used in error
-- messages and may be the empty string. Returns a computation in the
-- underlying monad @m@ that returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').

runParserT :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> String        -- ^ Name of source file
  -> s             -- ^ Input for parser
  -> m (Either (ParseError (Token s) e) a)
runParserT p name s = snd `liftM` runParserT' p (initialState name s)

-- | This function is similar to 'runParserT', but like 'runParser'' it
-- accepts and returns parser state. This is thus the most general way to
-- run a parser.
--
-- @since 4.2.0

runParserT' :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> State s       -- ^ Initial state
  -> m (State s, Either (ParseError (Token s) e) a)
runParserT' p s = do
  (Reply s' _ result) <- runParsecT p s
  case result of
    OK    x -> return (s', Right x)
    Error e -> return (s', Left  e)

-- | Given name of source file and input construct initial state for parser.

initialState :: String -> s -> State s
initialState name s = State s (initialPos name :| []) defaultTabWidth

-- | Low-level unpacking of the 'ParsecT' type. 'runParserT' and 'runParser'
-- are built upon this.

runParsecT :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> State s       -- ^ Initial state
  -> m (Reply e s a)
runParsecT p s = unParser p s cok cerr eok eerr
  where cok a s' _  = return $ Reply s' Consumed (OK a)
        cerr err s' = return $ Reply s' Consumed (Error err)
        eok a s' _  = return $ Reply s' Virgin   (OK a)
        eerr err s' = return $ Reply s' Virgin   (Error err)

----------------------------------------------------------------------------
-- Instances of 'MonadParsec'

instance MonadParsec e s m => MonadParsec e s (L.StateT st m) where
  failure us ps xs           = lift (failure us ps xs)
  label n       (L.StateT m) = L.StateT $ label n . m
  try           (L.StateT m) = L.StateT $ try . m
  lookAhead     (L.StateT m) = L.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (L.StateT m) = L.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  withRecovery r (L.StateT m) = L.StateT $ \s ->
    withRecovery (\e -> L.runStateT (r e) s) (m s)
  eof                        = lift eof
  token test mt              = lift (token test mt)
  tokens e ts                = lift (tokens e ts)
  getParserState             = lift getParserState
  updateParserState f        = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (S.StateT st m) where
  failure us ps xs           = lift (failure us ps xs)
  label n       (S.StateT m) = S.StateT $ label n . m
  try           (S.StateT m) = S.StateT $ try . m
  lookAhead     (S.StateT m) = S.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (S.StateT m) = S.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  withRecovery r (S.StateT m) = S.StateT $ \s ->
    withRecovery (\e -> S.runStateT (r e) s) (m s)
  eof                        = lift eof
  token test mt              = lift (token test mt)
  tokens e ts                = lift (tokens e ts)
  getParserState             = lift getParserState
  updateParserState f        = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (L.ReaderT st m) where
  failure us ps xs            = lift (failure us ps xs)
  label n       (L.ReaderT m) = L.ReaderT $ label n . m
  try           (L.ReaderT m) = L.ReaderT $ try . m
  lookAhead     (L.ReaderT m) = L.ReaderT $ lookAhead . m
  notFollowedBy (L.ReaderT m) = L.ReaderT $ notFollowedBy . m
  withRecovery r (L.ReaderT m) = L.ReaderT $ \s ->
    withRecovery (\e -> L.runReaderT (r e) s) (m s)
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (L.WriterT w m) where
  failure us ps xs            = lift (failure us ps xs)
  label n       (L.WriterT m) = L.WriterT $ label n m
  try           (L.WriterT m) = L.WriterT $ try m
  lookAhead     (L.WriterT m) = L.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (L.WriterT m) = L.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (L.WriterT m) = L.WriterT $
    withRecovery (L.runWriterT . r) m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (S.WriterT w m) where
  failure us ps xs            = lift (failure us ps xs)
  label n       (S.WriterT m) = S.WriterT $ label n m
  try           (S.WriterT m) = S.WriterT $ try m
  lookAhead     (S.WriterT m) = S.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (S.WriterT m) = S.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (S.WriterT m) = S.WriterT $
    withRecovery (S.runWriterT . r) m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (IdentityT m) where
  failure us ps xs            = lift (failure us ps xs)
  label n       (IdentityT m) = IdentityT $ label n m
  try                         = IdentityT . try . runIdentityT
  lookAhead     (IdentityT m) = IdentityT $ lookAhead m
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  withRecovery r (IdentityT m) = IdentityT $
    withRecovery (runIdentityT . r) m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift $ tokens e ts
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f

----------------------------------------------------------------------------
-- Debugging

-- | @dbg label p@ parser works exactly like @p@, but when it's evaluated it
-- prints information useful for debugging. The @label@ is only used to
-- refer to this parser in the debugging output. This combinator uses the
-- 'trace' function from "Debug.Trace" under the hood.
--
-- Typical usage is to wrap every sub-parser in misbehaving parser with
-- 'dbg' assigning meaningful labels. Then give it a shot and go through the
-- print-out. As of current version, this combinator prints all available
-- information except for /hints/, which are probably only interesting to
-- the maintainer of Megaparsec itself and may be quite verbose to output in
-- general. Let me know if you would like to be able to see hints as part of
-- debugging output.
--
-- The output itself is pretty self-explanatory, although the following
-- abbreviations should be clarified (they are derived from low-level source
-- code):
--
--     * @COK@ — “consumed OK”. The parser consumed input and succeeded.
--     * @CERR@ — “consumed error”. The parser consumed input and failed.
--     * @EOK@ — “empty OK”. The parser succeeded without consuming input.
--     * @EERR@ — “empty error”. The parser failed without consuming input.
--
-- Due to how input streams are represented (see 'Stream'), we need to
-- traverse entire input twice (calculating length before and after @p@
-- parser) to understand what part of input was matched and consumed. This
-- makes this combinator very inefficient, be sure to remove it from your
-- code once you have finished with debugging.
--
-- Finally, it's not possible to lift this function into some monad
-- transformers without introducing surprising behavior (e.g. unexpected
-- state backtracking) or adding otherwise redundant constraints (e.g.
-- 'Show' instance for state), so this helper is only available for
-- 'ParsecT' monad, not 'MonadParsec' in general.
--
-- @since 5.1.0

dbg :: forall e s m a.
  ( Stream s
  , ShowToken (Token s)
  , ShowErrorComponent e
  , Show a )
  => String            -- ^ Debugging label
  -> ParsecT e s m a   -- ^ Parser to debug
  -> ParsecT e s m a   -- ^ Parser that prints debugging messages
dbg lbl p = ParsecT $ \s cok cerr eok eerr ->
  let l = dbgLog lbl :: DbgItem s e a -> String
      cok' x s' hs = flip trace (cok x s' hs) $
        l (DbgIn (unfold (stateInput s))) ++
        l (DbgCOK (streamTake (streamDelta s s') (stateInput s)) x)
      cerr' err s' = flip trace (cerr err s') $
        l (DbgIn (unfold (stateInput s))) ++
        l (DbgCERR (streamTake (streamDelta s s') (stateInput s)) err)
      eok' x s' hs = flip trace (eok x s' hs) $
        l (DbgIn (unfold (stateInput s))) ++
        l (DbgEOK (streamTake (streamDelta s s') (stateInput s)) x)
      eerr' err s' = flip trace (eerr err s') $
        l (DbgIn (unfold (stateInput s))) ++
        l (DbgEERR (streamTake (streamDelta s s') (stateInput s)) err)
  in unParser p s cok' cerr' eok' eerr'

-- | Single piece of info to be rendered with 'dbgLog'.

data DbgItem s e a
  = DbgIn   [Token s]
  | DbgCOK  [Token s] a
  | DbgCERR [Token s] (ParseError (Token s) e)
  | DbgEOK  [Token s] a
  | DbgEERR [Token s] (ParseError (Token s) e)

-- | Render a single piece of debugging info.

dbgLog :: (ShowToken (Token s), ShowErrorComponent e, Show a, Ord (Token s))
  => String            -- ^ Debugging label
  -> DbgItem s e a     -- ^ Information to render
  -> String            -- ^ Rendered result
dbgLog lbl item = prefix msg
  where
    prefix = unlines . fmap ((lbl ++ "> ") ++) . lines
    msg = case item of
      DbgIn   ts   ->
        "IN: " ++ showStream ts
      DbgCOK  ts a ->
        "MATCH (COK): " ++ showStream ts ++ "\nVALUE: " ++ show a
      DbgCERR ts e ->
        "MATCH (CERR): " ++ showStream ts ++ "\nERROR:\n" ++ parseErrorPretty e
      DbgEOK  ts a ->
        "MATCH (EOK): " ++ showStream ts ++ "\nVALUE: " ++ show a
      DbgEERR ts e ->
        "MATCH (EERR): " ++ showStream ts ++ "\nERROR:\n" ++ parseErrorPretty e

-- | Pretty-print a list of tokens.

showStream :: ShowToken t => [t] -> String
showStream ts =
  case NE.nonEmpty ts of
    Nothing -> "<EMPTY>"
    Just ne ->
      let (h, r) = splitAt 40 (showTokens ne)
      in if null r then h else h ++ " <…>"

-- | Calculate difference in length of two input streams from given parser
-- 'State's.

streamDelta :: Stream s
  => State s           -- ^ State of parser before consumption
  -> State s           -- ^ State of parser after consumption
  -> Int               -- ^ Number of consumed tokens
streamDelta s0 s1 = streamLength (stateInput s0) - streamLength (stateInput s1)
  where streamLength s = length (unfold s)

-- | Extract given number of tokens from the stream.

streamTake :: Stream s => Int -> s -> [Token s]
streamTake n s = take n (unfold s)

-- | Custom version of 'unfold' that matches signature of 'uncons' method in
-- 'Stream' type class we use.

unfold :: Stream s => s -> [Token s]
unfold s = case uncons s of
  Nothing -> []
  Just (t, s') -> t : unfold s'
