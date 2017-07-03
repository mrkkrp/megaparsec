-- |
-- Module      :  Text.Megaparsec
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module includes everything you need to get started writing a parser.
-- If you are new to Megaparsec and don't know where to begin, take a look
-- at the tutorials <https://markkarpov.com/learn-haskell.html#megaparsec-tutorials>.
--
-- In addition to the "Text.Megaparsec" module, which exports and re-exports
-- most everything that you may need, we advise to import
-- "Text.Megaparsec.Char" if you plan to work with a stream of 'Char' tokens
-- or "Text.Megaparsec.Byte" if you indend to parse binary data.
--
-- It is common to start working with the library by defining a type synonym
-- like this:
--
-- > type Parser = Parsec Void Text
-- >                      ^    ^
-- >                      |    |
-- > Custom error component    Type of input
--
-- Then you can write type signatures like @Parser Int@—for a parser that
-- returns an 'Int' for example.
--
-- Megaparsec 6 uses some type-level machinery to provide flexibility
-- without compromising on type safety. Thus type signatures are sometimes
-- necessary to avoid ambiguous types. If you're seeing a error message that
-- reads like “Type variable @e0@ is ambiguous …”, you need to give an
-- explicit signature to your parser to resolve the ambiguity. It's a good
-- idea to provide type signatures for all top-level definitions.
--
-- Megaparsec is capable of a lot. Apart from this standard functionality
-- you can parse permutation phrases with "Text.Megaparsec.Perm",
-- expressions with "Text.Megaparsec.Expr", and even entire languages with
-- "Text.Megaparsec.Lexer". These modules should be imported explicitly
-- along with the two modules mentioned above.

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
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Text.Megaparsec
  ( -- * Re-exports
    module Text.Megaparsec.Pos
  , module Text.Megaparsec.Error
  , module Text.Megaparsec.Stream
  , module Control.Applicative.Combinators
    -- * Data types
  , State (..)
  , Parsec
  , ParsecT
    -- * Running parser
  , parse
  , parseMaybe
  , parseTest
  , runParser
  , runParser'
  , runParserT
  , runParserT'
    -- * Primitive combinators
  , MonadParsec (..)
    -- * Derivatives of primitive combinators
  , (<?>)
  , unexpected
  , match
  , region
  , skipWhileP
  , skipWhile1P
    -- * Parser state combinators
  , getInput
  , setInput
  , getPosition
  , getNextTokenPosition
  , setPosition
  , pushPosition
  , popPosition
  , getTokensProcessed
  , setTokensProcessed
  , getTabWidth
  , setTabWidth
  , setParserState
    -- * Debugging
  , dbg )
where

import Control.Applicative.Combinators
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Data.Proxy
import Data.Semigroup
import Data.Set (Set)
import Data.Typeable (Typeable)
import Debug.Trace
import GHC.Generics
import qualified Control.Applicative               as A
import qualified Control.Monad.Fail                as Fail
import qualified Control.Monad.RWS.Lazy            as L
import qualified Control.Monad.RWS.Strict          as S
import qualified Control.Monad.Trans.Reader        as L
import qualified Control.Monad.Trans.State.Lazy    as L
import qualified Control.Monad.Trans.State.Strict  as S
import qualified Control.Monad.Trans.Writer.Lazy   as L
import qualified Control.Monad.Trans.Writer.Strict as S
import qualified Data.List.NonEmpty                as NE
import qualified Data.Set                          as E

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Stream

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

----------------------------------------------------------------------------
-- Data types

-- | This is the Megaparsec's state, it's parametrized over stream type @s@.

data State s = State
  { stateInput :: s
    -- ^ Current input (already processed input is removed from the stream)
  , statePos :: NonEmpty SourcePos
    -- ^ Current position (column + line number) with support for include files
  , stateTokensProcessed :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
    --
    -- @since 5.2.0
  , stateTabWidth :: Pos
    -- ^ Tab width to use
  } deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (State s)

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
toHints (TrivialError _ _ ps) = Hints (if E.null ps then [] else [ps])
toHints (FancyError   _ _)    = mempty
{-# INLINE toHints #-}

-- | @withHints hs c@ makes “error” continuation @c@ use given hints @hs@.
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

-- | Replace the most recent group of hints (if any) with the given
-- 'ErrorItem' (or delete it if 'Nothing' is given). This is used in 'label'
-- primitive.

refreshLastHint :: Hints t -> Maybe (ErrorItem t) -> Hints t
refreshLastHint (Hints [])     _        = Hints []
refreshLastHint (Hints (_:xs)) Nothing  = Hints xs
refreshLastHint (Hints (_:xs)) (Just m) = Hints (E.singleton m : xs)
{-# INLINE refreshLastHint #-}

-- | @Parsec@ is a non-transformer variant of the more general 'ParsecT'
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

instance (Stream s, Semigroup a) => Semigroup (ParsecT e s m a) where
  (<>) = A.liftA2 (<>)
  {-# INLINE (<>) #-}

instance (Stream s, Monoid a) => Monoid (ParsecT e s m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = A.liftA2 mappend
  {-# INLINE mappend #-}

instance Functor (ParsecT e s m) where
  fmap = pMap

pMap :: (a -> b) -> ParsecT e s m a -> ParsecT e s m b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

instance Stream s => A.Applicative (ParsecT e s m) where
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

instance (Ord e, Stream s) => A.Alternative (ParsecT e s m) where
  empty  = mzero
  (<|>)  = mplus

instance Stream s => Monad (ParsecT e s m) where
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

instance Stream s => Fail.MonadFail (ParsecT e s m) where
  fail = pFail

pFail :: String -> ParsecT e s m a
pFail msg = ParsecT $ \s@(State _ pos _ _) _ _ _ eerr ->
  let d = E.singleton (ErrorFail msg)
  in eerr (FancyError pos d) s
{-# INLINE pFail #-}

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
            neok x s' hs  = eok x s' (toHints err <> hs)
            neerr err' s' = eerr (err' <> err) (longestMatch ms s')
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

instance (Stream s, MonadFix m) => MonadFix (ParsecT e s m) where
  mfix f = mkPT $ \s -> mfix $ \(~(Reply _ _ result)) -> do
    let
      a = case result of
        OK a' -> a'
        Error _ -> error "mfix ParsecT"
    runParsecT (f a) s

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

instance MonadTrans (ParsecT e s) where
  lift amb = ParsecT $ \s _ _ eok _ ->
    amb >>= \a -> eok a s mempty

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

-- | @parseMaybe p input@ runs the parser @p@ on @input@ and returns the
-- result inside 'Just' on success and 'Nothing' on failure. This function
-- also parses 'eof', so if the parser doesn't consume all of its input, it
-- will fail.
--
-- The function is supposed to be useful for lightweight parsing, where
-- error messages (and thus file name) are not important and entire input
-- should be parsed. For example it can be used when parsing of a single
-- number according to specification of its format is desired.

parseMaybe :: (Ord e, Stream s) => Parsec e s a -> s -> Maybe a
parseMaybe p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

-- | The expression @parseTest p input@ applies the parser @p@ against input
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

-- | @runParser p file input@ runs parser @p@ on the input stream of tokens
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

-- | Given name of source file and input construct initial state for parser.

initialState :: String -> s -> State s
initialState name s = State
  { stateInput           = s
  , statePos             = initialPos name :| []
  , stateTokensProcessed = 0
  , stateTabWidth        = defaultTabWidth }

----------------------------------------------------------------------------
-- Primitive combinators

-- | Type class describing parsers independent of input type.

class (Stream s, A.Alternative m, MonadPlus m)
    => MonadParsec e s m | m -> e s where

  -- | The most general way to stop parsing and report a trivial
  -- 'ParseError', that is, collection of unexpected and expected items.
  --
  -- @since 6.0.0

  failure
    :: Maybe (ErrorItem (Token s)) -- ^ Unexpected item
    -> Set (ErrorItem (Token s)) -- ^ Expected items
    -> m a

  -- | The most general way to stop parsing and report a fancy 'ParseError'.
  --
  -- @since 6.0.0

  fancyFailure
    :: Set (ErrorFancy e) -- ^ Fancy error components
    -> m a

  -- | The parser @'label' name p@ behaves as parser @p@, but whenever the
  -- parser @p@ fails /without consuming any input/, it replaces names of
  -- “expected” tokens with the name @name@.

  label :: String -> m a -> m a

  -- | @'hidden' p@ behaves just like parser @p@, but it doesn't show any
  -- “expected” tokens in error message when @p@ fails.

  hidden :: m a -> m a
  hidden = label ""

  -- | The parser @'try' p@ behaves like parser @p@, except that it
  -- backtracks the parser state when @p@ fails (either consuming input or
  -- not).
  --
  -- This combinator is used whenever arbitrary look ahead is needed. Since
  -- it pretends that it hasn't consumed any input when @p@ fails, the
  -- ('A.<|>') combinator will try its second alternative even when the
  -- first parser failed while consuming input.
  --
  -- For example, here is a parser that is supposed to parse the word “let”
  -- or the word “lexical”:
  --
  -- >>> parseTest (string "let" <|> string "lexical") "lexical"
  -- 1:1:
  -- unexpected "lex"
  -- expecting "let"
  --
  -- What happens here? The first parser consumes “le” and fails (because it
  -- doesn't see a “t”). The second parser, however, isn't tried, since the
  -- first parser has already consumed some input! 'try' fixes this behavior
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
  -- __Please note__ that as of Megaparsec 4.4.0, 'string' backtracks
  -- automatically (see 'tokens'), so it does not need 'try'. However, the
  -- examples above demonstrate the idea behind 'try' so well that it was
  -- decided to keep them. You still need to use 'try' when your
  -- alternatives are complex, composite parsers.

  try :: m a -> m a

  -- | If @p@ in @'lookAhead' p@ succeeds (either consuming input or not)
  -- the whole parser behaves like @p@ succeeded without consuming anything
  -- (parser state is not updated as well). If @p@ fails, 'lookAhead' has no
  -- effect, i.e. it will fail consuming input if @p@ fails consuming input.
  -- Combine with 'try' if this is undesirable.

  lookAhead :: m a -> m a

  -- | @'notFollowedBy' p@ only succeeds when the parser @p@ fails. This
  -- parser /never consumes/ any input and /never modifies/ parser state. It
  -- can be used to implement the “longest match” rule.

  notFollowedBy :: m a -> m ()

  -- | @'withRecovery' r p@ allows continue parsing even if parser @p@
  -- fails. In this case @r@ is called with the actual 'ParseError' as its
  -- argument. Typical usage is to return a value signifying failure to
  -- parse this particular object and to consume some part of the input up
  -- to the point where the next object starts.
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

  -- | @'observing' p@ allows to “observe” failure of the @p@ parser, should
  -- it happen, without actually ending parsing, but instead getting the
  -- 'ParseError' in 'Left'. On success parsed value is returned in 'Right'
  -- as usual. Note that this primitive just allows you to observe parse
  -- errors as they happen, it does not backtrack or change how the @p@
  -- parser works in any way.
  --
  -- @since 5.1.0

  observing
    :: m a             -- ^ The parser to run
    -> m (Either (ParseError (Token s) e) a)

  -- | This parser only succeeds at the end of the input.

  eof :: m ()

  -- | The parser @'token' test mrep@ accepts a token @t@ with result @x@
  -- when the function @test t@ returns @'Right' x@. @mrep@ may provide
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
  -- >         else Left (Set.singleton (Tokens (x:|[])), Set.empty)

  token
    :: (Token s -> Either ( Maybe (ErrorItem (Token s))
                          , Set (ErrorItem (Token s)) ) a)
       -- ^ Matching function for the token to parse, it allows to construct
       -- arbitrary error message on failure as well; things in the tuple
       -- are: unexpected item (if any) and expected items
    -> Maybe (Token s) -- ^ Token to report when input stream is empty
    -> m a

  -- | The parser @'tokens' test@ parses a chunk of input and returns it.
  -- Supplied predicate @test@ is used to check equality of given and parsed
  -- chunks after a candidate chunk of correct length is fetched from the
  -- stream.
  --
  -- This can be used for example to write 'Text.Megaparsec.Char.string':
  --
  -- > string = tokens (==)
  --
  -- Note that beginning from Megaparsec 4.4.0, this is an auto-backtracking
  -- primitive, which means that if it fails, it never consumes any input.
  -- This is done to make its consumption model match how error messages for
  -- this primitive are reported (which becomes an important thing as user
  -- gets more control with primitives like 'withRecovery'):
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
    :: (Tokens s -> Tokens s -> Bool)
       -- ^ Predicate to check equality of tokens
    -> Tokens s
       -- ^ List of tokens to parse
    -> m (Tokens s)

  -- | Parse /zero/ or more tokens for which the supplied predicate holds.
  -- Try to use this as much as possible because for many streams the
  -- combinator is much faster than parsers built with 'many' and @satisfy@.
  --
  -- The following equations should clarify the behavior:
  --
  -- > takeWhileP (Just "foo") f = many (satisfy f <?> "foo")
  -- > takeWhileP Nothing f      = many (satisfy f)
  --
  -- The combinator never fails, although it may parse an empty chunk.
  --
  -- @since 6.0.0

  takeWhileP
    :: Maybe String    -- ^ Name for a single token in the row
    -> (Token s -> Bool) -- ^ Predicate to use to test tokens
    -> m (Tokens s)    -- ^ A chunk of matching tokens

  -- | Similar to 'takeWhileP', but fails if it can't parse at least one
  -- token.
  --
  -- @since 6.0.0

  takeWhile1P
    :: Maybe String    -- ^ Name for a single token in the row
    -> (Token s -> Bool) -- ^ Predicate to use to test tokens
    -> m (Tokens s)    -- ^ A chunk of matching tokens

  -- | Return the full parser state as a 'State' record.

  getParserState :: m (State s)

  -- | @updateParserState f@ applies the function @f@ to the parser state.

  updateParserState :: (State s -> State s) -> m ()

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

pObserving
  :: ParsecT e s m a
  -> ParsecT e s m (Either (ParseError (Token s) e) a)
pObserving p = ParsecT $ \s cok _ eok _ ->
  let cerr' err s' = cok (Left err) s' mempty
      eerr' err s' = eok (Left err) s' (toHints err)
  in unParser p s (cok . Right) cerr' (eok . Right) eerr'
{-# INLINE pObserving #-}

pEof :: forall e s m. Stream s => ParsecT e s m ()
pEof = ParsecT $ \s@(State input (pos:|z) tp w) _ _ eok eerr ->
  case take1_ input of
    Nothing    -> eok () s mempty
    Just (x,_) ->
      let !apos = positionAt1 (Proxy :: Proxy s) pos x
          us    = (pure . Tokens . nes) x
          ps    = E.singleton EndOfInput
      in eerr (TrivialError (apos:|z) us ps)
          (State input (apos:|z) tp w)
{-# INLINE pEof #-}

pToken :: forall e s m a. Stream s
  => (Token s -> Either ( Maybe (ErrorItem (Token s))
                        , Set (ErrorItem (Token s)) ) a)
  -> Maybe (Token s)
  -> ParsecT e s m a
pToken test mtoken = ParsecT $ \s@(State input (pos:|z) tp w) cok _ _ eerr ->
  case take1_ input of
    Nothing ->
      let us = pure EndOfInput
          ps = maybe E.empty (E.singleton . Tokens . nes) mtoken
      in eerr (TrivialError (pos:|z) us ps) s
    Just (c,cs) ->
      case test c of
        Left (us, ps) ->
          let !apos = positionAt1 (Proxy :: Proxy s) pos c
          in eerr (TrivialError (apos:|z) us ps)
                  (State input (apos:|z) tp w)
        Right x ->
          let !npos = advance1 (Proxy :: Proxy s) w pos c
              newstate = State cs (npos:|z) (tp + 1) w
          in cok x newstate mempty
{-# INLINE pToken #-}

pTokens :: forall e s m. Stream s
  => (Tokens s -> Tokens s -> Bool)
  -> Tokens s
  -> ParsecT e s m (Tokens s)
pTokens f tts = ParsecT $ \s@(State input (pos:|z) tp w) cok _ _ eerr ->
  let pxy = Proxy :: Proxy s
      unexpect pos' u =
        let us = pure u
            ps = (E.singleton . Tokens . NE.fromList . chunkToTokens pxy) tts
        in TrivialError pos' us ps
      len = chunkLength pxy tts
  in case takeN_ len input of
    Nothing ->
      eerr (unexpect (pos:|z) EndOfInput) s
    Just (tts', input') ->
      if f tts tts'
        then let !npos = advanceN pxy w pos tts'
             in cok tts' (State input' (npos:|z) (tp + len) w) mempty
        else let !apos = positionAtN pxy pos tts'
                 ps = (Tokens . NE.fromList . chunkToTokens pxy) tts'
             in eerr (unexpect (apos:|z) ps) (State input (apos:|z) tp w)
{-# INLINE pTokens #-}

pTakeWhileP :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhileP ml f = ParsecT $ \(State input (pos:|z) tp w) cok _ eok _ ->
  let pxy = Proxy :: Proxy s
      (ts, input') = takeWhile_ f input
      !npos = advanceN pxy w pos ts
      len = chunkLength pxy ts
      hs =
        case ml >>= NE.nonEmpty of
          Nothing -> mempty
          Just l -> (Hints . pure . E.singleton . Label) l
  in if chunkEmpty pxy ts
       then eok ts (State input' (npos:|z) (tp + len) w) hs
       else cok ts (State input' (npos:|z) (tp + len) w) hs
{-# INLINE pTakeWhileP #-}

pTakeWhile1P :: forall e s m. Stream s
  => Maybe String
  -> (Token s -> Bool)
  -> ParsecT e s m (Tokens s)
pTakeWhile1P ml f = ParsecT $ \(State input (pos:|z) tp w) cok _ _ eerr ->
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
            in eerr (TrivialError (apos:|z) us ps)
                    (State input (apos:|z) tp w)
       else let !npos = advanceN pxy w pos ts
            in cok ts (State input' (npos:|z) (tp + len) w) hs
{-# INLINE pTakeWhile1P #-}

pGetParserState :: ParsecT e s m (State s)
pGetParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s -> State s) -> ParsecT e s m ()
pUpdateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE pUpdateParserState #-}

-- | Make a singleton non-empty list from a value.

nes :: a -> NonEmpty a
nes x = x :| []
{-# INLINE nes #-}

instance MonadParsec e s m => MonadParsec e s (L.StateT st m) where
  failure us ps              = lift (failure us ps)
  fancyFailure xs            = lift (fancyFailure xs)
  label n       (L.StateT m) = L.StateT $ label n . m
  try           (L.StateT m) = L.StateT $ try . m
  lookAhead     (L.StateT m) = L.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (L.StateT m) = L.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  withRecovery r (L.StateT m) = L.StateT $ \s ->
    withRecovery (\e -> L.runStateT (r e) s) (m s)
  observing     (L.StateT m) = L.StateT $ \s ->
    fixs s <$> observing (m s)
  eof                        = lift eof
  token test mt              = lift (token test mt)
  tokens e ts                = lift (tokens e ts)
  takeWhileP l f             = lift (takeWhileP l f)
  takeWhile1P l f            = lift (takeWhile1P l f)
  getParserState             = lift getParserState
  updateParserState f        = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (S.StateT st m) where
  failure us ps              = lift (failure us ps)
  fancyFailure xs            = lift (fancyFailure xs)
  label n       (S.StateT m) = S.StateT $ label n . m
  try           (S.StateT m) = S.StateT $ try . m
  lookAhead     (S.StateT m) = S.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (S.StateT m) = S.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  withRecovery r (S.StateT m) = S.StateT $ \s ->
    withRecovery (\e -> S.runStateT (r e) s) (m s)
  observing     (S.StateT m) = S.StateT $ \s ->
    fixs s <$> observing (m s)
  eof                        = lift eof
  token test mt              = lift (token test mt)
  tokens e ts                = lift (tokens e ts)
  takeWhileP l f             = lift (takeWhileP l f)
  takeWhile1P l f            = lift (takeWhile1P l f)
  getParserState             = lift getParserState
  updateParserState f        = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (L.ReaderT r m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n       (L.ReaderT m) = L.ReaderT $ label n . m
  try           (L.ReaderT m) = L.ReaderT $ try . m
  lookAhead     (L.ReaderT m) = L.ReaderT $ lookAhead . m
  notFollowedBy (L.ReaderT m) = L.ReaderT $ notFollowedBy . m
  withRecovery r (L.ReaderT m) = L.ReaderT $ \s ->
    withRecovery (\e -> L.runReaderT (r e) s) (m s)
  observing     (L.ReaderT m) = L.ReaderT $ observing . m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (L.WriterT w m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n       (L.WriterT m) = L.WriterT $ label n m
  try           (L.WriterT m) = L.WriterT $ try m
  lookAhead     (L.WriterT m) = L.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (L.WriterT m) = L.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (L.WriterT m) = L.WriterT $
    withRecovery (L.runWriterT . r) m
  observing     (L.WriterT m) = L.WriterT $
    fixs mempty <$> observing m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (S.WriterT w m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n       (S.WriterT m) = S.WriterT $ label n m
  try           (S.WriterT m) = S.WriterT $ try m
  lookAhead     (S.WriterT m) = S.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (S.WriterT m) = S.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  withRecovery r (S.WriterT m) = S.WriterT $
    withRecovery (S.runWriterT . r) m
  observing     (S.WriterT m) = S.WriterT $
    fixs mempty <$> observing m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (L.RWST r w st m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n          (L.RWST m) = L.RWST $ \r s -> label n (m r s)
  try              (L.RWST m) = L.RWST $ \r s -> try (m r s)
  lookAhead        (L.RWST m) = L.RWST $ \r s -> do
    (x,_,_) <- lookAhead (m r s)
    return (x,s,mempty)
  notFollowedBy    (L.RWST m) = L.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((),s,mempty)
  withRecovery   n (L.RWST m) = L.RWST $ \r s ->
    withRecovery (\e -> L.runRWST (n e) r s) (m r s)
  observing        (L.RWST m) = L.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance (Monoid w, MonadParsec e s m) => MonadParsec e s (S.RWST r w st m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n          (S.RWST m) = S.RWST $ \r s -> label n (m r s)
  try              (S.RWST m) = S.RWST $ \r s -> try (m r s)
  lookAhead        (S.RWST m) = S.RWST $ \r s -> do
    (x,_,_) <- lookAhead (m r s)
    return (x,s,mempty)
  notFollowedBy    (S.RWST m) = S.RWST $ \r s -> do
    notFollowedBy (void $ m r s)
    return ((),s,mempty)
  withRecovery   n (S.RWST m) = S.RWST $ \r s ->
    withRecovery (\e -> S.runRWST (n e) r s) (m r s)
  observing        (S.RWST m) = S.RWST $ \r s ->
    fixs' s <$> observing (m r s)
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift (tokens e ts)
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift (updateParserState f)

instance MonadParsec e s m => MonadParsec e s (IdentityT m) where
  failure us ps               = lift (failure us ps)
  fancyFailure xs             = lift (fancyFailure xs)
  label n       (IdentityT m) = IdentityT $ label n m
  try                         = IdentityT . try . runIdentityT
  lookAhead     (IdentityT m) = IdentityT $ lookAhead m
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  withRecovery r (IdentityT m) = IdentityT $
    withRecovery (runIdentityT . r) m
  observing     (IdentityT m) = IdentityT $ observing m
  eof                         = lift eof
  token test mt               = lift (token test mt)
  tokens e ts                 = lift $ tokens e ts
  takeWhileP l f              = lift (takeWhileP l f)
  takeWhile1P l f             = lift (takeWhile1P l f)
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f

fixs :: s -> Either a (b, s) -> (Either a b, s)
fixs s (Left a)       = (Left a, s)
fixs _ (Right (b, s)) = (Right b, s)
{-# INLINE fixs #-}

fixs' :: Monoid w => s -> Either a (b, s, w) -> (Either a b, s, w)
fixs' s (Left a)        = (Left a, s, mempty)
fixs' _ (Right (b,s,w)) = (Right b, s, w)
{-# INLINE fixs' #-}

----------------------------------------------------------------------------
-- Derivatives of primitive combinators

-- | A synonym for 'label' in the form of an operator.

infix 0 <?>

(<?>) :: MonadParsec e s m => m a -> String -> m a
(<?>) = flip label
{-# INLINE (<?>) #-}

-- | The parser @unexpected item@ fails with an error message telling about
-- unexpected item @item@ without consuming any input.
--
-- > unexpected item = failure (Set.singleton item) Set.empty

unexpected :: MonadParsec e s m => ErrorItem (Token s) -> m a
unexpected item = failure (pure item) E.empty
{-# INLINE unexpected #-}

-- | Return both the result of a parse and the list of tokens that were
-- consumed during parsing. This relies on the change of the
-- 'stateTokensProcessed' value to evaluate how many tokens were consumed.
--
-- @since 5.3.0

match :: MonadParsec e s m => m a -> m (Tokens s, a)
match p = do
  tp  <- getTokensProcessed
  s   <- getInput
  r   <- p
  tp' <- getTokensProcessed
  -- NOTE The 'fromJust' call here should never fail because if the stream
  -- is empty before 'p' (the only case when 'takeN_' can return 'Nothing'
  -- as per its invariants), (tp' - tp) won't be greater than 0, and in that
  -- case 'Just' is guaranteed to be returned as per another invariant of
  -- 'takeN_'.
  return ((fst . fromJust) (takeN_ (tp' - tp) s), r)
{-# INLINEABLE match #-}

-- | Specify how to process 'ParseError's that happen inside of this
-- wrapper. As a side effect of the current implementation changing
-- 'errorPos' with this combinator will also change the final 'statePos' in
-- the parser state.
--
-- @since 5.3.0

region :: MonadParsec e s m
  => (ParseError (Token s) e -> ParseError (Token s) e)
     -- ^ How to process 'ParseError's
  -> m a               -- ^ The “region” that processing applies to
  -> m a
region f m = do
  r <- observing m
  case r of
    Left err ->
      case f err of
        TrivialError pos us ps -> do
          updateParserState $ \st -> st { statePos = pos }
          failure us ps
        FancyError pos xs -> do
          updateParserState $ \st -> st { statePos = pos }
          fancyFailure xs
    Right x -> return x
{-# INLINEABLE region #-}

-- | The same as 'takeWhileP', but discards the result.
--
-- @since 6.0.0

skipWhileP :: MonadParsec e s m
  => Maybe String      -- ^ Name of a single token in the row
  -> (Token s -> Bool) -- ^ Predicate to use to test tokens
  -> m ()
skipWhileP l f = void (takeWhileP l f)
{-# INLINE skipWhileP #-}

-- | The same as 'takeWhile1P', but discards the result.
--
-- @since 6.0.0

skipWhile1P :: MonadParsec e s m
  => Maybe String      -- ^ Name of a single token in the row
  -> (Token s -> Bool) -- ^ Predicate to use to test tokens
  -> m ()
skipWhile1P l f = void (takeWhile1P l f)
{-# INLINE skipWhile1P #-}

----------------------------------------------------------------------------
-- Parser state combinators

-- | Return the current input.

getInput :: MonadParsec e s m => m s
getInput = stateInput <$> getParserState

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- 'setInput' functions can for example be used to deal with include files.

setInput :: MonadParsec e s m => s -> m ()
setInput s = updateParserState (\(State _ pos tp w) -> State s pos tp w)

-- | Return the current source position.
--
-- See also: 'setPosition', 'pushPosition', 'popPosition', and 'SourcePos'.

getPosition :: MonadParsec e s m => m SourcePos
getPosition = NE.head . statePos <$> getParserState

-- | Get the position where the next token in the stream begins. If the
-- stream is empty, return 'Nothing'.
--
-- @since 5.3.0

getNextTokenPosition :: forall e s m. MonadParsec e s m => m (Maybe SourcePos)
getNextTokenPosition = do
  State {..} <- getParserState
  let f = positionAt1 (Proxy :: Proxy s) (NE.head statePos)
  return (f . fst <$> take1_ stateInput)
{-# INLINEABLE getNextTokenPosition #-}

-- | @setPosition pos@ sets the current source position to @pos@.
--
-- See also: 'getPosition', 'pushPosition', 'popPosition', and 'SourcePos'.

setPosition :: MonadParsec e s m => SourcePos -> m ()
setPosition pos = updateParserState $ \(State s (_:|z) tp w) ->
  State s (pos:|z) tp w

-- | Push a position into stack of positions and continue parsing working
-- with this position. Useful for working with include files and the like.
--
-- See also: 'getPosition', 'setPosition', 'popPosition', and 'SourcePos'.
--
-- @since 5.0.0

pushPosition :: MonadParsec e s m => SourcePos -> m ()
pushPosition pos = updateParserState $ \(State s z tp w) ->
  State s (NE.cons pos z) tp w

-- | Pop a position from the stack of positions unless it only contains one
-- element (in that case the stack of positions remains the same). This is
-- how to return to previous source file after 'pushPosition'.
--
-- See also: 'getPosition', 'setPosition', 'pushPosition', and 'SourcePos'.
--
-- @since 5.0.0

popPosition :: MonadParsec e s m => m ()
popPosition = updateParserState $ \(State s z tp w) ->
  case snd (NE.uncons z) of
    Nothing -> State s z  tp w
    Just z' -> State s z' tp w

-- | Get the number of tokens processed so far.
--
-- @since 6.0.0

getTokensProcessed :: MonadParsec e s m => m Int
getTokensProcessed = stateTokensProcessed <$> getParserState

-- | Set the number of tokens processed so far.
--
-- @since 6.0.0

setTokensProcessed :: MonadParsec e s m => Int -> m ()
setTokensProcessed tp = updateParserState $ \(State s pos _ w) ->
  State s pos tp w

-- | Return the tab width. The default tab width is equal to
-- 'defaultTabWidth'. You can set a different tab width with the help of
-- 'setTabWidth'.

getTabWidth :: MonadParsec e s m => m Pos
getTabWidth = stateTabWidth <$> getParserState

-- | Set tab width. If the argument of the function is not a positive
-- number, 'defaultTabWidth' will be used.

setTabWidth :: MonadParsec e s m => Pos -> m ()
setTabWidth w = updateParserState $ \(State s pos tp _) ->
  State s pos tp w

-- | @setParserState st@ sets the parser state to @st@.

setParserState :: MonadParsec e s m => State s -> m ()
setParserState st = updateParserState (const st)

----------------------------------------------------------------------------
-- Debugging

-- | @dbg label p@ parser works exactly like @p@, but when it's evaluated it
-- also prints information useful for debugging. The @label@ is only used to
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
      unfold = streamTake 40
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

-- | A single piece of info to be rendered with 'dbgLog'.

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

-- | Calculate number of consumed tokens given 'State' of parser before and
-- after parsing.

streamDelta
  :: State s           -- ^ State of parser before consumption
  -> State s           -- ^ State of parser after consumption
  -> Int               -- ^ Number of consumed tokens
streamDelta s0 s1 = stateTokensProcessed s1 - stateTokensProcessed s0

-- | Extract a given number of tokens from the stream.

streamTake :: forall s. Stream s => Int -> s -> [Token s]
streamTake n s =
  case fst <$> takeN_ n s of
    Nothing -> []
    Just chunk -> chunkToTokens (Proxy :: Proxy s) chunk
