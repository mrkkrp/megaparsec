-- |
-- Module      :  Text.Megaparsec.Prim
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (MPTC with FD)
--
-- The primitive parser combinators.

{-# OPTIONS_HADDOCK not-home #-}

module Text.Megaparsec.Prim
  ( -- * Used data-types
    State (..)
  , Stream (..)
  , StorableStream (..)
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
  , parseFromFile )
where

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (state)
import Control.Monad.Trans
import Control.Monad.Trans.Identity
import Data.Monoid
import qualified Control.Applicative as A
import qualified Control.Monad.Trans.Reader as L
import qualified Control.Monad.Trans.State.Lazy as L
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Writer.Lazy as L
import qualified Control.Monad.Trans.Writer.Strict as S

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), pure)
#endif

-- | This is Megaparsec state, it's parametrized over stream type @s@.

data State s = State
  { stateInput    :: s
  , statePos      :: !SourcePos
  , stateTabWidth :: !Int }
  deriving (Show, Eq)

-- | All information available after parsing. This includes consumption of
-- input, success (with return value) or failure (with parse error), parser
-- state at the end of parsing.
--
-- See also: 'Consumption', 'Result'.

data Reply s a = Reply !(State s) Consumption (Result a)

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

data Result a
  = OK a             -- ^ Parser succeeded
  | Error ParseError -- ^ Parser failed

-- | 'Hints' represent collection of strings to be included into 'ParserError'
-- as “expected” messages when a parser fails without consuming input right
-- after successful parser that produced the hints.
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

newtype Hints = Hints [[String]] deriving Monoid

-- | Convert 'ParseError' record into 'Hints'.

toHints :: ParseError -> Hints
toHints err = Hints hints
  where hints = if null msgs then [] else [messageString <$> msgs]
        msgs  = filter ((== 1) . fromEnum) $ errorMessages err

-- | @withHints hs c@ makes “error” continuation @c@ use given hints @hs@.

withHints :: Hints -> (ParseError -> m b) -> ParseError -> m b
withHints (Hints xs) c = c . addErrorMessages (Expected <$> concat xs)

-- | @accHints hs c@ results in “OK” continuation that will add given hints
-- @hs@ to third argument of original continuation @c@.

accHints :: Hints -> (a -> State s -> Hints -> m b) ->
            a -> State s -> Hints -> m b
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)

-- | Replace most recent group of hints (if any) with given string. Used in
-- 'label' combinator.

refreshLastHint :: Hints -> String -> Hints
refreshLastHint (Hints [])     _  = Hints []
refreshLastHint (Hints (_:xs)) "" = Hints xs
refreshLastHint (Hints (_:xs)) l  = Hints ([l]:xs)

-- | An instance of @Stream s t@ has stream type @s@, and token type @t@
-- determined by the stream.

class (ShowToken t, ShowToken [t]) => Stream s t | s -> t where
  uncons :: s -> Maybe (t, s)

instance (ShowToken t, ShowToken [t]) => Stream [t] t where
  uncons []     = Nothing
  uncons (t:ts) = Just (t, ts)
  {-# INLINE uncons #-}

instance Stream B.ByteString Char where
  uncons = B.uncons
  {-# INLINE uncons #-}

instance Stream BL.ByteString Char where
  uncons = BL.uncons
  {-# INLINE uncons #-}

instance Stream T.Text Char where
  uncons = T.uncons
  {-# INLINE uncons #-}

instance Stream TL.Text Char where
  uncons = TL.uncons
  {-# INLINE uncons #-}

-- | @StorableStream@ abstracts ability of some streams to be stored in a
-- file. This is used by polymorphic function 'readFromFile'.

class Stream s t => StorableStream s t where

  -- | @fromFile filename@ returns action that will try to read contents of
  -- file named @filename@.

  fromFile :: FilePath -> IO s

instance StorableStream String Char where
  fromFile = readFile

instance StorableStream B.ByteString Char where
  fromFile = B.readFile

instance StorableStream BL.ByteString Char where
  fromFile = BL.readFile

instance StorableStream T.Text Char where
  fromFile = T.readFile

instance StorableStream TL.Text Char where
  fromFile = TL.readFile

-- If you're reading this, you may be interested in how Megaparsec works on
-- lower level. That's quite simple. 'ParsecT' is a wrapper around function
-- that takes five arguments:
--
--     * State. It includes input stream, position in input stream and
--     user's backtracking state.
--
--     * “Consumed-OK” continuation (cok). This is just a function that
--     takes three arguments: result of parsing, state after parsing, and
--     hints (see their description above). This continuation is called when
--     something has been consumed during parsing and result is OK (no error
--     occurred).
--
--     * “Consumed-error” continuation (cerr). This function is called when
--     some part of input stream has been consumed and parsing resulted in
--     an error. When error happens, parsing stops and we're only interested
--     in error message, so this continuation takes 'ParseError' as its only
--     argument.
--
--     * “Empty-OK” continuation (eok). The function takes the same
--     arguments as “consumed-OK” continuation. “Empty-OK” is called when no
--     input has been consumed and no error occurred.
--
--     * “Empty-error” continuation (eerr). The function is called when no
--     input has been consumed, but nonetheless parsing resulted in an
--     error. Just like “consumed-error”, the continuation take single
--     argument — 'ParseError' record.
--
-- You call specific continuation when you want to proceed in that specific
-- branch of control flow.

-- | @Parsec@ is non-transformer variant of more general 'ParsecT'
-- monad transformer.

type Parsec s = ParsecT s Identity

-- | @ParsecT s m a@ is a parser with stream type @s@, underlying monad @m@
-- and return type @a@.

newtype ParsecT s m a = ParsecT
  { unParser :: forall b. State s
             -> (a -> State s -> Hints -> m b) -- consumed-OK
             -> (ParseError -> m b)            -- consumed-error
             -> (a -> State s -> Hints -> m b) -- empty-OK
             -> (ParseError -> m b)            -- empty-error
             -> m b }

instance Functor (ParsecT s m) where
  fmap = pMap

pMap :: (a -> b) -> ParsecT s m a -> ParsecT s m b
pMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr
{-# INLINE pMap #-}

instance A.Applicative (ParsecT s m) where
  pure     = return
  (<*>)    = ap
  (*>)     = (>>)
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

instance A.Alternative (ParsecT s m) where
  empty  = mzero
  (<|>)  = mplus
  many p = reverse <$> manyAcc p

manyAcc :: ParsecT s m a -> ParsecT s m [a]
manyAcc p = ParsecT $ \s cok cerr eok _ ->
  let errToHints c err = c (toHints err)
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
  ++ " that accepts an empty string."

instance Monad (ParsecT s m) where
  return = pReturn
  (>>=)  = pBind
  fail   = pFail

pReturn :: a -> ParsecT s m a
pReturn x = ParsecT $ \s _ _ eok _ -> eok x s mempty
{-# INLINE pReturn #-}

pBind :: ParsecT s m a -> (a -> ParsecT s m b) -> ParsecT s m b
pBind m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser (k x) s' cok cerr
                     (accHints hs cok) (withHints hs cerr)
      meok x s' hs = unParser (k x) s' cok cerr
                     (accHints hs eok) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr
{-# INLINE pBind #-}

pFail :: String -> ParsecT s m a
pFail msg = ParsecT $ \s _ _ _ eerr ->
  eerr $ newErrorMessage (Message msg) (statePos s)
{-# INLINE pFail #-}

-- | Low-level creation of the ParsecT type.

mkPT :: Monad m => (State s -> m (Reply s a)) -> ParsecT s m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
  (Reply s' consumption result) <- k s
  case consumption of
    Consumed ->
      case result of
        OK    x -> cok x s' mempty
        Error e -> cerr e
    Virgin ->
      case result of
        OK    x -> eok x s' mempty
        Error e -> eerr e

instance MonadIO m => MonadIO (ParsecT s m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (ParsecT s m) where
  ask       = lift ask
  local f p = mkPT $ \s -> local f (runParsecT p s)

instance MonadState s m => MonadState s (ParsecT s' m) where
  get = lift get
  put = lift . put

instance MonadCont m => MonadCont (ParsecT s m) where
  callCC f = mkPT $ \s ->
    callCC $ \c ->
      runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s
    where pack s a = Reply s Virgin (OK a)

instance MonadError e m => MonadError e (ParsecT s m) where
  throwError = lift . throwError
  p `catchError` h = mkPT $ \s ->
    runParsecT p s `catchError` \e ->
      runParsecT (h e) s

instance MonadPlus (ParsecT s m) where
  mzero = pZero
  mplus = pPlus

pZero :: ParsecT s m a
pZero = ParsecT $ \(State _ pos _) _ _ _ eerr -> eerr $ newErrorUnknown pos

pPlus :: ParsecT s m a -> ParsecT s m a -> ParsecT s m a
pPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err =
        let ncerr   err' = cerr (err' <> err)
            neok x s' hs = eok x s' (toHints err <> hs)
            neerr   err' = eerr (err' <> err)
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr
{-# INLINE pPlus #-}

instance MonadTrans (ParsecT s) where
  lift amb = ParsecT $ \s _ _ eok _ -> amb >>= \a -> eok a s mempty

-- Primitive combinators

-- | Type class describing parsers independent of input type.

class (A.Alternative m, Monad m, Stream s t)
      => MonadParsec s m t | m -> s t where

  -- | The most general way to stop parsing and report 'ParseError'.
  --
  -- 'unexpected' is defined in terms of the function:
  --
  -- > unexpected = failure . pure . Unexpected

  failure :: [Message] -> m a

  -- | The parser @label name p@ behaves as parser @p@, but whenever the
  -- parser @p@ fails /without consuming any input/, it replaces names of
  -- “expected” tokens with the name @name@.

  label :: String -> m a -> m a

  -- | @hidden p@ behaves just like parser @p@, but it doesn't show any
  -- “expected” tokens in error message when @p@ fails.

  hidden :: m a -> m a
  hidden = label ""

  -- | The parser @try p@ behaves like parser @p@, except that it
  -- pretends that it hasn't consumed any input when an error occurs.
  --
  -- This combinator is used whenever arbitrary look ahead is needed. Since
  -- it pretends that it hasn't consumed any input when @p@ fails, the
  -- ('A.<|>') combinator will try its second alternative even when the
  -- first parser failed while consuming input.
  --
  -- For example, here is a parser that will /try/ (sorry for the pun) to
  -- parse word “let” or “lexical”:
  --
  -- >>> parseTest (string "let" <|> string "lexical") "lexical"
  -- 1:1:
  -- unexpected "lex"
  -- expecting "let"
  --
  -- What happens here? First parser consumes “le” and fails (because it
  -- doesn't see a “t”). The second parser, however, isn't tried, since the
  -- first parser has already consumed some input! @try@ fixes this
  -- behavior and allows backtracking to work:
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

  try :: m a -> m a

  -- | @lookAhead p@ parses @p@ without consuming any input.
  --
  -- If @p@ fails and consumes some input, so does @lookAhead@. Combine with
  -- 'try' if this is undesirable.

  lookAhead :: m a -> m a

  -- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
  -- does not consume any input and can be used to implement the “longest
  -- match” rule.

  notFollowedBy :: m a -> m ()

  -- | This parser only succeeds at the end of the input.

  eof :: m ()

  -- | The parser @token nextPos testTok@ accepts a token @t@ with result
  -- @x@ when the function @testTok t@ returns @'Right' x@. The position of
  -- the /next/ token should be returned when @nextPos@ is called with the
  -- tab width, current source position, and the current token.
  --
  -- This is the most primitive combinator for accepting tokens. For
  -- example, the 'Text.Megaparsec.Char.char' parser could be implemented
  -- as:
  --
  -- > char c = token updatePosChar testChar
  -- >   where testChar x = if x == c
  -- >                      then Right x
  -- >                      else Left . pure . Unexpected . showToken $ x

  token :: (Int -> SourcePos -> t -> SourcePos) -- ^ Next position calculating function
        -> (t -> Either [Message] a) -- ^ Matching function for the token to parse
        -> m a

  -- | The parser @tokens posFromTok test@ parses list of tokens and returns
  -- it. @posFromTok@ is called with three arguments: tab width, initial
  -- position, and collection of tokens to parse. The resulting parser will
  -- use 'showToken' to pretty-print the collection of tokens in error
  -- messages. Supplied predicate @test@ is used to check equality of given
  -- and parsed tokens.
  --
  -- This can be used for example to write 'Text.Megaparsec.Char.string':
  --
  -- > string = tokens updatePosString (==)

  tokens :: Eq t
         => (Int -> SourcePos -> [t] -> SourcePos) -- ^ Computes position of tokens
         -> (t -> t -> Bool)      -- ^ Predicate to check equality of tokens
         -> [t]                   -- ^ List of tokens to parse
         -> m [t]

  -- | Returns the full parser state as a 'State' record.

  getParserState :: m (State s)

  -- | @updateParserState f@ applies function @f@ to the parser state.

  updateParserState :: (State s -> State s) -> m ()

instance Stream s t => MonadParsec s (ParsecT s m) t where
  failure           = pFailure
  label             = pLabel
  try               = pTry
  lookAhead         = pLookAhead
  notFollowedBy     = pNotFollowedBy
  eof               = pEof
  token             = pToken
  tokens            = pTokens
  getParserState    = pGetParserState
  updateParserState = pUpdateParserState

pFailure :: [Message] -> ParsecT s m a
pFailure msgs = ParsecT $ \(State _ pos _) _ _ _ eerr ->
  eerr $ newErrorMessages msgs pos

pLabel :: String -> ParsecT s m a -> ParsecT s m a
pLabel l p = ParsecT $ \s cok cerr eok eerr ->
  let l' = if null l then l else "rest of " ++ l
      cok' x s' hs = cok x s' $ refreshLastHint hs l'
      eok' x s' hs = eok x s' $ refreshLastHint hs l
      eerr'    err = eerr $ setErrorMessage (Expected l) err
  in unParser p s cok' cerr eok' eerr'

pTry :: ParsecT s m a -> ParsecT s m a
pTry p = ParsecT $ \s cok _ eok eerr -> unParser p s cok eerr eok eerr
{-# INLINE pTry #-}

pLookAhead :: ParsecT s m a -> ParsecT s m a
pLookAhead p = ParsecT $ \s _ cerr eok eerr ->
  let eok' a _ _ = eok a s mempty
  in unParser p s eok' cerr eok' eerr
{-# INLINE pLookAhead #-}

pNotFollowedBy :: Stream s t => ParsecT s m a -> ParsecT s m ()
pNotFollowedBy p = ParsecT $ \s@(State input pos _) _ _ eok eerr ->
  let l = maybe eoi (showToken . fst) (uncons input)
      cok' _ _ _ = eerr $ unexpectedErr l pos
      cerr'    _ = eok () s mempty
      eok' _ _ _ = eerr $ unexpectedErr l pos
      eerr'    _ = eok () s mempty
  in unParser p s cok' cerr' eok' eerr'

pEof :: Stream s t => ParsecT s m ()
pEof = label eoi $ ParsecT $ \s@(State input pos _) _ _ eok eerr ->
  case uncons input of
    Nothing    -> eok () s mempty
    Just (x,_) -> eerr $ unexpectedErr (showToken x) pos
{-# INLINE pEof #-}

pToken :: Stream s t
       => (Int -> SourcePos -> t -> SourcePos)
       -> (t -> Either [Message] a)
       -> ParsecT s m a
pToken nextpos test = ParsecT $ \(State input pos w) cok _ _ eerr ->
    case uncons input of
      Nothing     -> eerr $ unexpectedErr eoi pos
      Just (c,cs) ->
        case test c of
          Left ms -> eerr $ addErrorMessages ms (newErrorUnknown pos)
          Right x -> let newpos   = nextpos w pos c
                         newstate = State cs newpos w
                     in seq newpos $ seq newstate $ cok x newstate mempty
{-# INLINE pToken #-}

pTokens :: Stream s t
        => (Int -> SourcePos -> [t] -> SourcePos)
        -> (t -> t -> Bool)
        -> [t]
        -> ParsecT s m [t]
pTokens _ _ [] = ParsecT $ \s _ _ eok _ -> eok [] s mempty
pTokens nextpos test tts = ParsecT $ \(State input pos w) cok cerr _ eerr ->
  let errExpect x = setErrorMessage (Expected $ showToken tts)
                    (newErrorMessage (Unexpected x) pos)
      walk [] is rs = let pos' = nextpos w pos tts
                          s'   = State rs pos' w
                      in cok (reverse is) s' mempty
      walk (t:ts) is rs =
        let errorCont = if null is then eerr else cerr
            what      = if null is then eoi  else showToken $ reverse is
        in case uncons rs of
             Nothing -> errorCont . errExpect $ what
             Just (x,xs)
               | test t x  -> walk ts (x:is) xs
               | otherwise -> errorCont . errExpect . showToken $ reverse (x:is)
  in walk tts [] input
{-# INLINE pTokens #-}

pGetParserState :: ParsecT s m (State s)
pGetParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty
{-# INLINE pGetParserState #-}

pUpdateParserState :: (State s -> State s) -> ParsecT s m ()
pUpdateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty
{-# INLINE pUpdateParserState #-}

-- | A synonym for 'label' in form of an operator.

infix 0 <?>

(<?>) :: MonadParsec s m t => m a -> String -> m a
(<?>) = flip label

-- | The parser @unexpected msg@ always fails with an unexpected error
-- message @msg@ without consuming any input.
--
-- The parsers 'fail', 'label' and 'unexpected' are the three parsers used
-- to generate error messages. Of these, only 'label' is commonly used.

unexpected :: MonadParsec s m t => String -> m a
unexpected = failure . pure . Unexpected

unexpectedErr :: String -> SourcePos -> ParseError
unexpectedErr msg = newErrorMessage (Unexpected msg)

eoi :: String
eoi = "end of input"

-- Parser state combinators

-- | Returns the current input.

getInput :: MonadParsec s m t => m s
getInput = stateInput <$> getParserState

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- @setInput@ functions can for example be used to deal with #include files.

setInput :: MonadParsec s m t => s -> m ()
setInput s = updateParserState (\(State _ pos w) -> State s pos w)

-- | Returns the current source position.
--
-- See also: 'SourcePos'.

getPosition :: MonadParsec s m t => m SourcePos
getPosition = statePos <$> getParserState

-- | @setPosition pos@ sets the current source position to @pos@.

setPosition :: MonadParsec s m t => SourcePos -> m ()
setPosition pos = updateParserState (\(State s _ w) -> State s pos w)

-- | Returns tab width. Default tab width is equal to 'defaultTabWidth'. You
-- can set different tab width with help of 'setTabWidth'.

getTabWidth :: MonadParsec s m t => m Int
getTabWidth = stateTabWidth <$> getParserState

-- | Set tab width. If argument of the function is not positive number,
-- 'defaultTabWidth' will be used.

setTabWidth :: MonadParsec s m t => Int -> m ()
setTabWidth w = updateParserState (\(State s pos _) -> State s pos w)

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: MonadParsec s m t => State s -> m ()
setParserState st = updateParserState (const st)

-- Running a parser

-- | @parse p file input@ runs parser @p@ over 'Identity' (see 'runParserT'
-- if you're using the 'ParserT' monad transformer; 'parse' itself is just a
-- synonym for 'runParser'). It returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right'). 'show' or 'print' can be used to turn
-- 'ParseError' into the string representation of the error message. See
-- "Text.Megaparsec.Error" if you need to do more advanced error analysis.
--
-- > main = case (parse numbers "" "11, 2, 43") of
-- >          Left err -> print err
-- >          Right xs -> print (sum xs)
-- >
-- > numbers = commaSep integer

parse :: Stream s t
      => Parsec s a -- ^ Parser to run
      -> String     -- ^ Name of source file
      -> s          -- ^ Input for parser
      -> Either ParseError a
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

parseMaybe :: Stream s t => Parsec s a -> s -> Maybe a
parseMaybe p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

-- | The expression @parseTest p input@ applies a parser @p@ against
-- input @input@ and prints the result to stdout. Used for testing.

parseTest :: (Stream s t, Show a) => Parsec s a -> s -> IO ()
parseTest p input =
  case parse p "" input of
    Left  e -> print e
    Right x -> print x

-- | @runParser p file input@ runs parser @p@ on the input list of tokens
-- @input@, obtained from source @file@. The @file@ is only used in error
-- messages and may be the empty string. Returns either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').
--
-- > parseFromFile p file = runParser p file <$> readFile file

runParser :: Stream s t
          => Parsec s a -- ^ Parser to run
          -> String     -- ^ Name of source file
          -> s          -- ^ Input for parser
          -> Either ParseError a
runParser p name s = snd $ runParser' p (initialState name s)

-- | The function is similar to 'runParser' with the difference that it
-- accepts and returns parser state. This allows to specify arbitrary
-- textual position at the beginning of parsing, for example. This is the
-- most general way to run a parser over the 'Identity' monad.

runParser' :: Stream s t
           => Parsec s a -- ^ Parser to run
           -> State s    -- ^ Initial state
           -> (State s, Either ParseError a)
runParser' p = runIdentity . runParserT' p

-- | @runParserT p file input@ runs parser @p@ on the input list of tokens
-- @input@, obtained from source @file@. The @file@ is only used in error
-- messages and may be the empty string. Returns a computation in the
-- underlying monad @m@ that returns either a 'ParseError' ('Left') or a
-- value of type @a@ ('Right').

runParserT :: (Monad m, Stream s t)
           => ParsecT s m a -> String -> s -> m (Either ParseError a)
runParserT p name s = snd `liftM` runParserT' p (initialState name s)

-- | This function is similar to 'runParserT', but like 'runParser'' it
-- accepts and returns parser state. This is thus the most general way to
-- run a parser.

runParserT' :: (Monad m, Stream s t)
            => ParsecT s m a -- ^ Parser to run
            -> State s       -- ^ Initial state
            -> m (State s, Either ParseError a)
runParserT' p s = do
  (Reply s' _ result) <- runParsecT p s
  case result of
    OK    x -> return (s', Right x)
    Error e -> return (s', Left  e)

-- | Given name of source file and input construct initial state for parser.

initialState :: Stream s t => String -> s -> State s
initialState name s = State s (initialPos name) defaultTabWidth

-- | Low-level unpacking of the 'ParsecT' type. 'runParserT' and 'runParser'
-- are built upon this.

runParsecT :: Monad m
           => ParsecT s m a -- ^ Parser to run
           -> State s       -- ^ Initial state
           -> m (Reply s a)
runParsecT p s = unParser p s cok cerr eok eerr
  where cok a s' _ = return $ Reply s' Consumed (OK a)
        cerr err   = return $ Reply s  Consumed (Error err)
        eok a s' _ = return $ Reply s' Virgin   (OK a)
        eerr err   = return $ Reply s  Virgin   (Error err)

-- | @parseFromFile p filename@ runs parser @p@ on the input read from
-- @filename@. Returns either a 'ParseError' ('Left') or a value of type @a@
-- ('Right').
--
-- > main = do
-- >   result <- parseFromFile numbers "digits.txt"
-- >   case result of
-- >     Left err -> print err
-- >     Right xs -> print $ sum xs

parseFromFile :: StorableStream s t
              => Parsec s a -- ^ Parser to run
              -> FilePath   -- ^ Name of file to parse
              -> IO (Either ParseError a)
parseFromFile p filename = runParser p filename <$> fromFile filename

-- Instances of 'MonadParsec'

instance (MonadPlus m, MonadParsec s m t) =>
         MonadParsec s (L.StateT e m) t where
  label n       (L.StateT m) = L.StateT $ label n . m
  try           (L.StateT m) = L.StateT $ try . m
  lookAhead     (L.StateT m) = L.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (L.StateT m) = L.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  failure                    = lift . failure
  eof                        = lift eof
  token  f e                 = lift $ token  f e
  tokens f e ts              = lift $ tokens f e ts
  getParserState             = lift getParserState
  updateParserState f        = lift $ updateParserState f

instance (MonadPlus m, MonadParsec s m t)
         => MonadParsec s (S.StateT e m) t where
  label n       (S.StateT m) = S.StateT $ label n . m
  try           (S.StateT m) = S.StateT $ try . m
  lookAhead     (S.StateT m) = S.StateT $ \s ->
    (,s) . fst <$> lookAhead (m s)
  notFollowedBy (S.StateT m) = S.StateT $ \s ->
    notFollowedBy (fst <$> m s) >> return ((),s)
  failure                    = lift . failure
  eof                        = lift eof
  token  f e                 = lift $ token  f e
  tokens f e ts              = lift $ tokens f e ts
  getParserState             = lift getParserState
  updateParserState f        = lift $ updateParserState f

instance (MonadPlus m, MonadParsec s m t)
         => MonadParsec s (L.ReaderT e m) t where
  label n       (L.ReaderT m) = L.ReaderT $ label n . m
  try           (L.ReaderT m) = L.ReaderT $ try . m
  lookAhead     (L.ReaderT m) = L.ReaderT $ lookAhead . m
  notFollowedBy (L.ReaderT m) = L.ReaderT $ notFollowedBy . m
  failure                     = lift . failure
  eof                         = lift eof
  token  f e                  = lift $ token  f e
  tokens f e ts               = lift $ tokens f e ts
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f

instance (MonadPlus m, Monoid w, MonadParsec s m t)
         => MonadParsec s (L.WriterT w m) t where
  label n       (L.WriterT m) = L.WriterT $ label n m
  try           (L.WriterT m) = L.WriterT $ try m
  lookAhead     (L.WriterT m) = L.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (L.WriterT m) = L.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  failure                     = lift . failure
  eof                         = lift eof
  token  f e                  = lift $ token  f e
  tokens f e ts               = lift $ tokens f e ts
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f

instance (MonadPlus m, Monoid w, MonadParsec s m t)
         => MonadParsec s (S.WriterT w m) t where
  label n       (S.WriterT m) = S.WriterT $ label n m
  try           (S.WriterT m) = S.WriterT $ try m
  lookAhead     (S.WriterT m) = S.WriterT $
    (,mempty) . fst <$> lookAhead m
  notFollowedBy (S.WriterT m) = S.WriterT $
    (,mempty) <$> notFollowedBy (fst <$> m)
  failure                     = lift . failure
  eof                         = lift eof
  token  f e                  = lift $ token  f e
  tokens f e ts               = lift $ tokens f e ts
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f

instance (Monad m, MonadParsec s m t)
         => MonadParsec s (IdentityT m) t where
  label n       (IdentityT m) = IdentityT $ label n m
  try                         = IdentityT . try . runIdentityT
  lookAhead     (IdentityT m) = IdentityT $ lookAhead m
  notFollowedBy (IdentityT m) = IdentityT $ notFollowedBy m
  failure                     = lift . failure
  eof                         = lift eof
  token  f e                  = lift $ token  f e
  tokens f e ts               = lift $ tokens f e ts
  getParserState              = lift getParserState
  updateParserState f         = lift $ updateParserState f
