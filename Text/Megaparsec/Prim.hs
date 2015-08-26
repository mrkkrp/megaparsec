-- |
-- Module      :  Text.Megaparsec.Prim
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The primitive parser combinators.

{-# OPTIONS_HADDOCK not-home #-}

module Text.Megaparsec.Prim
  ( -- * Used data-types
    State (..)
  , Stream (..)
  , Consumed (..)
  , Reply (..)
  , Parsec
  , ParsecT
    -- * Running parser
  , runParser
  , runParserT
  , parse
  , parse'
  , parseTest
    -- * Primitive combinators
  , unexpected
  , (<?>)
  , label
  , hidden
  , try
  , lookAhead
  , notFollowedBy
  , eof
  , token
  , tokens
    -- * Parser state combinators
  , getPosition
  , setPosition
  , getInput
  , setInput
  , getParserState
  , setParserState
  , updateParserState
    -- * User state combinators
  , getState
  , setState
  , modifyState )
where

import Data.Bool (bool)
import Data.Monoid

import Control.Monad
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.Identity
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (state)
import Control.Monad.Trans
import qualified Control.Applicative as A

import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy.Char8 as BL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken

-- | This is Megaparsec state, this is parametrized over stream type @s@, and
-- user state @u@.

data State s u = State
  { stateInput :: s
  , statePos   :: !SourcePos
  , stateUser  :: !u }
  deriving (Show, Eq)

-- | An instance of @Stream s m t@ has stream type @s@, underlying monad @m@
-- and token type @t@ determined by the stream.
--
-- Some rough guidelines for a “correct” instance of Stream:
--
--     * @unfoldM uncons@ gives the @[t]@ corresponding to the stream.
--     * A @Stream@ instance is responsible for maintaining the “position
--       within the stream” in the stream state @s@. This is trivial unless
--       you are using the monad in a non-trivial way.

class (Monad m, ShowToken t) => Stream s m t | s -> t where
  uncons :: s -> m (Maybe (t, s))

instance (Monad m, ShowToken t) => Stream [t] m t where
  uncons []     = return Nothing
  uncons (t:ts) = return $ Just (t, ts)
  {-# INLINE uncons #-}

instance Monad m => Stream B.ByteString m Char where
  uncons = return . B.uncons

instance Monad m => Stream BL.ByteString m Char where
  uncons = return . BL.uncons

instance Monad m => Stream T.Text m Char where
  uncons = return . T.uncons
  {-# INLINE uncons #-}

instance Monad m => Stream TL.Text m Char where
  uncons = return . TL.uncons
  {-# INLINE uncons #-}

-- | This data structure represents an aspect of result of parser's
-- work. The two constructors have the following meaning:
--
--     * @Cosumed@ is a wrapper for result when some part of input stream
--       was consumed.
--     * @Empty@ is a wrapper for result when input stream is empty.
--
-- See also: 'Reply'.

data Consumed a
  = Consumed a
  | Empty   !a

-- | This data structure represents an aspect of result of parser's
-- work. The two constructors have the following meaning:
--
--     * @Ok@ for successfully run parser.
--     * @Error@ for failed parser.
--
-- See also 'Consumed'.

data Reply s u a
  = Ok a !(State s u)
  | Error ParseError

-- | 'Hints' represent collection of strings to be included into 'ParserError'
-- as “expected” messages when a parser fails without consuming input right
-- after successful parser that produced the hints.
--
-- For example, without hints you could get:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- parse error at line 1, column 2:
-- unexpected 'a'
-- expecting end of input
--
-- we're getting better error messages with help of hints:
--
-- >>> parseTest (many (char 'r') <* eof) "ra"
-- parse error at line 1, column 2:
-- unexpected 'a'
-- expecting 'r' or end of input

newtype Hints = Hints [[String]] deriving Monoid

-- | Convert 'ParseError' record into 'Hints'.

toHints :: ParseError -> Hints
toHints err = Hints [messageString <$> msgs]
  where msgs = filter ((== 1) . fromEnum) $ errorMessages err

-- | @withHints hs c@ makes “error” continuation @c@ use given hints @hs@.

withHints :: Hints -> (ParseError -> m b) -> ParseError -> m b
withHints (Hints xs) c = c . addHints
  where addHints err = foldr addErrorMessage err (Expected <$> concat xs)

-- | @accHints hs c@ results in “OK” continuation that will add given hints
-- @hs@ to third argument of original continuation @c@.

accHints :: Hints -> (a -> State s u -> Hints -> m b) ->
            a -> State s u -> Hints -> m b
accHints hs1 c x s hs2 = c x s (hs1 <> hs2)

-- | Replace most recent group of hints (if any) with given string. Used in
-- 'label' combinator.

refreshLastHint :: Hints -> String -> Hints
refreshLastHint (Hints [])     _  = Hints []
refreshLastHint (Hints (_:xs)) "" = Hints xs
refreshLastHint (Hints (_:xs)) l  = Hints ([l]:xs)

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

-- | @Parsec@ is non-transformer variant of more general @ParsecT@
-- monad-transformer.

type Parsec s u = ParsecT s u Identity

-- | @ParsecT s u m a@ is a parser with stream type @s@, user state type @u@,
-- underlying monad @m@ and return type @a@. Parsec is strict in the user
-- state. If this is undesirable, simply use a data type like @data Box a =
-- Box a@ and the state type @Box YourStateType@ to add a level of
-- indirection.

newtype ParsecT s u m a = ParsecT
  { unParser :: forall b . State s u
             -> (a -> State s u -> Hints -> m b) -- consumed-OK
             -> (ParseError -> m b)              -- consumed-error
             -> (a -> State s u -> Hints -> m b) -- empty-OK
             -> (ParseError -> m b)              -- empty-error
             -> m b }

instance Functor (ParsecT s u m) where
  fmap = parsecMap

parsecMap :: (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap f p = ParsecT $ \s cok cerr eok eerr ->
  unParser p s (cok . f) cerr (eok . f) eerr

instance A.Applicative (ParsecT s u m) where
  pure     = return
  (<*>)    = ap
  (*>)     = (>>)
  p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

instance A.Alternative (ParsecT s u m) where
  empty  = mzero
  (<|>)  = mplus
  many p = reverse <$> manyAcc p

manyAcc :: ParsecT s u m a -> ParsecT s u m [a]
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
manyErr = error
    "Text.Megaparsec.Prim.many: combinator 'many' is applied to a parser \
    \that accepts an empty string."

instance Monad (ParsecT s u m) where
  return = parserReturn
  (>>=)  = parserBind
  fail   = parserFail

parserReturn :: a -> ParsecT s u m a
parserReturn x = ParsecT $ \s _ _ eok _ -> eok x s mempty

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
{-# INLINE parserBind #-}
parserBind m k = ParsecT $ \s cok cerr eok eerr ->
  let mcok x s' hs = unParser (k x) s' cok cerr
                     (accHints hs cok) (withHints hs cerr)
      meok x s' hs = unParser (k x) s' cok cerr
                     (accHints hs eok) (withHints hs eerr)
  in unParser m s mcok cerr meok eerr

parserFail :: String -> ParsecT s u m a
parserFail msg = ParsecT $ \s _ _ _ eerr ->
  eerr $ newErrorMessage (Message msg) (statePos s)

-- | Low-level unpacking of the ParsecT type. To actually run parser see
-- 'runParserT' and 'runParser'.

runParsecT :: Monad m =>
              ParsecT s u m a -> State s u -> m (Consumed (m (Reply s u a)))
runParsecT p s = unParser p s cok cerr eok eerr
  where cok a s' _ = return . Consumed . return $ Ok a s'
        cerr err   = return . Consumed . return $ Error err
        eok a s' _ = return . Empty    . return $ Ok a s'
        eerr err   = return . Empty    . return $ Error err

-- | Low-level creation of the ParsecT type. You really shouldn't have to do
-- this.

mkPT :: Monad m =>
        (State s u -> m (Consumed (m (Reply s u a)))) -> ParsecT s u m a
mkPT k = ParsecT $ \s cok cerr eok eerr -> do
  cons <- k s
  case cons of
    Consumed mrep -> do
      rep <- mrep
      case rep of
        Ok x s'   -> cok x s' mempty
        Error err -> cerr err
    Empty mrep -> do
      rep <- mrep
      case rep of
        Ok x s'   -> eok x s' mempty
        Error err -> eerr err

instance MonadIO m => MonadIO (ParsecT s u m) where
  liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (ParsecT s u m) where
  ask       = lift ask
  local f p = mkPT $ \s -> local f (runParsecT p s)

instance MonadState s m => MonadState s (ParsecT s' u m) where
  get = lift get
  put = lift . put

instance MonadCont m => MonadCont (ParsecT s u m) where
  callCC f = mkPT $ \s ->
        callCC $ \c ->
        runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s
    where pack s a = Empty $ return (Ok a s)

instance MonadError e m => MonadError e (ParsecT s u m) where
  throwError = lift . throwError
  p `catchError` h = mkPT $ \s ->
      runParsecT p s `catchError` \e ->
          runParsecT (h e) s

instance MonadPlus (ParsecT s u m) where
  mzero = parserZero
  mplus = parserPlus

parserZero :: ParsecT s u m a
parserZero = ParsecT $ \(State _ pos _) _ _ _ eerr ->
  eerr $ newErrorUnknown pos

parserPlus :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
{-# INLINE parserPlus #-}
parserPlus m n = ParsecT $ \s cok cerr eok eerr ->
  let meerr err =
        let ncerr   err' = cerr (mergeError err' err)
            neok x s' hs = eok x s' (toHints err <> hs)
            neerr   err' = eerr (mergeError err' err)
        in unParser n s cok ncerr neok neerr
  in unParser m s cok cerr eok meerr

instance MonadTrans (ParsecT s u) where
  lift amb = ParsecT $ \s _ _ eok _ -> amb >>= \a -> eok a s mempty

-- Running a parser

-- | The most general way to run a parser over the identity monad.
-- @runParser p state filePath input@ runs parser @p@ on the input list of
-- tokens @input@, obtained from source @filePath@ with the initial user
-- state @st@.  The @filePath@ is only used in error messages and may be the
-- empty string. Returns either a 'ParseError' ('Left') or a value of type
-- @a@ ('Right').
--
-- > parseFromFile p fname = runParser p () fname <$> readFile fname

runParser :: Stream s Identity t =>
             Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParser p u name s = runIdentity $ runParserT p u name s

-- | The most general way to run a parser. @runParserT p state filePath
-- input@ runs parser @p@ on the input list of tokens @input@, obtained from
-- source @filePath@ with the initial user state @st@. The @filePath@ is
-- only used in error messages and may be the empty string. Returns a
-- computation in the underlying monad @m@ that return either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').

runParserT :: Stream s m t =>
              ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT p u name s = do
  res <- runParsecT p (State s (initialPos name) u)
  r <- parserReply res
  case r of
    Ok x _    -> return $ Right x
    Error err -> return $ Left err
  where parserReply res =
          case res of
            Consumed r -> r
            Empty    r -> r

-- | @parse p filePath input@ runs a parser @p@ over identity without user
-- state. The @filePath@ is only used in error messages and may be the empty
-- string. Returns either a 'ParseError' ('Left') or a value of type @a@
-- ('Right').
--
-- > main = case (parse numbers "" "11, 2, 43") of
-- >          Left err -> print err
-- >          Right xs -> print (sum xs)
-- >
-- > numbers = commaSep integer

parse :: Stream s Identity t =>
         Parsec s () a -> SourceName -> s -> Either ParseError a
parse p = runParser p ()

-- | @parse' p input@ runs parser @p@ on @input@ and returns result
-- inside 'Just' on success and 'Nothing' on failure. This function also
-- parses 'eof', so all input should be consumed by the parser @p@.
--
-- The function is supposed to be useful for lightweight parsing, where
-- error messages (and thus file name) are not important and entire input
-- should be parsed. For example it can be used when parsing of single
-- number according to specification of its format is desired.

parse' :: Stream s Identity t => Parsec s () a -> s -> Maybe a
parse' p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

-- | The expression @parseTest p input@ applies a parser @p@ against
-- input @input@ and prints the result to stdout. Used for testing.

parseTest :: (Stream s Identity t, Show a) => Parsec s () a -> s -> IO ()
parseTest p input =
  case parse p "" input of
    Left err -> putStr "parse error at " >> print err
    Right x  -> print x

-- Primitive combinators

-- | The parser @unexpected msg@ always fails with an unexpected error
-- message @msg@ without consuming any input.
--
-- The parsers 'fail', ('<?>') and @unexpected@ are the three parsers used
-- to generate error messages. Of these, only ('<?>') is commonly used.

unexpected :: Stream s m t => String -> ParsecT s u m a
unexpected msg = ParsecT $ \(State _ pos _) _ _ _ eerr ->
  eerr $ newErrorMessage (Unexpected msg) pos

infix 0 <?>

-- | The parser @p \<?> msg@ behaves as parser @p@, but whenever the
-- parser @p@ fails /without consuming any input/, it replaces expect error
-- messages with the expect error message @msg@.
--
-- This is normally used at the end of a set alternatives where we want to
-- return an error message in terms of a higher level construct rather than
-- returning all possible characters. For example, if the @expr@ parser from
-- the “try” example would fail, the error message is: “…: expecting
-- expression”. Without the @(\<?>)@ combinator, the message would be like
-- “…: expecting \"let\" or letter”, which is less friendly.

(<?>) :: ParsecT s u m a -> String -> ParsecT s u m a
(<?>) = flip label

-- | A synonym for @(\<?>)@, but as a function instead of an operator.

label :: String -> ParsecT s u m a -> ParsecT s u m a
label l p = ParsecT $ \s cok cerr eok eerr ->
  let cok' x s' hs = cok x s' $ refreshLastHint hs l
      eok' x s' hs = eok x s' $ refreshLastHint hs l
      eerr'    err = eerr $ setErrorMessage (Expected l) err
  in unParser p s cok' cerr eok' eerr'

-- | @hidden p@ behaves just like parser @p@, but it doesn't show any “expected”
-- tokens in error message when @p@ fails.

hidden :: ParsecT s u m a -> ParsecT s u m a
hidden = label ""

-- | The parser @try p@ behaves like parser @p@, except that it
-- pretends that it hasn't consumed any input when an error occurs.
--
-- This combinator is used whenever arbitrary look ahead is needed. Since it
-- pretends that it hasn't consumed any input when @p@ fails, the ('A.<|>')
-- combinator will try its second alternative even when the first parser
-- failed while consuming input.
--
-- For example, here is a parser that will /try/ (sorry for the pun) to
-- parse word “let” or “lexical”:
--
-- >>> parseTest (string "let" <|> string "lexical") "lexical"
-- parse error at line 1, column 1:
-- unexpected "lex"
-- expecting "let"
--
-- First parser consumed “le” and failed, @string "lexical"@ couldn't
-- succeed with “xical” as its input! Things get much better with help of
-- @try@:
--
-- >>> parseTest (try (string "let") <|> string "lexical") "lexical"
-- "lexical"
--
-- @try@ also improves error messages in case of overlapping alternatives,
-- because Megparsec's hint system can be used:
--
-- >>> parseTest (try (string "let") <|> string "lexical") "le"
-- parse error at line 1, column 1:
-- unexpected "le"
-- expecting "let" or "lexical"

try :: ParsecT s u m a -> ParsecT s u m a
try p = ParsecT $ \s cok _ eok eerr -> unParser p s cok eerr eok eerr

-- | @lookAhead p@ parses @p@ without consuming any input.
--
-- If @p@ fails and consumes some input, so does @lookAhead@. Combine with
-- 'try' if this is undesirable.

lookAhead :: Stream s m t => ParsecT s u m a -> ParsecT s u m a
lookAhead p = ParsecT $ \s _ cerr eok eerr ->
  let eok' a _ _ = eok a s mempty
  in unParser p s eok' cerr eok' eerr

-- | @notFollowedBy p@ only succeeds when parser @p@ fails. This parser
-- does not consume any input and can be used to implement the “longest
-- match” rule.

notFollowedBy :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
notFollowedBy p = ParsecT $ \s@(State input pos _) _ _ eok eerr -> do
  l <- maybe eoi (showToken . fst) <$> uncons input
  let cok' _ _ _ = eerr $ unexpectedErr l pos
      cerr'    _ = eok () s mempty
      eok' _ _ _ = eerr $ unexpectedErr l pos
      eerr'    _ = eok () s mempty
  unParser p s cok' cerr' eok' eerr'

-- | This parser only succeeds at the end of the input.

eof :: Stream s m t => ParsecT s u m ()
eof = eof' <?> eoi

eof' :: Stream s m t => ParsecT s u m ()
eof' = ParsecT $ \s@(State input pos _) _ _ eok eerr -> do
  r <- uncons input
  case r of
    Nothing    -> eok () s mempty
    Just (x,_) -> eerr $ unexpectedErr (showToken x) pos

-- | The parser @token nextPos testTok@ accepts a token @t@ with result
-- @x@ when the function @testTok t@ returns @'Just' x@. The position of the
-- /next/ token should be returned when @nextPos@ is called with the current
-- source position @pos@, the current token @t@ and the rest of the tokens
-- @toks@, @nextPos pos t toks@.
--
-- This is the most primitive combinator for accepting tokens. For example,
-- the 'Text.Megaparsec.Char.char' parser could be implemented as:
--
-- > char c = token nextPos testChar
-- >   where testChar x       = if x == c then Just x else Nothing
-- >         nextPos pos x xs = updatePosChar pos x

token :: Stream s m t =>
         (SourcePos -> t -> s -> SourcePos) -- ^ Next position calculating function.
      -> (t -> Maybe a) -- ^ Matching function for the token to parse.
      -> ParsecT s u m a
{-# INLINE token #-}
token nextpos test = ParsecT $ \(State input pos u) cok _ _ eerr -> do
    r <- uncons input
    case r of
      Nothing     -> eerr $ unexpectedErr eoi pos
      Just (c,cs) ->
        case test c of
          Just x -> let newpos = nextpos pos c cs
                        newstate = State cs newpos u
                    in seq newpos $ seq newstate $ cok x newstate mempty
          Nothing -> eerr $ unexpectedErr (showToken c) pos

-- | The parser @tokens posFromTok@ parses list of tokens and returns
-- it. The resulting parser will use 'showToken' to pretty-print the
-- collection of tokens.
--
-- This can be used to example to write 'Text.Megaparsec.Char.string':
--
-- > string = tokens updatePosString

tokens :: (Stream s m t, Eq t, ShowToken [t]) =>
          (SourcePos -> [t] -> SourcePos) -- ^ Computes position of tokens.
       -> [t]                             -- ^ List of tokens to parse
       -> ParsecT s u m [t]
{-# INLINE tokens #-}
tokens _ [] = ParsecT $ \s _ _ eok _ -> eok [] s mempty
tokens nextposs tts = ParsecT $ \(State input pos u) cok cerr _ eerr ->
  let errExpect x = setErrorMessage (Expected $ showToken tts)
                    (newErrorMessage (Unexpected x) pos)
      walk [] _ rs = let pos' = nextposs pos tts
                         s'   = State rs pos' u
                     in cok tts s' mempty
      walk (t:ts) i rs = do
        sr <- uncons rs
        let errorCont = if i == 0 then eerr else cerr
            what = bool (showToken $ take i tts) "end of input" (i == 0)
        case sr of
          Nothing -> errorCont . errExpect $ what
          Just (x,xs)
              | t == x    -> walk ts (succ i) xs
              | otherwise -> errorCont . errExpect . showToken $
                             take i tts ++ [x]
  in walk tts 0 input

unexpectedErr :: String -> SourcePos -> ParseError
unexpectedErr msg = newErrorMessage (Unexpected msg)

eoi :: String
eoi = "end of input"

-- Parser state combinators

-- | Returns the current source position. See also 'SourcePos'.

getPosition :: Monad m => ParsecT s u m SourcePos
getPosition = statePos <$> getParserState

-- | @setPosition pos@ sets the current source position to @pos@.

setPosition :: Monad m => SourcePos -> ParsecT s u m ()
setPosition pos = updateParserState (\(State s _ u) -> State s pos u)

-- | Returns the current input.

getInput :: Monad m => ParsecT s u m s
getInput = stateInput <$> getParserState

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- @setInput@ functions can for example be used to deal with #include files.

setInput :: Monad m => s -> ParsecT s u m ()
setInput s = updateParserState (\(State _ pos u) -> State s pos u)

-- | Returns the full parser state as a 'State' record.

getParserState :: Monad m => ParsecT s u m (State s u)
getParserState = ParsecT $ \s _ _ eok _ -> eok s s mempty

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: Monad m => State s u -> ParsecT s u m ()
setParserState st = updateParserState (const st)

-- | @updateParserState f@ applies function @f@ to the parser state.

updateParserState :: (State s u -> State s u) -> ParsecT s u m ()
updateParserState f = ParsecT $ \s _ _ eok _ -> eok () (f s) mempty

-- User state combinators

-- | Returns the current user state.

getState :: Monad m => ParsecT s u m u
getState = stateUser <$> getParserState

-- | @setState st@ set the user state to @st@.

setState :: Monad m => u -> ParsecT s u m ()
setState u = updateParserState (\s -> s { stateUser = u })

-- | @modifyState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- > expr = Id <$> identifier <* modifyState (+1)

modifyState :: Monad m => (u -> u) -> ParsecT s u m ()
modifyState f = updateParserState (\s -> s { stateUser = f (stateUser s)})
