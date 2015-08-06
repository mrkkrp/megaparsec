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
    ( State (..)
    , Stream (..)
    , Consumed (..)
    , Reply (..)
    , ParsecT
    , Parsec
    , runParsecT
    , mkPT
    , unknownError
    , sysUnExpectError
    , unexpected
    , mergeErrorReply
    , (<?>)
    , label
    , runParserT
    , runParser
    , parse
    , parseMaybe
    , parseTest
    , try
    , lookAhead
    , token
    , tokens
    , tokenPrim
    , skipMany
    , getPosition
    , getInput
    , setPosition
    , setInput
    , getParserState
    , setParserState
    , updateParserState
    , getState
    , putState
    , modifyState )
where

import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as CL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

import Control.Monad
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Reader.Class
import Control.Monad.State.Class hiding (state)
import Control.Monad.Cont.Class
import Control.Monad.Error.Class

import qualified Control.Applicative as A

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.ShowToken

-- | This is Parsec state, this is parametrized over stream type @s@, and
-- user state @u@.

data State s u = State
    { stateInput :: s
    , statePos   :: !SourcePos
    , stateUser  :: !u }

-- | An instance of @Stream s m t@ has stream type @s@, underlying monad @m@
-- and token type @t@ determined by the stream.
--
-- Some rough guidelines for a \"correct\" instance of Stream:
--
--    * unfoldM uncons gives the [t] corresponding to the stream
--
--    * A @Stream@ instance is responsible for maintaining the \"position
--      within the stream\" in the stream state @s@.  This is trivial unless
--      you are using the monad in a non-trivial way.

class (Monad m, ShowToken t) => Stream s m t | s -> t where
    uncons :: s -> m (Maybe (t, s))

instance (Monad m, ShowToken t) => Stream [t] m t where
    uncons []     = return Nothing
    uncons (t:ts) = return $ Just (t, ts)
    {-# INLINE uncons #-}

instance Monad m => Stream CL.ByteString m Char where
    uncons = return . CL.uncons

instance Monad m => Stream C.ByteString m Char where
    uncons = return . C.uncons

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
--     was consumed.
--
--     * @Empty@ is a wrapper for result when input stream is empty.
--
-- You shouldn't really need to know this. See also: 'Reply'.

data Consumed a = Consumed a
                | Empty !a

instance Functor Consumed where
    fmap f (Consumed x) = Consumed (f x)
    fmap f (Empty x)    = Empty (f x)

-- | This data structure represents an aspect of result of parser's
-- work. The two constructors have the following meaning:
--
--     * @Ok@ for successfully run parser.
--     * @Error@ for failed parser.
--
-- You shouldn't really need to know this. See also 'Consumed'.

data Reply s u a = Ok a !(State s u) ParseError
                 | Error ParseError

instance Functor (Reply s u) where
    fmap f (Ok x s e) = Ok (f x) s e
    fmap _ (Error e)  = Error e

-- | @ParsecT s u m a@ is a parser with stream type @s@, user state type @u@,
-- underlying monad @m@ and return type @a@. Parsec is strict in the user
-- state. If this is undesirable, simply use a data type like @data Box a =
-- Box a@ and the state type @Box YourStateType@ to add a level of
-- indirection.

newtype ParsecT s u m a = ParsecT
    { unParser :: forall b . State s u
               -> (a -> State s u -> ParseError -> m b) -- consumed ok
               -> (ParseError -> m b)                   -- consumed err
               -> (a -> State s u -> ParseError -> m b) -- empty ok
               -> (ParseError -> m b)                   -- empty err
               -> m b }

-- | @Parsec@ is non-transformer variant of more general @ParsecT@
-- monad-transformer.

type Parsec s u = ParsecT s u Identity

instance Functor (ParsecT s u m) where
    fmap = parsecMap

parsecMap :: (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap f p = ParsecT $ \s cok cerr eok eerr ->
                unParser p s (cok . f) cerr (eok . f) eerr

instance A.Applicative (ParsecT s u m) where
    pure     = return
    (<*>)    = ap -- TODO: Can this be optimized?
    (*>)     = (>>)
    p1 <* p2 = do { x1 <- p1 ; void p2 ; return x1 }

instance A.Alternative (ParsecT s u m) where
    empty  = mzero
    (<|>)  = mplus
    many p = reverse <$> manyAccum (:) p

instance Monad (ParsecT s u m) where
    return = parserReturn
    (>>=)  = parserBind
    fail   = parserFail

parserReturn :: a -> ParsecT s u m a
parserReturn x = ParsecT $ \s _ _ eok _ -> eok x s (unknownError s)

parserBind :: ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b
{-# INLINE parserBind #-}
parserBind m k = ParsecT $ \s cok cerr eok eerr ->
    let
        -- consumed-okay case for m
        mcok x st err =
            let
                -- if (k x) consumes, those go straight up
                pcok  = cok
                pcerr = cerr
                -- if (k x) doesn't consume input, but is okay, we still
                -- return in the consumed continuation
                peok x' s' err' = cok x' s' (mergeError err err')
                -- if (k x) doesn't consume input, but errors, we return the
                -- error in the 'consumed-error' continuation
                peerr err' = cerr (mergeError err err')
            in  unParser (k x) st pcok pcerr peok peerr

        -- empty-ok case for m
        meok x st err =
            let
                -- in these cases, (k x) can return as empty
                pcok = cok
                peok x' s' err' = eok x' s' (mergeError err err')
                pcerr = cerr
                peerr err' = eerr (mergeError err err')
            in  unParser (k x) st pcok pcerr peok peerr

        -- consumed-error case for m
        mcerr = cerr

        -- empty-error case for m
        meerr = eerr

    in unParser m s mcok mcerr meok meerr

parserFail :: String -> ParsecT s u m a
parserFail msg = ParsecT $ \s _ _ _ eerr ->
                 eerr $ newErrorMessage (Message msg) (statePos s)

-- | Low-level unpacking of the ParsecT type. To actually run parser see
-- 'runParserT' and 'runParser'.

runParsecT :: Monad m =>
              ParsecT s u m a -> State s u -> m (Consumed (m (Reply s u a)))
runParsecT p s = unParser p s cok cerr eok eerr
    where cok a s' err = return . Consumed . return $ Ok a s' err
          cerr     err = return . Consumed . return $ Error err
          eok a s' err = return . Empty    . return $ Ok a s' err
          eerr     err = return . Empty    . return $ Error err

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
                         Ok x s' err -> cok x s' err
                         Error   err -> cerr err
             Empty mrep -> do
                       rep <- mrep
                       case rep of
                         Ok x s' err -> eok x s' err
                         Error   err -> eerr err

instance MonadIO m => MonadIO (ParsecT s u m) where
    liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (ParsecT s u m) where
    ask       = lift ask
    local f p = mkPT $ \s -> local f (runParsecT p s)

-- I'm presuming the user might want a separate, non-backtracking state
-- aside from the Parsec user state.

instance MonadState s m => MonadState s (ParsecT s' u m) where
    get = lift get
    put = lift . put

instance MonadCont m => MonadCont (ParsecT s u m) where
    callCC f = mkPT $ \s ->
          callCC $ \c ->
          runParsecT (f (\a -> mkPT $ \s' -> c (pack s' a))) s

     where pack s a= Empty $ return (Ok a s (unknownError s))

instance MonadError e m => MonadError e (ParsecT s u m) where
    throwError = lift . throwError
    p `catchError` h = mkPT $ \s ->
        runParsecT p s `catchError` \e ->
            runParsecT (h e) s

instance MonadPlus (ParsecT s u m) where
    mzero = parserZero
    mplus = parserPlus

parserZero :: ParsecT s u m a
parserZero = ParsecT $ \s _ _ _ eerr -> eerr $ unknownError s

parserPlus :: ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
{-# INLINE parserPlus #-}
parserPlus m n
    = ParsecT $ \s cok cerr eok eerr ->
      let
          meerr err =
              let
                  neok y s' err' = eok y s' (mergeError err err')
                  neerr err' = eerr $ mergeError err err'
              in unParser n s cok cerr neok neerr
      in unParser m s cok cerr eok meerr

instance MonadTrans (ParsecT s u) where
    lift amb = ParsecT $ \s _ _ eok _ -> do
               a <- amb
               eok a s $ unknownError s

-- Errors

-- | Create new @ParseError@ object. It will contain information about
-- position at which error is happened and nothing more.

unknownError :: State s u -> ParseError
unknownError state = newErrorUnknown (statePos state)

-- | @sysUnExpectError m pos@ creates 'Reply' that represents \"unexpected\"
-- error with associated message @m@ and position @pos@.

sysUnExpectError :: String -> SourcePos -> Reply s u a
sysUnExpectError msg pos = Error (newErrorMessage (SysUnExpect msg) pos)

-- | The parser @unexpected msg@ always fails with an unexpected error
-- message @msg@ without consuming any input.
--
-- The parsers 'fail', ('<?>') and @unexpected@ are the three parsers used
-- to generate error messages. Of these, only ('<?>') is commonly used. For
-- an example of the use of @unexpected@, see the definition of
-- 'Text.Megaparsec.Combinator.notFollowedBy'.

unexpected :: Stream s m t => String -> ParsecT s u m a
unexpected msg = ParsecT $ \s _ _ _ eerr ->
      eerr $ newErrorMessage (UnExpect msg) (statePos s)

-- | @mergeErrorReply e reply@ returns @reply@ with error @e@ added.

mergeErrorReply :: ParseError -> Reply s u a -> Reply s u a
mergeErrorReply err1 reply
    = case reply of
        Ok x state err2 -> Ok x state (mergeError err1 err2)
        Error err2      -> Error (mergeError err1 err2)

-- Basic combinators

infix  0 <?>

-- | The parser @p \<?> msg@ behaves as parser @p@, but whenever the
-- parser @p@ fails /without consuming any input/, it replaces expect error
-- messages with the expect error message @msg@.
--
-- This is normally used at the end of a set alternatives where we want to
-- return an error message in terms of a higher level construct rather than
-- returning all possible characters. For example, if the @expr@ parser from
-- the 'try' example would fail, the error message is: '…: expecting
-- expression'. Without the @(\<?>)@ combinator, the message would be like
-- '…: expecting \"let\" or letter', which is less friendly.

(<?>) :: ParsecT s u m a -> String -> ParsecT s u m a
p <?> msg = label p msg

-- | A synonym for @\<?>@, but as a function instead of an operator.

label :: ParsecT s u m a -> String -> ParsecT s u m a
label p msg = labels p [msg]

labels :: ParsecT s u m a -> [String] -> ParsecT s u m a
labels p msgs = ParsecT $ \s cok cerr eok eerr ->
    let eok' x s' error' = eok x s' $ if errorIsUnknown error'
                                      then error'
                                      else setExpectErrors error' msgs
        eerr' err = eerr $ setExpectErrors err msgs
    in unParser p s cok cerr eok' eerr'
 where
   setExpectErrors err []       = addErrorMessage (Expect "") err
   setExpectErrors err [m]      = addErrorMessage (Expect m) err
   setExpectErrors err (m:ms)
       = foldr (\msg' err' -> addErrorMessage (Expect msg') err')
         (addErrorMessage (Expect m) err) ms

-- Running a parser

-- | The most general way to run a parser. @runParserT p state filePath
-- input@ runs parser @p@ on the input list of tokens @input@, obtained from
-- source @filePath@ with the initial user state @st@. The @filePath@ is
-- only used in error messages and may be the empty string. Returns a
-- computation in the underlying monad @m@ that return either a 'ParseError'
-- ('Left') or a value of type @a@ ('Right').

runParserT :: Stream s m t =>
              ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runParserT p u name s
    = do res <- runParsecT p (State s (initialPos name) u)
         r <- parserReply res
         case r of
           Ok x _ _  -> return (Right x)
           Error err -> return (Left err)
    where parserReply res
              = case res of
                  Consumed r -> r
                  Empty    r -> r

-- | The most general way to run a parser over the Identity
-- monad. @runParser p state filePath input@ runs parser @p@ on the input
-- list of tokens @input@, obtained from source @filePath@ with the initial
-- user state @st@.  The @filePath@ is only used in error messages and may
-- be the empty string. Returns either a 'ParseError' ('Left') or a value of
-- type @a@ ('Right').
--
-- > parseFromFile p fname = runParser p () fname <$> readFile fname

runParser :: Stream s Identity t =>
             Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParser p u name s = runIdentity $ runParserT p u name s

-- | @parse p filePath input@ runs a parser @p@ over Identity without user
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

-- | @parseMaybe p input@ runs parser @p@ on @input@ and returns result
-- inside @Just@ on success and @Nothing@ on failure.

parseMaybe :: Stream s Identity t => Parsec s () a -> s -> Maybe a
parseMaybe p s =
    case parse p "" s of
      Left  _ -> Nothing
      Right x -> Just x

-- | The expression @parseTest p input@ applies a parser @p@ against
-- input @input@ and prints the result to stdout. Used for testing.

parseTest :: (Stream s Identity t, Show a) => Parsec s () a -> s -> IO ()
parseTest p input =
    case parse p "" input of
      Left err -> putStr "parse error at " >> print err
      Right x  -> print x

-- | The parser @try p@ behaves like parser @p@, except that it
-- pretends that it hasn't consumed any input when an error occurs.
--
-- This combinator is used whenever arbitrary look ahead is needed. Since it
-- pretends that it hasn't consumed any input when @p@ fails, the ('<|>')
-- combinator will try its second alternative even when the first parser
-- failed while consuming input.
--
-- The @try@ combinator can for example be used to distinguish identifiers
-- and reserved words. Both reserved words and identifiers are a sequence of
-- letters. Whenever we expect a certain reserved word where we can also
-- expect an identifier we have to use the @try@ combinator. Suppose we
-- write:
--
-- > expr       = letExpr <|> identifier <?> "expression"
-- >
-- > letExpr    = string "let" >> …
-- > identifier = some letter
--
-- If the user writes \"lexical\", the parser fails with: @unexpected \'x\',
-- expecting \'t\' in \"let\"@. Indeed, since the ('<|>') combinator only
-- tries alternatives when the first alternative hasn't consumed input, the
-- @identifier@ parser is never tried (because the prefix \"le\" of the
-- @string \"let\"@ parser is already consumed). The right behaviour can be
-- obtained by adding the @try@ combinator:
--
-- > expr       = letExpr <|> identifier <?> "expression"
-- >
-- > letExpr    = try (string "let") >> …
-- > identifier = some letter

try :: ParsecT s u m a -> ParsecT s u m a
try p = ParsecT $ \s cok _ eok eerr -> unParser p s cok eerr eok eerr

-- | @lookAhead p@ parses @p@ without consuming any input.
--
-- If @p@ fails and consumes some input, so does @lookAhead@. Combine with
-- 'try' if this is undesirable.

lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead p = ParsecT $ \s _ cerr eok eerr -> do
                let eok' a _ _ = eok a s (newErrorUnknown (statePos s))
                unParser p s eok' cerr eok' eerr

-- | The parser @token posFromTok testTok@ accepts a token @t@ with result
-- @x@ when the function @testTok t@ returns @'Just' x@. The source position
-- of the @t@ should be returned by @posFromTok t@. Token will be shown with
-- 'showToken' function.
--
-- This combinator is expressed in terms of 'tokenPrim'. It is used to
-- accept user defined token streams. For example, suppose that we have a
-- stream of basic tokens tupled with source positions. We can than define a
-- parser that accepts single tokens as:
--
-- > mytoken x = token posFromTok testTok
-- >   where posFromTok (pos,t) = pos
-- >         testTok    (pos,t) = if x == t then Just t else Nothing

token :: Stream s Identity t =>
         (t -> SourcePos) -- ^ Computes the position of a token.
      -> (t -> Maybe a)   -- ^ Matching function for the token to parse.
      -> Parsec s u a
token tokpos = tokenPrim nextpos
    where nextpos _ tok ts =
              case runIdentity (uncons ts) of
                Nothing        -> tokpos tok
                Just (tok', _) -> tokpos tok'

-- | The parser @tokens posFromTok@ parses list of tokens and returns
-- it. The resulting parser will use @showToks@ to pretty-print the
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
tokens _ [] = ParsecT $ \s _ _ eok _ -> eok [] s $ unknownError s
tokens nextposs tts@(tok:toks)
    = ParsecT $ \(State input pos u) cok cerr _ eerr ->
    let
        errEof = addErrorMessage (Expect (showToken tts))
                 (newErrorMessage (SysUnExpect "") pos)
        errExpect x = addErrorMessage (Expect (showToken tts))
                      (newErrorMessage (SysUnExpect (showToken [x])) pos)

        walk []     rs = ok rs
        walk (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing -> cerr errEof
            Just (x,xs)
                | t == x    -> walk ts xs
                | otherwise -> cerr $ errExpect x

        ok rs = let pos' = nextposs pos tts
                    s' = State rs pos' u
                in cok tts s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        case sr of
            Nothing -> eerr errEof
            Just (x,xs)
                | tok == x  -> walk toks xs
                | otherwise -> eerr $ errExpect x

-- | The parser @tokenPrim nextPos testTok@ accepts a token @t@ with result
-- @x@ when the function @testTok t@ returns @'Just' x@. The position of the
-- /next/ token should be returned when @nextPos@ is called with the current
-- source position @pos@, the current token @t@ and the rest of the tokens
-- @toks@, @nextPos pos t toks@.
--
-- This is the most primitive combinator for accepting tokens. For example,
-- the 'Text.Megaparsec.Char.char' parser could be implemented as:
--
-- > char c  = tokenPrim nextPos testChar
-- >   where testChar x       = if x == c then Just x else Nothing
-- >         nextPos pos x xs = updatePosChar pos x

tokenPrim :: Stream s m t =>
             (SourcePos -> t -> s -> SourcePos) -- ^ Next position calculating function.
          -> (t -> Maybe a) -- ^ Matching function for the token to parse.
          -> ParsecT s u m a
{-# INLINE tokenPrim #-}
tokenPrim nextpos = tokenPrimEx nextpos Nothing

tokenPrimEx :: Stream s m t =>
               (SourcePos -> t -> s -> SourcePos)
            -> Maybe (SourcePos -> t -> s -> u -> u)
            -> (t -> Maybe a)
            -> ParsecT s u m a
{-# INLINE tokenPrimEx #-}

tokenPrimEx nextpos Nothing test
  = ParsecT $ \(State input pos user) cok _ _ eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newstate = State cs newpos user
                        in seq newpos $ seq newstate $
                           cok x newstate (newErrorUnknown newpos)
              Nothing -> eerr $ unexpectError (showToken c) pos

tokenPrimEx nextpos (Just nextState) test
  = ParsecT $ \(State input pos user) cok _ _ eerr -> do
      r <- uncons input
      case r of
        Nothing -> eerr $ unexpectError "" pos
        Just (c,cs)
         -> case test c of
              Just x -> let newpos = nextpos pos c cs
                            newUser = nextState pos c cs user
                            newstate = State cs newpos newUser
                        in seq newpos $ seq newstate $
                           cok x newstate $ newErrorUnknown newpos
              Nothing -> eerr $ unexpectError (showToken c) pos

unexpectError :: String -> SourcePos -> ParseError
unexpectError msg = newErrorMessage (SysUnExpect msg)

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- > spaces = skipMany space

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany p = void $ manyAccum (\_ _ -> []) p

manyAccum :: (a -> [a] -> [a]) -> ParsecT s u m a -> ParsecT s u m [a]
manyAccum acc p =
    ParsecT $ \s cok cerr eok _ ->
    let walk xs x s' _ =
            unParser p s'
              (seq xs $ walk $ acc x xs) -- consumed-ok
              cerr                       -- consumed-err
              manyErr                    -- empty-ok
              (cok (acc x xs) s')        -- empty-err
    in unParser p s (walk []) cerr manyErr (eok [] s)

manyErr :: forall t . t
manyErr =
    error
    "Text.Megaparsec.Prim.many: combinator 'many' is applied to a parser \
    \that accepts an empty string."

-- Parser state combinators

-- | Returns the current source position. See also 'SourcePos'.

getPosition :: Monad m => ParsecT s u m SourcePos
getPosition = statePos <$> getParserState

-- | Returns the current input.

getInput :: Monad m => ParsecT s u m s
getInput = stateInput <$> getParserState

-- | @setPosition pos@ sets the current source position to @pos@.

setPosition :: Monad m => SourcePos -> ParsecT s u m ()
setPosition pos = void $ updateParserState (\(State s _ u) -> State s pos u)

-- | @setInput input@ continues parsing with @input@. The 'getInput' and
-- @setInput@ functions can for example be used to deal with #include files.

setInput :: Monad m => s -> ParsecT s u m ()
setInput s = void $ updateParserState (\(State _ pos u) -> State s pos u)

-- | Returns the full parser state as a 'State' record.

getParserState :: Monad m => ParsecT s u m (State s u)
getParserState = updateParserState id

-- | @setParserState st@ set the full parser state to @st@.

setParserState :: Monad m => State s u -> ParsecT s u m (State s u)
setParserState st = updateParserState (const st)

-- | @updateParserState f@ applies function @f@ to the parser state.

updateParserState :: (State s u -> State s u) -> ParsecT s u m (State s u)
updateParserState f =
    ParsecT $ \s _ _ eok _ -> let s' = f s in eok s' s' $ unknownError s'

-- User state combinators

-- | Returns the current user state.

getState :: Monad m => ParsecT s u m u
getState = stateUser `liftM` getParserState

-- | @putState st@ set the user state to @st@.

putState :: Monad m => u -> ParsecT s u m ()
putState u = void $ updateParserState (\s -> s { stateUser = u })

-- | @modifyState f@ applies function @f@ to the user state. Suppose
-- that we want to count identifiers in a source, we could use the user
-- state as:
--
-- > expr = Id <$> identifier <* modifyState (+1)

modifyState :: Monad m => (u -> u) -> ParsecT s u m ()
modifyState f = void $ updateParserState (\s -> s { stateUser = f (stateUser s)})
