-- |
-- Module      :  Text.Megaparsec
-- Copyright   :  © 2015–present Megaparsec contributors
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
-- at the tutorials
-- <https://markkarpov.com/learn-haskell.html#megaparsec-tutorials>.
--
-- In addition to the "Text.Megaparsec" module, which exports and re-exports
-- most everything that you may need, we advise to import
-- "Text.Megaparsec.Char" if you plan to work with a stream of 'Char' tokens
-- or "Text.Megaparsec.Byte" if you intend to parse binary data.
--
-- It is common to start working with the library by defining a type synonym
-- like this:
--
-- > type Parser = Parsec Void Text
-- >                      ^    ^
-- >                      |    |
-- > Custom error component    Input stream type
--
-- Then you can write type signatures like @Parser 'Int'@—for a parser that
-- returns an 'Int' for example.
--
-- Similarly (since it's known to cause confusion), you should use
-- 'ParseErrorBundle' type parametrized like this:
--
-- > ParseErrorBundle Text Void
-- >                  ^    ^
-- >                  |    |
-- >  Input stream type    Custom error component (the same you used in Parser)
--
-- Megaparsec uses some type-level machinery to provide flexibility without
-- compromising on type safety. Thus type signatures are sometimes necessary
-- to avoid ambiguous types. If you're seeing an error message that reads
-- like “Type variable @e0@ is ambiguous …”, you need to give an explicit
-- signature to your parser to resolve the ambiguity. It's a good idea to
-- provide type signatures for all top-level definitions.

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Text.Megaparsec
  ( -- * Re-exports
    -- $reexports
    module Text.Megaparsec.Pos
  , module Text.Megaparsec.Error
  , module Text.Megaparsec.Stream
  , module Control.Monad.Combinators
    -- * Data types
  , State (..)
  , PosState (..)
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
  , single
  , satisfy
  , anySingle
  , anySingleBut
  , oneOf
  , noneOf
  , chunk
  , (<?>)
  , unexpected
  , customFailure
  , match
  , region
  , takeRest
  , atEnd
    -- * Parser state combinators
  , getInput
  , setInput
  , getSourcePos
  , getOffset
  , setOffset
  , setParserState )
where

import Control.Monad.Combinators
import Control.Monad.Identity
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromJust)
import Text.Megaparsec.Class
import Text.Megaparsec.Error
import Text.Megaparsec.Internal
import Text.Megaparsec.Pos
import Text.Megaparsec.State
import Text.Megaparsec.Stream
import qualified Data.Set as E

-- $reexports
--
-- Note that we re-export monadic combinators from
-- "Control.Monad.Combinators" because these are more efficient than
-- 'Applicative'-based ones. Thus 'many' and 'some' may clash with the
-- functions from "Control.Applicative". You need to hide the functions like
-- this:
--
-- > import Control.Applicative hiding (many, some)
--
-- Also note that you can import "Control.Monad.Combinators.NonEmpty" if you
-- wish that combinators like 'some' return 'NonEmpty' lists. The module
-- lives in the @parser-combinators@ package (you need at least version
-- /0.4.0/).
--
-- This module is intended to be imported qualified:
--
-- > import qualified Control.Monad.Combinators.NonEmpty as NE
--
-- Other modules of interest are:
--
--     * "Control.Monad.Combinators.Expr" for parsing of expressions.
--     * "Control.Applicative.Permutations" for parsing of permutations
--       phrases.

----------------------------------------------------------------------------
-- Data types

-- | 'Parsec' is a non-transformer variant of the more general 'ParsecT'
-- monad transformer.

type Parsec e s = ParsecT e s Identity

----------------------------------------------------------------------------
-- Running a parser

-- | @'parse' p file input@ runs parser @p@ over 'Identity' (see
-- 'runParserT' if you're using the 'ParsecT' monad transformer; 'parse'
-- itself is just a synonym for 'runParser'). It returns either a
-- 'ParseErrorBundle' ('Left') or a value of type @a@ ('Right').
-- 'errorBundlePretty' can be used to turn 'ParseErrorBundle' into the
-- string representation of the error message. See "Text.Megaparsec.Error"
-- if you need to do more advanced error analysis.
--
-- > main = case parse numbers "" "11,2,43" of
-- >          Left bundle -> putStr (errorBundlePretty bundle)
-- >          Right xs -> print (sum xs)
-- >
-- > numbers = decimal `sepBy` char ','

parse
  :: Parsec e s a -- ^ Parser to run
  -> String       -- ^ Name of source file
  -> s            -- ^ Input for parser
  -> Either (ParseErrorBundle s e) a
parse = runParser

-- | @'parseMaybe' p input@ runs the parser @p@ on @input@ and returns the
-- result inside 'Just' on success and 'Nothing' on failure. This function
-- also parses 'eof', so if the parser doesn't consume all of its input, it
-- will fail.
--
-- The function is supposed to be useful for lightweight parsing, where
-- error messages (and thus file names) are not important and entire input
-- should be parsed. For example, it can be used when parsing of a single
-- number according to a specification of its format is desired.

parseMaybe :: (Ord e, Stream s) => Parsec e s a -> s -> Maybe a
parseMaybe p s =
  case parse (p <* eof) "" s of
    Left  _ -> Nothing
    Right x -> Just x

-- | The expression @'parseTest' p input@ applies the parser @p@ against the
-- input @input@ and prints the result to stdout. Useful for testing.

parseTest :: ( ShowErrorComponent e
             , Show a
             , Stream s
             )
  => Parsec e s a -- ^ Parser to run
  -> s            -- ^ Input for parser
  -> IO ()
parseTest p input =
  case parse p "" input of
    Left  e -> putStr (errorBundlePretty e)
    Right x -> print x

-- | @'runParser' p file input@ runs parser @p@ on the input stream of
-- tokens @input@, obtained from source @file@. The @file@ is only used in
-- error messages and may be the empty string. Returns either a
-- 'ParseErrorBundle' ('Left') or a value of type @a@ ('Right').
--
-- > parseFromFile p file = runParser p file <$> readFile file

runParser
  :: Parsec e s a -- ^ Parser to run
  -> String     -- ^ Name of source file
  -> s          -- ^ Input for parser
  -> Either (ParseErrorBundle s e) a
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
  -> (State s, Either (ParseErrorBundle s e) a)
runParser' p = runIdentity . runParserT' p

-- | @'runParserT' p file input@ runs parser @p@ on the input list of tokens
-- @input@, obtained from source @file@. The @file@ is only used in error
-- messages and may be the empty string. Returns a computation in the
-- underlying monad @m@ that returns either a 'ParseErrorBundle' ('Left') or
-- a value of type @a@ ('Right').

runParserT :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> String        -- ^ Name of source file
  -> s             -- ^ Input for parser
  -> m (Either (ParseErrorBundle s e) a)
runParserT p name s = snd <$> runParserT' p (initialState name s)

-- | This function is similar to 'runParserT', but like 'runParser'' it
-- accepts and returns parser state. This is thus the most general way to
-- run a parser.
--
-- @since 4.2.0

runParserT' :: Monad m
  => ParsecT e s m a -- ^ Parser to run
  -> State s       -- ^ Initial state
  -> m (State s, Either (ParseErrorBundle s e) a)
runParserT' p s = do
  (Reply s' _ result) <- runParsecT p s
  return $ case result of
    OK    x -> (s', Right x)
    Error e ->
      let bundle = ParseErrorBundle
            { bundleErrors = e :| []
            , bundlePosState = statePosState s
            }
      in (s', Left bundle)

-- | Given name of source file and input construct initial state for parser.

initialState :: String -> s -> State s
initialState name s = State
  { stateInput  = s
  , stateOffset = 0
  , statePosState = PosState
    { pstateInput = s
    , pstateOffset = 0
    , pstateSourcePos = initialPos name
    , pstateTabWidth = defaultTabWidth
    , pstateLinePrefix = ""
    }
  }

----------------------------------------------------------------------------
-- Derivatives of primitive combinators

-- | @'single' t@ only matches the single token @t@.
--
-- > semicolon = single ';'
--
-- See also: 'token', 'anySingle', 'Text.Megaparsec.Byte.char',
-- 'Text.Megaparsec.Char.char'.
--
-- @since 7.0.0

single :: MonadParsec e s m
  => Token s           -- ^ Token to match
  -> m (Token s)
single t = token testToken expected
  where
    testToken x = if x == t then Just x else Nothing
    expected    = E.singleton (Tokens (t:|[]))
{-# INLINE single #-}

-- | The parser @'satisfy' f@ succeeds for any token for which the supplied
-- function @f@ returns 'True'.
--
-- > digitChar = satisfy isDigit <?> "digit"
-- > oneOf cs  = satisfy (`elem` cs)
--
-- See also: 'anySingle', 'anySingleBut', 'oneOf', 'noneOf'.
--
-- @since 7.0.0

satisfy :: MonadParsec e s m
  => (Token s -> Bool) -- ^ Predicate to apply
  -> m (Token s)
satisfy f = token testChar E.empty
  where
    testChar x = if f x then Just x else Nothing
{-# INLINE satisfy #-}

-- | Parse and return a single token. It's a good idea to attach a 'label'
-- to this parser.
--
-- > anySingle = satisfy (const True)
--
-- See also: 'satisfy', 'anySingleBut'.
--
-- @since 7.0.0

anySingle :: MonadParsec e s m => m (Token s)
anySingle = satisfy (const True)
{-# INLINE anySingle #-}

-- | Match any token but the given one. It's a good idea to attach a 'label'
-- to this parser.
--
-- > anySingleBut t = satisfy (/= t)
--
-- See also: 'single', 'anySingle', 'satisfy'.
--
-- @since 7.0.0

anySingleBut :: MonadParsec e s m
  => Token s           -- ^ Token we should not match
  -> m (Token s)
anySingleBut t = satisfy (/= t)
{-# INLINE anySingleBut #-}

-- | @'oneOf' ts@ succeeds if the current token is in the supplied
-- collection of tokens @ts@. Returns the parsed token. Note that this
-- parser cannot automatically generate the “expected” component of error
-- message, so usually you should label it manually with 'label' or ('<?>').
--
-- > oneOf cs = satisfy (`elem` cs)
--
-- See also: 'satisfy'.
--
-- > digit = oneOf ['0'..'9'] <?> "digit"
--
-- __Performance note__: prefer 'satisfy' when you can because it's faster
-- when you have only a couple of tokens to compare to:
--
-- > quoteFast = satisfy (\x -> x == '\'' || x == '\"')
-- > quoteSlow = oneOf "'\""
--
-- @since 7.0.0

oneOf :: (Foldable f, MonadParsec e s m)
  => f (Token s)       -- ^ Collection of matching tokens
  -> m (Token s)
oneOf cs = satisfy (`elem` cs)
{-# INLINE oneOf #-}

-- | As the dual of 'oneOf', @'noneOf' ts@ succeeds if the current token
-- /not/ in the supplied list of tokens @ts@. Returns the parsed character.
-- Note that this parser cannot automatically generate the “expected”
-- component of error message, so usually you should label it manually with
-- 'label' or ('<?>').
--
-- > noneOf cs = satisfy (`notElem` cs)
--
-- See also: 'satisfy'.
--
-- __Performance note__: prefer 'satisfy' and 'anySingleBut' when you can
-- because it's faster.
--
-- @since 7.0.0

noneOf :: (Foldable f, MonadParsec e s m)
  => f (Token s)       -- ^ Collection of taken we should not match
  -> m (Token s)
noneOf cs = satisfy (`notElem` cs)
{-# INLINE noneOf #-}

-- | @'chunk' chk@ only matches the chunk @chk@.
--
-- > divOrMod = chunk "div" <|> chunk "mod"
--
-- See also: 'tokens', 'Text.Megaparsec.Char.string',
-- 'Text.Megaparsec.Byte.string'.
--
-- @since 7.0.0

chunk :: MonadParsec e s m
  => Tokens s          -- ^ Chunk to match
  -> m (Tokens s)
chunk = tokens (==)
{-# INLINE chunk #-}

-- | A synonym for 'label' in the form of an operator.

infix 0 <?>

(<?>) :: MonadParsec e s m => m a -> String -> m a
(<?>) = flip label
{-# INLINE (<?>) #-}

-- | The parser @'unexpected' item@ fails with an error message telling
-- about unexpected item @item@ without consuming any input.
--
-- > unexpected item = failure (Just item) Set.empty

unexpected :: MonadParsec e s m => ErrorItem (Token s) -> m a
unexpected item = failure (Just item) E.empty
{-# INLINE unexpected #-}

-- | Report a custom parse error. For a more general version, see
-- 'fancyFailure'.
--
-- > customFailure = fancyFailure . E.singleton . ErrorCustom
--
-- @since 6.3.0

customFailure :: MonadParsec e s m => e -> m a
customFailure = fancyFailure . E.singleton . ErrorCustom
{-# INLINE customFailure #-}

-- | Return both the result of a parse and a chunk of input that was
-- consumed during parsing. This relies on the change of the 'stateOffset'
-- value to evaluate how many tokens were consumed. If you mess with it
-- manually in the argument parser, prepare for troubles.
--
-- @since 5.3.0

match :: MonadParsec e s m => m a -> m (Tokens s, a)
match p = do
  o  <- getOffset
  s  <- getInput
  r  <- p
  o' <- getOffset
  -- NOTE The 'fromJust' call here should never fail because if the stream
  -- is empty before 'p' (the only case when 'takeN_' can return 'Nothing'
  -- as per its invariants), (tp' - tp) won't be greater than 0, and in that
  -- case 'Just' is guaranteed to be returned as per another invariant of
  -- 'takeN_'.
  return ((fst . fromJust) (takeN_ (o' - o) s), r)
{-# INLINEABLE match #-}

-- | Specify how to process 'ParseError's that happen inside of this
-- wrapper. As a side effect of the current implementation changing
-- 'errorOffset' with this combinator will also change the final
-- 'stateOffset' in the parser state (try to avoid that because
-- 'stateOffset' will go out of sync with factual position in the input
-- stream and pretty-printing of parse errors afterwards will be incorrect).
--
-- @since 5.3.0

region :: MonadParsec e s m
  => (ParseError s e -> ParseError s e)
     -- ^ How to process 'ParseError's
  -> m a               -- ^ The “region” that the processing applies to
  -> m a
region f m = do
  r <- observing m
  case r of
    Left err ->
      case f err of
        TrivialError o us ps -> do
          updateParserState $ \st -> st { stateOffset = o }
          failure us ps
        FancyError o xs -> do
          updateParserState $ \st -> st { stateOffset = o }
          fancyFailure xs
    Right x -> return x
{-# INLINEABLE region #-}

-- | Consume the rest of the input and return it as a chunk. This parser
-- never fails, but may return the empty chunk.
--
-- > takeRest = takeWhileP Nothing (const True)
--
-- @since 6.0.0

takeRest :: MonadParsec e s m => m (Tokens s)
takeRest = takeWhileP Nothing (const True)
{-# INLINE takeRest #-}

-- | Return 'True' when end of input has been reached.
--
-- > atEnd = option False (True <$ hidden eof)
--
-- @since 6.0.0

atEnd :: MonadParsec e s m => m Bool
atEnd = option False (True <$ hidden eof)
{-# INLINE atEnd #-}

----------------------------------------------------------------------------
-- Parser state combinators

-- | Return the current input.

getInput :: MonadParsec e s m => m s
getInput = stateInput <$> getParserState
{-# INLINE getInput #-}

-- | @'setInput' input@ continues parsing with @input@.

setInput :: MonadParsec e s m => s -> m ()
setInput s = updateParserState (\(State _ o pst) -> State s o pst)
{-# INLINE setInput #-}

-- | Return the current source position. This function /is not cheap/, do
-- not call it e.g. on matching of every token, that's a bad idea. Still you
-- can use it to get 'SourcePos' to attach to things that you parse.
--
-- The function works under the assumption that we move in the input stream
-- only forwards and never backwards, which is always true unless the user
-- abuses the library.
--
-- @since 7.0.0

getSourcePos :: MonadParsec e s m => m SourcePos
getSourcePos = do
  st <- getParserState
  let pst = reachOffsetNoLine (stateOffset st) (statePosState st)
  setParserState st { statePosState = pst }
  return (pstateSourcePos pst)
{-# INLINE getSourcePos #-}

-- | Get the number of tokens processed so far.
--
-- See also: 'setOffset'.
--
-- @since 7.0.0

getOffset :: MonadParsec e s m => m Int
getOffset = stateOffset <$> getParserState
{-# INLINE getOffset #-}

-- | Set the number of tokens processed so far.
--
-- See also: 'getOffset'.
--
-- @since 7.0.0

setOffset :: MonadParsec e s m => Int -> m ()
setOffset o = updateParserState $ \(State s _ pst) ->
  State s o pst
{-# INLINE setOffset #-}

-- | @'setParserState' st@ sets the parser state to @st@.
--
-- See also: 'getParserState', 'updateParserState'.

setParserState :: MonadParsec e s m => State s -> m ()
setParserState st = updateParserState (const st)
{-# INLINE setParserState #-}
