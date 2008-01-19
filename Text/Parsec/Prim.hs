-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Prim
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  portable
-- 
-- The primitive parser combinators.
-- 
-----------------------------------------------------------------------------   

{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}

module Text.Parsec.Prim where

import qualified Control.Applicative as Applicative ( Applicative(..), Alternative(..) )
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Identity

import Text.Parsec.Pos
import Text.Parsec.Error

unknownError state        = newErrorUnknown (statePos state)

sysUnExpectError msg pos  = Error (newErrorMessage (SysUnExpect msg) pos)

unexpected :: (Stream s m t) => String -> ParsecT s u m a
unexpected msg
    = ParsecT $ \s -> return $ Empty $ return $ 
                        Error (newErrorMessage (UnExpect msg) (statePos s))

-- | ParserT monad transformer and Parser type

data ParsecT s u m a
    = ParsecT { runParsecT :: State s u -> m (Consumed (m (Reply s u a))) }

type Parsec s u a = ParsecT s u Identity a

data Consumed a  = Consumed a
                 | Empty !a
data Reply s u a = Ok !a !(State s u) ParseError
                 | Error ParseError

data State s u = State {
      stateInput :: s,
      statePos   :: !SourcePos,
      stateUser  :: !u
    }

-- | Functor: fmap

instance Functor Consumed where
    fmap f (Consumed x) = Consumed (f x)
    fmap f (Empty x)    = Empty (f x)

instance Functor (Reply s u) where
    fmap f (Ok x s e) = Ok (f x) s e
    fmap f (Error e) = Error e -- XXX

instance (Monad m) => Functor (ParsecT s u m) where
    fmap f p = parsecMap f p

parsecMap :: (Monad m) => (a -> b) -> ParsecT s u m a -> ParsecT s u m b
parsecMap f p
    = ParsecT $ \s -> liftM (fmap (liftM (fmap f))) (runParsecT p s)

instance (Monad m) => Applicative.Applicative (ParsecT s u m) where
    pure = return
    (<*>) = ap -- TODO: Can this be optimized?

instance (Monad m) => Applicative.Alternative (ParsecT s u m) where
    empty = mzero
    (<|>) = mplus

instance (Monad m) => Monad (ParsecT s u m) where
    return x = parserReturn x
    p >>= f  = parserBind p f
    fail msg = parserFail msg

parserReturn :: (Monad m) => a -> ParsecT s u m a
parserReturn x
    = ParsecT $ \s -> return $ Empty $ return (Ok x s (unknownError s))

parserBind :: (Monad m)
           => ParsecT s u m a -> (a -> ParsecT s u m b) -> ParsecT s u m b

parserBind p f
    = ParsecT $ \s@(State _ u _) -> do
        res1 <- runParsecT p s
        case res1 of

          Empty mReply1
            -> do reply1 <- mReply1
                  case reply1 of
                    Ok x s' err1 -> do
                      res2 <- runParsecT (f x) s'
                      case res2 of
                        Empty mReply2
                          -> do reply2 <- mReply2
                                return $ Empty $
                                            return $ mergeErrorReply err1 reply2
                        other
                          -> do return $ other
                    Error err1 -> return $ Empty $ return $ Error err1

          Consumed mReply1
            -> do reply1 <- mReply1
                  return $ Consumed $ -- `early' returning
                    case reply1 of
                      Ok x s' err1 -> do
                        res2 <- runParsecT (f x) s'
                        case res2 of
                          Empty mReply2
                            -> do reply2 <- mReply2
                                  return $ mergeErrorReply err1 reply2
                          Consumed reply2 -> reply2
                      Error err1   -> return $ Error err1


mergeErrorReply err1 reply -- XXX where to put it?
    = case reply of
        Ok x state err2 -> Ok x state (mergeError err1 err2)
        Error err2      -> Error (mergeError err1 err2)

parserFail :: (Monad m) => String -> ParsecT s u m a
parserFail msg
    = ParsecT $ \s -> return $ Empty $ return $
        Error (newErrorMessage (Message msg) (statePos s))

-- | MonadPlus: choice (mplus) and zero (mzero)

instance (Monad m) => MonadPlus (ParsecT s u m) where
    mzero = parserZero
    mplus p1 p2 = parserPlus p1 p2

parserZero :: (Monad m) => ParsecT s u m a
parserZero
    = ParsecT $ \s -> return $ Empty $ return $ Error (unknownError s)

parserPlus :: (Monad m)
           => ParsecT s u m a -> ParsecT s u m a -> ParsecT s u m a
parserPlus (ParsecT p1) (ParsecT p2)
    = ParsecT $ \s -> do
        c1 <- p1 s
        case c1 of
          Empty mReply1
            -> do r1 <- mReply1
                  case r1 of
                    Error err -> do
                      c2 <- p2 s
                      case c2 of
                        Empty mReply2
                          -> do reply2 <- mReply2
                                return $ Empty $ return (mergeErrorReply err reply2)
                        consumed
                          -> return $ consumed
          other -> return $ other

-- | MonadTrans: lifing actions from the underlying monad to ParsecT

instance MonadTrans (ParsecT s u) where
    lift amb = ParsecT $ \s -> do
                 a <- amb
                 return $ Empty $ return $ Ok a s (unknownError s)

-- | Operators: label (<?>) and choice (<|>)
infix  0 <?>
infixr 1 <|>

(<?>) :: (Monad m)
      => (ParsecT s u m a) -> String -> (ParsecT s u m a)
p <?> msg = label p msg

(<|>) :: (Monad m)
      => (ParsecT s u m a) -> (ParsecT s u m a) -> (ParsecT s u m a)
p1 <|> p2 = mplus p1 p2

label :: (Monad m) => ParsecT s u m a -> String -> ParsecT s u m a
label p msg
  = labels p [msg]

labels :: (Monad m) => ParsecT s u m a -> [String] -> ParsecT s u m a
labels p msgs
    = ParsecT $ \s -> do
        r <- runParsecT p s
        case r of
          Empty mReply -> do
            reply <- mReply
            return $ Empty $ case reply of
              Error err
                -> return $ Error (setExpectErrors err msgs)
              Ok x s' err
                | errorIsUnknown err -> return $ reply
                | otherwise -> return (Ok x s' (setExpectErrors err msgs))
          other        -> return $ other
    where
        setExpectErrors err []         = setErrorMessage (Expect "") err
        setExpectErrors err [msg]      = setErrorMessage (Expect msg) err
        setExpectErrors err (msg:msgs)
            = foldr (\msg err -> addErrorMessage (Expect msg) err)
                    (setErrorMessage (Expect msg) err) msgs

-- | Streams: providing uncons

class (Monad m) => Stream s m t | s -> t where
    uncons :: s -> m (Maybe (t,s))

-- instance (Monad m) => Stream String m Char where
--     uncons [    ] = return $ Nothing
--     uncons (x:xs) = return $ Just (x,xs)

-- | Primitive parsers: tokens, try

tokens :: (Stream s m t, Eq t)
       => ([t] -> String)      -- Pretty print a list of tokens
       -> (SourcePos -> [t] -> SourcePos)
       -> [t]                  -- List of tokens to parse
       -> ParsecT s u m [t]
tokens _ _ []
    = ParsecT $ \s -> return $ Empty $ return $ Ok [] s (unknownError s)
tokens shows nextposs tts@(t:ts)
    = ParsecT $ \s@(State input pos u) -> 
    let
        errEof = return $ Error (setErrorMessage (Expect (shows tts))
                                 (newErrorMessage (SysUnExpect "") pos))
        errExpect x = return $ Error (setErrorMessage (Expect (shows tts))
                                 (newErrorMessage (SysUnExpect (shows [x])) pos))
        walk []     rs = return (ok rs)
        walk (t:ts) rs = do
          sr <- uncons rs
          case sr of
            Nothing                 -> errEof
            Just (x,xs) | t == x    -> walk ts xs
                        | otherwise -> errExpect x
        ok rs = let pos' = nextposs pos tts
                    s' = State rs pos' u
                in Ok tts s' (newErrorUnknown pos')
    in do
        sr <- uncons input
        return $ case sr of
            Nothing         -> Empty    $ errEof
            Just (x,xs)
                | t == x    -> Consumed $ walk ts xs
                | otherwise -> Empty    $ errExpect x
        
try :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
try (ParsecT p)
    = ParsecT $ \s@(State _ pos _) -> do
        res <- p s
        case res of
          Consumed rep -> do r <- rep
                             case r of
                               Error err -> return $ Empty $ return $ Error
                                                         (setErrorPos pos err)
                               ok        -> return $ Consumed $ return $ ok
          empty        -> return $ empty

token :: (Stream s Identity t)
      => (t -> String)            -- Token pretty-printing function.
      -> (t -> SourcePos)         -- Computes the position of a token.
      -> (t -> Maybe a)           -- Matching function for the token to parse.
      -> Parsec s u a
token show tokpos test = tokenPrim show nextpos test
    where
        nextpos _ tok ts = case runIdentity (uncons ts) of
                             Nothing -> tokpos tok
                             Just (tok',_) -> tokpos tok'

tokenPrim :: (Stream s m t)
          => (t -> String)
          -> (SourcePos -> t -> s -> SourcePos)
          -> (t -> Maybe a)
          -> ParsecT s u m a
tokenPrim show nextpos test = tokenPrimEx show nextpos Nothing test

tokenPrimEx :: (Stream s m t)
            => (t -> String)
            -> (SourcePos -> t -> s -> SourcePos)
            -> Maybe (SourcePos -> t -> s -> u -> u)
            -> (t -> Maybe a)
            -> ParsecT s u m a
tokenPrimEx show nextpos mbNextState test
    = case mbNextState of
        Nothing
          -> ParsecT $ \s@(State input pos user) -> do
              r <- uncons input
              case r of
                Nothing -> return $ Empty $ return (sysUnExpectError "" pos)
                Just (c,cs)
                  -> case test c of
                       Just x  -> let newpos   = nextpos pos c cs
                                      newstate = State cs newpos user
                                  in seq newpos $ seq newstate $
                                     return $ Consumed $ return $
                                       (Ok x newstate (newErrorUnknown newpos))
                       Nothing -> return $ Empty $ return $
                                    (sysUnExpectError (show c) pos)
        Just nextState
          -> ParsecT $ \s@(State input pos user) -> do
              r <- uncons input
              case r of
                Nothing -> return $ Empty $ return (sysUnExpectError "" pos)
                Just (c,cs)
                  -> case test c of
                       Just x  -> let newpos   = nextpos pos c cs
                                      newuser  = nextState pos c cs user
                                      newstate = State cs newpos newuser
                                  in seq newpos $ seq newstate $
                                     return $ Consumed $ return $
                                       (Ok x newstate (newErrorUnknown newpos))
                       Nothing -> return $ Empty $ return $
                                    (sysUnExpectError (show c) pos)

many :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
many p
  = do xs <- manyAccum (:) p
       return (reverse xs)

skipMany :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
skipMany p
  = do manyAccum (\x xs -> []) p
       return ()

manyAccum :: (Stream s m t)
          => (a -> [a] -> [a])
          -> ParsecT s u m a
          -> ParsecT s u m [a]
manyAccum accum p
    = ParsecT $ \s ->
        let walk xs state mr
              = do r <- mr
                   case r of
                     Empty mReply
                         -> do reply <- mReply
                               case reply of
                                 Error err -> return $ Ok xs s err
                                 ok -> error "Text.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
                     Consumed mReply
                         -> do reply <- mReply
                               case reply of
                                 Error err
                                     -> return $ Error err
                                 Ok x s' err
                                     -> let ys = accum x xs
                                        in seq ys (walk ys s' (runParsecT p s'))
        in do r <- runParsecT p s
              case r of
                Empty mReply
                    -> do reply <- mReply
                          case reply of
                            Ok x s' err
                                -> error "Text.ParserCombinators.Parsec.Prim.many: combinator 'many' is applied to a parser that accepts an empty string."
                            Error err
                                -> return $ Empty $ return (Ok [] s err)
                consumed
                    -> return $ Consumed $ walk [] s (return consumed)


-- | Running a parser: monadic (runPT) and pure (runP)

runPT :: (Stream s m t)
      => ParsecT s u m a -> u -> SourceName -> s -> m (Either ParseError a)
runPT p u name s
    = do res <- runParsecT p (State s (initialPos name) u)
         r <- parserReply res
         case r of
           Ok x _ _  -> return (Right x)
           Error err -> return (Left err)
    where
        parserReply res
            = case res of
                Consumed r -> r
                Empty    r -> r

runP :: (Stream s Identity t)
     => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runP p u name s = runIdentity $ runPT p u name s

runParser :: (Stream s Identity t)
          => Parsec s u a -> u -> SourceName -> s -> Either ParseError a
runParser = runP

parse :: (Stream s Identity t)
      => Parsec s () a -> SourceName -> s -> Either ParseError a
parse p = runP p ()

parseTest :: (Stream s Identity t, Show a)
          => Parsec s () a -> s -> IO ()
parseTest p input
    = case parse p "" input of
        Left err -> do putStr "parse error at "
                       print err
        Right x  -> print x

-- | Parser state combinators

getPosition :: (Monad m) => ParsecT s u m SourcePos
getPosition = do state <- getParserState
                 return (statePos state)

getInput :: (Monad m) => ParsecT s u m s
getInput = do state <- getParserState
              return (stateInput state)

setPosition :: (Monad m) => SourcePos -> ParsecT s u m ()
setPosition pos
    = do updateParserState (\(State input _ user) -> State input pos user)
         return ()

setInput :: (Monad m) => s -> ParsecT s u m ()
setInput input
    = do updateParserState (\(State _ pos user) -> State input pos user)
         return ()

getParserState :: (Monad m) => ParsecT s u m (State s u)
getParserState = updateParserState id

setParserState :: (Monad m) => State s u -> ParsecT s u m (State s u)
setParserState st = updateParserState (const st)

updateParserState :: (Monad m)
                  => (State s u -> State s u) -> ParsecT s u m (State s u)
updateParserState f
    = ParsecT $ \s -> let s' = f s
                      in return $ Empty $ return (Ok s' s' (unknownError s'))

-- | User state combinators

getState :: (Monad m) => ParsecT s u m u
getState = stateUser `liftM` getParserState

putState :: (Monad m) => u -> ParsecT s u m ()
putState u = do updateParserState $ \s -> s { stateUser = u }
                return ()

modifyState :: (Monad m) => (u -> u) -> ParsecT s u m ()
modifyState f = do updateParserState $ \s -> s { stateUser = f (stateUser s) }
                   return ()

-- XXX Compat

setState :: (Monad m) => u -> ParsecT s u m ()
setState = putState

updateState :: (Monad m) => (u -> u) -> ParsecT s u m ()
updateState = modifyState
