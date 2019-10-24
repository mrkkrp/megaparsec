{-# LANGUAGE RankNTypes                 #-}
module Text.Megaparsec.Internal where

import Data.Set (Set)
import Text.Megaparsec.Error
import Text.Megaparsec.State
import Text.Megaparsec.Stream

newtype Hints t = Hints [Set (ErrorItem t)]

instance Functor (ParsecT e s m)
instance Stream s => Applicative (ParsecT e s m)

newtype ParsecT e s m a = ParsecT
  { unParser
      :: forall b. State s
      -> (a -> State s   -> Hints (Token s) -> m b) -- consumed-OK
      -> (ParseError s e -> State s         -> m b) -- consumed-error
      -> (a -> State s   -> Hints (Token s) -> m b) -- empty-OK
      -> (ParseError s e -> State s         -> m b) -- empty-error
      -> m b }

pBind :: Stream s
  => ParsecT e s m a
  -> (a -> ParsecT e s m b)
  -> ParsecT e s m b

pFail :: String -> ParsecT e s m a
