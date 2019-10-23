{-# OPTIONS_GHC -Wno-orphans            #-}
module Text.Megaparsec.Internal.Monad() where

import qualified Control.Monad.Fail  as Fail
import Text.Megaparsec.Stream
import Text.Megaparsec.Internal.ParsecT

-- | 'return' returns a parser that __succeeds__ without consuming input.

instance Stream s => Monad (ParsecT e s m) where
  return = pure
  (>>=)  = pBind
  fail   = Fail.fail

instance Stream s => Fail.MonadFail (ParsecT e s m) where
  fail = pFail
