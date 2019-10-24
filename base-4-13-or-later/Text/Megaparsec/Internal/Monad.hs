{-# OPTIONS_GHC -Wno-orphans            #-}
module Text.Megaparsec.Internal.Monad() where

import {-# SOURCE #-} Text.Megaparsec.Internal
import Text.Megaparsec.Stream

-- | 'return' returns a parser that __succeeds__ without consuming input.

instance Stream s => Monad (ParsecT e s m) where
  return = pure
  (>>=)  = pBind
