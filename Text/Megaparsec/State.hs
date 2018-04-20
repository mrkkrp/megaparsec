-- |
-- Module      :  Text.Megaparsec.State
-- Copyright   :  © 2015–2018 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Definition of Megaparsec's 'State'.
--
-- @since 6.5.0

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Text.Megaparsec.State
  ( State (..) )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.Megaparsec.Pos

-- | This is the Megaparsec's state parametrized over stream type @s@.

data State s = State
  { stateInput :: s
    -- ^ The rest of input to process
  , statePos :: SourcePos
    -- ^ Current position (column + line number) with support for include
    -- files
    --
    -- In version /7.0.0/ type of the filed was changed.
  , stateTokensProcessed :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
    --
    -- @since 5.2.0
  , stateTabWidth :: Pos
    -- ^ Tab width to use
  } deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (State s)
