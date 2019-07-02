-- |
-- Module      :  Text.Megaparsec.State
-- Copyright   :  © 2015–present Megaparsec contributors
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
  ( State (..)
  , PosState (..) )
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
  , stateOffset :: {-# UNPACK #-} !Int
    -- ^ Number of processed tokens so far
    --
    -- @since 7.0.0
  , statePosState :: PosState s
    -- ^ State that is used for line\/column calculation
    --
    -- @since 7.0.0
  } deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (State s)

-- | Special kind of state that is used to calculate line\/column positions
-- on demand.
--
-- @since 7.0.0

data PosState s = PosState
  { pstateInput :: s
    -- ^ The rest of input to process
  , pstateOffset :: !Int
    -- ^ Offset corresponding to beginning of 'pstateInput'
  , pstateSourcePos :: !SourcePos
    -- ^ Source position corresponding to beginning of 'pstateInput'
  , pstateTabWidth :: Pos
    -- ^ Tab width to use for column calculation
  , pstateLinePrefix :: String
    -- ^ Prefix to prepend to offending line
  } deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (PosState s)
