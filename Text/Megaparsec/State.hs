{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

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
module Text.Megaparsec.State
  ( State (..),
    PosState (..),
  )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import {-# SOURCE #-} Text.Megaparsec.Error (ParseError)
import Text.Megaparsec.Pos

-- | This is the Megaparsec's state parametrized over stream type @s@ and
-- custom error component type @e@.
data State s e = State
  { -- | The rest of input to process
    stateInput :: s,
    -- | Number of processed tokens so far
    --
    -- @since 7.0.0
    stateOffset :: {-# UNPACK #-} !Int,
    -- | State that is used for line\/column calculation
    --
    -- @since 7.0.0
    statePosState :: PosState s,
    -- | Collection of “delayed” 'ParseError's in reverse order. This means
    -- that the last registered error is the first element of the list.
    --
    -- @since 8.0.0
    stateParseErrors :: [ParseError s e]
  }
  deriving (Typeable, Generic)

deriving instance
  ( Show (ParseError s e),
    Show s
  ) =>
  Show (State s e)

deriving instance
  ( Eq (ParseError s e),
    Eq s
  ) =>
  Eq (State s e)

deriving instance
  ( Data e,
    Data (ParseError s e),
    Data s
  ) =>
  Data (State s e)

instance (NFData s, NFData (ParseError s e)) => NFData (State s e)

-- | Special kind of state that is used to calculate line\/column positions
-- on demand.
--
-- @since 7.0.0
data PosState s = PosState
  { -- | The rest of input to process
    pstateInput :: s,
    -- | Offset corresponding to beginning of 'pstateInput'
    pstateOffset :: !Int,
    -- | Source position corresponding to beginning of 'pstateInput'
    pstateSourcePos :: !SourcePos,
    -- | Tab width to use for column calculation
    pstateTabWidth :: Pos,
    -- | Prefix to prepend to offending line
    pstateLinePrefix :: String
  }
  deriving (Show, Eq, Data, Typeable, Generic)

instance NFData s => NFData (PosState s)
