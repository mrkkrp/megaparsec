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

{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE UndecidableInstances #-}

module Text.Megaparsec.State
  ( State (..)
  , PosState (..) )
where

import Control.DeepSeq (NFData)
import Data.Data (Data)
import Data.Typeable (Typeable)
import GHC.Generics
import Text.Megaparsec.Pos
import {-# SOURCE #-} Text.Megaparsec.Error (ParseError)

-- | This is the Megaparsec's state parametrized over stream type @s@ and
-- custom error component type @e@.

data State s e = State
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
  , stateParseErrors :: [ParseError s e]
    -- ^ Collection of “delayed” 'ParseError's in reverse order. This means
    -- that the last registered error is the first element of the list.
    --
    -- @since 8.0.0
  } deriving (Typeable, Generic)

deriving instance ( Show (ParseError s e)
                  , Show s
                  ) => Show (State s e)

deriving instance ( Eq (ParseError s e)
                  , Eq s
                  ) => Eq (State s e)

deriving instance ( Data e
                  , Data (ParseError s e)
                  , Data s
                  ) => Data (State s e)

instance (NFData s, NFData (ParseError s e)) => NFData (State s e)

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
