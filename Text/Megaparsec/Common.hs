-- |
-- Module      :  Text.Megaparsec.Common
-- Copyright   :  © 2018–2019 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Common token combinators. This module is not public, the functions from
-- it are re-exported in "Text.Megaparsec.Byte" and "Text.Megaparsec.Char".
--
-- @since 7.0.0

{-# LANGUAGE FlexibleContexts #-}

module Text.Megaparsec.Common
  ( string
  , string' )
where

import Data.Function (on)
import Text.Megaparsec
import qualified Data.CaseInsensitive as CI

-- | A synonym for 'chunk'.

string :: MonadParsec e s m => Tokens s -> m (Tokens s)
string = chunk
{-# INLINE string #-}

-- | The same as 'string', but case-insensitive. On success returns string
-- cased as actually parsed input.
--
-- >>> parseTest (string' "foobar") "foObAr"
-- "foObAr"

string' :: (MonadParsec e s m, CI.FoldCase (Tokens s))
  => Tokens s
  -> m (Tokens s)
string' = tokens ((==) `on` CI.mk)
{-# INLINE string' #-}
