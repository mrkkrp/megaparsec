-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.String
-- Copyright   :  (c) Antoine Latter 2011
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  aslatter@gmail.com
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Make Text an instance of 'Stream' with 'Char' token type.
--
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Text.Parsec.Text
    ( Parser, GenParser
    ) where

import qualified Data.Text as Text
import Text.Parsec.Error
import Text.Parsec.Prim

instance (Monad m) => Stream Text.Text m Char where
    uncons = return . Text.uncons
    {-# INLINE uncons #-}

type Parser = Parsec Text.Text ()
type GenParser st = Parsec Text.Text st
