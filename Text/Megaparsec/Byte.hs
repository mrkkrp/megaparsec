-- |
-- Module      :  Text.Megaparsec.Byte
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used binary parsers.

module Text.Megaparsec.Byte
  (
    -- * Sequence of bytes
    C.string
  , C.string' )
where

import qualified Text.Megaparsec.Char as C
