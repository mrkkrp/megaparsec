-- |
-- Module      :  Text.Megaparsec.ByteString.Lazy
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with lazy 'ByteString'.

module Text.Megaparsec.ByteString.Lazy (Parser) where

import Data.ByteString.Lazy
import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Prim

-- | Modules corresponding to various types of streams define 'Parser'
-- accordingly, so the user can use it to easily change the type of input
-- stream by importing different “type modules”. This one is for lazy
-- 'ByteString's.

type Parser = Parsec Dec ByteString
