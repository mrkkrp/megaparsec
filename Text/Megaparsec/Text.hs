-- |
-- Module      :  Text.Megaparsec.Text
-- Copyright   :  © 2015–2017 Megaparsec contributors
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with strict 'Text'.

module Text.Megaparsec.Text (Parser) where

import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Prim
import Data.Text

-- | Modules corresponding to various types of streams define 'Parser'
-- accordingly, so the user can use it to easily change type of input stream
-- by importing different “type modules”. This one is for strict 'Text'.

type Parser = Parsec Dec Text
