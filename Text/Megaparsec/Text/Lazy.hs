-- |
-- Module      :  Text.Megaparsec.Text.Lazy
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2011 Antoine Latter
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with lazy 'T.Text'.

module Text.Megaparsec.Text.Lazy (Parser) where

import Text.Megaparsec.Prim
import qualified Data.Text.Lazy as T

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for lazy text.

type Parser = Parsec String T.Text
