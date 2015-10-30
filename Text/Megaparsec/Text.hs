-- |
-- Module      :  Text.Megaparsec.Text
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2011 Antoine Latter
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with 'T.Text'.

module Text.Megaparsec.Text (Parser) where

import Text.Megaparsec.Prim
import qualified Data.Text as T

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strict text.

type Parser = Parsec T.Text
