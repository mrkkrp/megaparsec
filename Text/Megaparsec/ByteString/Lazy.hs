-- |
-- Module      :  Text.Megaparsec.ByteString.Lazy
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with lazy 'B.ByteString'.

module Text.Megaparsec.ByteString.Lazy (Parser) where

import Text.Megaparsec.Prim
import qualified Data.ByteString.Lazy as B

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for lazy byte-strings.

type Parser = Parsec B.ByteString
