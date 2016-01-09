-- |
-- Module      :  Text.Megaparsec.ByteString
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Convenience definitions for working with 'B.ByteString'.

module Text.Megaparsec.ByteString (Parser) where

import Text.Megaparsec.Prim
import qualified Data.ByteString as B

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strict byte-strings.

type Parser = Parsec B.ByteString
