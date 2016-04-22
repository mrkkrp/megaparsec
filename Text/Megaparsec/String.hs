-- |
-- Module      :  Text.Megaparsec.String
-- Copyright   :  © 2015–2016 Megaparsec contributors
--                © 2007 Paolo Martini
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Make Strings an instance of 'Stream' with 'Char' token type.

module Text.Megaparsec.String (Parser) where

import Text.Megaparsec.Error (Dec)
import Text.Megaparsec.Prim

-- | Different modules corresponding to various types of streams (@String@,
-- @Text@, @ByteString@) define it differently, so user can use “abstract”
-- @Parser@ type and easily change it by importing different “type
-- modules”. This one is for strings.

type Parser = Parsec Dec String
