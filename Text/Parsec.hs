-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  portable
-- 
-----------------------------------------------------------------------------

module Text.Parsec
    ( module Text.Parsec.Prim
    , module Text.Parsec.Char
    , module Text.Parsec.Combinator
    , module Text.Parsec.String
    , module Text.Parsec.ByteString
    , module Text.Parsec.LazyByteString
    , ParseError
    , errorPos
    , SourcePos
    , SourceName, Line, Column
    , sourceName, sourceLine, sourceColumn
    , incSourceLine, incSourceColumn
    , setSourceLine, setSourceColumn, setSourceName
    ) where

import Text.Parsec.Pos
import Text.Parsec.Error
import Text.Parsec.Prim
import Text.Parsec.Char
import Text.Parsec.Combinator
import Text.Parsec.String         hiding ( Parser, GenParser, parseFromFile )
import Text.Parsec.ByteString     hiding ( Parser, GenParser, parseFromFile )
import Text.Parsec.LazyByteString hiding ( Parser, GenParser, parseFromFile )
