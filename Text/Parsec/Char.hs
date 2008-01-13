-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Char
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Commonly used character parsers.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE FlexibleContexts #-}

module Text.Parsec.Char where

import Data.Char
import Text.Parsec.Pos
import Text.Parsec.Prim

-- | Character parsers

oneOf, noneOf :: (Stream s m Char) => [Char] -> ParsecT s u m Char
oneOf cs            = satisfy (\c -> elem c cs)
noneOf cs           = satisfy (\c -> not (elem c cs))

spaces :: (Stream s m Char) => ParsecT s u m ()
spaces              = skipMany space        <?> "white space"

space, newline, tab :: (Stream s m Char) => ParsecT s u m Char
space               = satisfy (isSpace)     <?> "space"
newline             = char '\n'             <?> "new-line"
tab                 = char '\t'             <?> "tab"

upper, lower, alphaNum, letter, digit, hexDigit, octDigit :: (Stream s m Char)
    => ParsecT s u m Char
upper               = satisfy (isUpper)     <?> "uppercase letter"
lower               = satisfy (isLower)     <?> "lowercase letter"
alphaNum            = satisfy (isAlphaNum)  <?> "letter or digit"
letter              = satisfy (isAlpha)     <?> "letter"
digit               = satisfy (isDigit)     <?> "digit"
hexDigit            = satisfy (isHexDigit)  <?> "hexadecimal digit"
octDigit            = satisfy (isOctDigit)  <?> "octal digit"

char :: (Stream s m Char) => Char -> ParsecT s u m Char
char c              = satisfy (==c)  <?> show [c]

anyChar :: (Stream s m Char) => ParsecT s u m Char
anyChar             = satisfy (const True)

-- | Primitive character parsers

satisfy :: (Stream s m Char) => (Char -> Bool) -> ParsecT s u m Char
satisfy f           = tokenPrim (\c -> show [c])
                                (\pos c cs -> updatePosChar pos c)
                                (\c -> if f c then Just c else Nothing)

string :: (Stream s m Char) => String -> ParsecT s u m String
string s            = tokens show updatePosString s
