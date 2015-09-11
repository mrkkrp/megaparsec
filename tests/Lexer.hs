-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's lexer.
--
-- Copyright © 2015 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders "as is" and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

module Lexer (tests) where

import Control.Applicative (some, (<|>))
import Data.Char (readLitChar, showLitChar)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Numeric (showInt, showHex, showOct, showSigned)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

-- import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Lexer
import Text.Megaparsec.Prim

import Util

tests :: Test
tests = testGroup "Lexer"
        [ testProperty "space combinator"       prop_space
        , testProperty "space lexeme"           prop_lexeme
        , testProperty "space symbol"           prop_symbol
        , testProperty "space symbol'"          prop_symbol'
        , testProperty "space indentGuard"      prop_indentGuard
        , testProperty "space skipLineComment"  prop_skipLineComment
        , testProperty "space skipBlockComment" prop_skipBlockComment
        , testProperty "space charLiteral"      prop_charLiteral
        , testProperty "space integer"          prop_integer
        , testProperty "space decimal"          prop_decimal
        , testProperty "space hexadecimal"      prop_hexadecimal
        , testProperty "space octal"            prop_octal
        , testProperty "space float"            prop_float
        , testProperty "space number"           prop_number
        , testProperty "space signed"           prop_signed ]

prop_space :: Property
prop_space = property True

prop_lexeme :: Property
prop_lexeme = property True

prop_symbol :: Property
prop_symbol = property True

prop_symbol' :: Property
prop_symbol' = property True

prop_indentGuard :: Property
prop_indentGuard = property True

prop_skipLineComment :: Property
prop_skipLineComment = property True

prop_skipBlockComment :: Property
prop_skipBlockComment = property True

prop_charLiteral :: String -> Bool -> Property
prop_charLiteral t i = checkParser charLiteral r s
  where b = listToMaybe $ readLitChar s
        (h, g) = fromJust b
        r | isNothing b = posErr 0 s $ exSpec "literal character" :
                          [ if null s then uneEof else uneCh (head s) ]
          | null g      = Right h
          | otherwise   = posErr l s [uneCh (head g), exEof]
        l = length s - length g
        s = if null t || i then t else showLitChar (head t) (tail t)

prop_integer :: NonNegative Integer -> Int -> Property
prop_integer n' i = checkParser integer r s
  where (r, s) = quasiCorrupted n' i showInt "integer"

prop_decimal :: NonNegative Integer -> Int -> Property
prop_decimal n' i = checkParser decimal r s
  where (r, s) = quasiCorrupted n' i showInt "digit"

prop_hexadecimal :: NonNegative Integer -> Int -> Property
prop_hexadecimal n' i = checkParser hexadecimal r s
  where (r, s) = quasiCorrupted n' i showHex "hexadecimal digit"

prop_octal :: NonNegative Integer -> Int -> Property
prop_octal n' i = checkParser octal r s
  where (r, s) = quasiCorrupted n' i showOct "octal digit"

prop_float :: Property
prop_float = property True

prop_number :: Property
prop_number = property True

prop_signed :: Property
prop_signed = property True

quasiCorrupted :: NonNegative Integer -> Int
               -> (Integer -> String -> String) -> String
               -> (Either ParseError Integer, String)
quasiCorrupted n' i shower l = (r, s)
  where n = getNonNegative n'
        r | i > length z = Right n
          | otherwise    = posErr i s $ [uneCh '?', exSpec l] ++
                           [ exEof | i > 0 ]
        z = shower n ""
        s = if i <= length z then take i z ++ "?" ++ drop i z else z
