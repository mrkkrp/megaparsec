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

import Data.Char (readLitChar, showLitChar, isDigit)
import Data.List (findIndices)
import Data.Maybe (listToMaybe, isNothing, fromJust)
import Numeric (showInt, showHex, showOct, showSigned)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Error
import Text.Megaparsec.Lexer
import Text.Megaparsec.Prim
import qualified Text.Megaparsec.Char as C

import Util

tests :: Test
tests = testGroup "Lexer"
        [ testProperty "space combinator"            prop_space
        , testProperty "lexeme combinator"           prop_lexeme
        , testProperty "symbol combinator"           prop_symbol
        , testProperty "symbol' combinator"          prop_symbol'
        , testProperty "indentGuard combinator"      prop_indentGuard
        , testProperty "skipLineComment combinator"  prop_skipLineComment
        , testProperty "skipBlockComment combinator" prop_skipBlockComment
        , testProperty "charLiteral"                 prop_charLiteral
        , testProperty "integer"                     prop_integer
        , testProperty "decimal"                     prop_decimal
        , testProperty "hexadecimal"                 prop_hexadecimal
        , testProperty "octal"                       prop_octal
        , testProperty "float 0"                     prop_float_0
        , testProperty "float 1"                     prop_float_1
        , testProperty "number"                      prop_number
        , testProperty "signed"                      prop_signed ]

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

prop_float_0 :: NonNegative Double -> Property
prop_float_0 n' = checkParser float r s
  where n = getNonNegative n'
        r = Right n
        s = show n

prop_float_1 :: Maybe (NonNegative Integer) -> Property
prop_float_1 n' = checkParser float r s
  where r | isNothing n' = posErr 0 s [uneEof, exSpec "float"]
          | otherwise    = posErr (length s) s [ uneEof, exCh '.', exCh 'E'
                                  , exCh 'e', exSpec "digit" ]
        s = maybe "" (show . getNonNegative) n'

prop_number :: Either (NonNegative Integer) (NonNegative Double)
            -> Integer -> Property
prop_number n' i = checkParser number r s
  where r | null s    = posErr 0 s [uneEof, exSpec "number"]
          | otherwise =
            Right $ case n' of
                      Left  x -> Left  $ getNonNegative x
                      Right x -> Right $ getNonNegative x
        s = if i < 5
            then ""
            else either (show . getNonNegative) (show . getNonNegative) n'

prop_signed :: Integer -> Int -> Bool -> Property
prop_signed n i plus = checkParser p r s
  where p = signed (hidden C.space) integer
        r | i > length z = Right n
          | otherwise    = posErr i s $ [uneCh '?', exSpec "integer"] ++
                           (if i <= 0 then [exCh '+', exCh '-'] else []) ++
                           [exEof | i > head (findIndices isDigit s)]
        z = let bar = showSigned showInt 0 n ""
            in if n < 0 || plus then bar else '+' : bar
        s = if i <= length z then take i z ++ "?" ++ drop i z else z

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
