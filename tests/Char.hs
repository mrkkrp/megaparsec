-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's character parsers.
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

module Char (tests) where

import Data.Char
import Data.List (findIndex, isPrefixOf)

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

import Util

tests :: Test
tests = testGroup "Character parsers"
        [ testProperty "oneOf" prop_oneOf
        , testProperty "noneOf" prop_noneOf
        , testProperty "spaces" prop_spaces
        , testProperty "space" prop_space
        , testProperty "newline" prop_newline
        , testProperty "crlf" prop_crlf
        , testProperty "eol" prop_eol
        , testProperty "tab" prop_tab
        , testProperty "upper" prop_upper
        , testProperty "lower" prop_lower
        , testProperty "alphaNum" prop_alphaNum
        , testProperty "letter" prop_letter
        , testProperty "digit" prop_digit
        , testProperty "hexDigit" prop_hexDigit
        , testProperty "octDigit" prop_octDigit
        , testProperty "char" prop_char
        , testProperty "anyChar" prop_anyChar
        , testProperty "string" prop_string ]

prop_oneOf :: String -> String -> Property
prop_oneOf a = singleChar (oneOf a) (`elem` a) Nothing

prop_noneOf :: String -> String -> Property
prop_noneOf a = singleChar (noneOf a) (`notElem` a) Nothing

prop_spaces :: String -> Property
prop_spaces s = checkParser spaces r s
    where r = case findIndex (not . isSpace) s of
                Just x  ->
                    let ch = s !! x
                    in posErr x s
                           [ suneCh ch
                           , uneCh  ch
                           , exSpec "white space"
                           , exStr "" ]
                Nothing -> Right ()

prop_space :: String -> Property
prop_space = singleChar space isSpace (Just "white space")

prop_newline :: String -> Property
prop_newline = singleChar newline (== '\n') (Just "newline")

prop_crlf :: String -> Property
prop_crlf s = property True -- TODO

prop_eol :: String -> Property
prop_eol s = property True -- TODO

prop_tab :: String -> Property
prop_tab = singleChar tab (== '\t') (Just "tab")

prop_upper :: String -> Property
prop_upper = singleChar upper isUpper (Just "uppercase letter")

prop_lower :: String -> Property
prop_lower = singleChar lower isLower (Just "lowercase letter")

prop_alphaNum :: String -> Property
prop_alphaNum = singleChar alphaNum isAlphaNum (Just "letter or digit")

prop_letter :: String -> Property
prop_letter = singleChar letter isAlpha (Just "letter")

prop_digit :: String -> Property
prop_digit = singleChar digit isDigit (Just "digit")

prop_hexDigit :: String -> Property
prop_hexDigit = singleChar hexDigit isHexDigit (Just "hexadecimal digit")

prop_octDigit :: String -> Property
prop_octDigit = singleChar octDigit isOctDigit (Just "octal digit")

prop_char :: Char -> String -> Property
prop_char c = singleChar (char c) (== c) (Just $ showToken c)

prop_anyChar :: String -> Property
prop_anyChar = singleChar anyChar (const True) (Just "character")

-- FIXME: In general, currently I don't like error messages that ‘string’
-- produces. It tells me about first mismatching letter, but also presents
-- the whole expected word:
--
-- λ> parseTest (string "re" <* eof) "ri"
-- parse error at line 1, column 1:
-- unexpected "i"
-- expecting "re"
--
-- is it the best error message we can get here?

prop_string :: String -> String -> Property
prop_string a s = property True
-- prop_string a s = checkParser (string a) r s
--     where r | a == s = Right s
