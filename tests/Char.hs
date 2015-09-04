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

{-# OPTIONS -fno-warn-orphans #-}

module Char (tests) where

import Data.Char
import Data.List (findIndex, isPrefixOf)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char

import Util

tests :: Test
tests = testGroup "Character parsers"
        [ testProperty "newline"         prop_newline
        , testProperty "crlf"            prop_crlf
        , testProperty "eol"             prop_eol
        , testProperty "tab"             prop_tab
        , testProperty "space"           prop_space
        , testProperty "controlChar"     prop_controlChar
        , testProperty "spaceChar"       prop_spaceChar
        , testProperty "upperChar"       prop_upperChar
        , testProperty "lowerChar"       prop_lowerChar
        , testProperty "letterChar"      prop_letterChar
        , testProperty "alphaNumChar"    prop_alphaNumChar
        , testProperty "printChar"       prop_printChar
        , testProperty "digitChar"       prop_digitChar
        , testProperty "hexDigitChar"    prop_hexDigitChar
        , testProperty "octDigitChar"    prop_octDigitChar
        , testProperty "markChar"        prop_markChar
        , testProperty "numberChar"      prop_numberChar
        , testProperty "punctuationChar" prop_punctuationChar
        , testProperty "symbolChar"      prop_symbolChar
        , testProperty "separatorChar"   prop_separatorChar
        , testProperty "asciiChar"       prop_asciiChar
        , testProperty "latin1Char"      prop_latin1Char
        , testProperty "charCategory"    prop_charCategory
        , testProperty "char"            prop_char
        , testProperty "anyChar"         prop_anyChar
        , testProperty "oneOf"           prop_oneOf
        , testProperty "noneOf"          prop_noneOf
        , testProperty "string"          prop_string
        , testProperty "string'"         prop_string' ]

instance Arbitrary GeneralCategory where
  arbitrary = elements
              [ UppercaseLetter
              , LowercaseLetter
              , TitlecaseLetter
              , ModifierLetter
              , OtherLetter
              , NonSpacingMark
              , SpacingCombiningMark
              , EnclosingMark
              , DecimalNumber
              , LetterNumber
              , OtherNumber
              , ConnectorPunctuation
              , DashPunctuation
              , OpenPunctuation
              , ClosePunctuation
              , InitialQuote
              , FinalQuote
              , OtherPunctuation
              , MathSymbol
              , CurrencySymbol
              , ModifierSymbol
              , OtherSymbol
              , Space
              , LineSeparator
              , ParagraphSeparator
              , Control
              , Format
              , Surrogate
              , PrivateUse
              , NotAssigned ]

prop_newline :: String -> Property
prop_newline = checkChar newline (== '\n') (Just "newline")

prop_crlf :: String -> Property
prop_crlf = checkString crlf "\r\n" (==) "crlf newline"

prop_eol :: String -> Property
prop_eol s = checkParser eol r s
  where h = head s
        r | s == "\n"   = Right "\n"
          | s == "\r\n" = Right "\r\n"
          | null s      = posErr 0 s [uneEof, exSpec "end of line"]
          | h == '\n'   = posErr 1 s [uneCh (s !! 1), exEof]
          | h /= '\r'   = posErr 0 s [uneCh h, exSpec "end of line"]
          | "\r\n" `isPrefixOf` s = posErr 2 s [uneCh (s !! 2), exEof]
          | otherwise   = posErr 0 s [ uneStr (take 2 s)
                                     , uneCh '\r'
                                     , exSpec "crlf newline"
                                     , exSpec "newline" ]

prop_tab :: String -> Property
prop_tab = checkChar tab (== '\t') (Just "tab")

prop_space :: String -> Property
prop_space s = checkParser space r s
  where r = case findIndex (not . isSpace) s of
              Just x  ->
                  let ch = s !! x
                  in posErr x s
                     [ uneCh ch
                     , uneCh ch
                     , exSpec "white space"
                     , exEof ]
              Nothing -> Right ()

prop_controlChar :: String -> Property
prop_controlChar = checkChar controlChar isControl (Just "control character")

prop_spaceChar :: String -> Property
prop_spaceChar = checkChar spaceChar isSpace (Just "white space")

prop_upperChar :: String -> Property
prop_upperChar = checkChar upperChar isUpper (Just "uppercase letter")

prop_lowerChar :: String -> Property
prop_lowerChar = checkChar lowerChar isLower (Just "lowercase letter")

prop_letterChar :: String -> Property
prop_letterChar = checkChar letterChar isAlpha (Just "letter")

prop_alphaNumChar :: String -> Property
prop_alphaNumChar =
  checkChar alphaNumChar isAlphaNum (Just "alphanumeric character")

prop_printChar :: String -> Property
prop_printChar = checkChar printChar isPrint (Just "printable character")

prop_digitChar :: String -> Property
prop_digitChar = checkChar digitChar isDigit (Just "digit")

prop_octDigitChar :: String -> Property
prop_octDigitChar = checkChar octDigitChar isOctDigit (Just "octal digit")

prop_hexDigitChar :: String -> Property
prop_hexDigitChar = checkChar hexDigitChar isHexDigit (Just "hexadecimal digit")

prop_markChar :: String -> Property
prop_markChar = checkChar markChar isMark (Just "mark character")

prop_numberChar :: String -> Property
prop_numberChar = checkChar numberChar isNumber (Just "numeric character")

prop_punctuationChar :: String -> Property
prop_punctuationChar =
  checkChar punctuationChar isPunctuation (Just "punctuation")

prop_symbolChar :: String -> Property
prop_symbolChar = checkChar symbolChar isSymbol (Just "symbol")

prop_separatorChar :: String -> Property
prop_separatorChar = checkChar separatorChar isSeparator (Just "separator")

prop_asciiChar :: String -> Property
prop_asciiChar = checkChar asciiChar isAscii (Just "ASCII character")

prop_latin1Char :: String -> Property
prop_latin1Char = checkChar latin1Char isLatin1 (Just "Latin-1 character")

prop_charCategory :: GeneralCategory -> String -> Property
prop_charCategory cat = checkChar (charCategory cat) p (Just $ categoryName cat)
  where p c = generalCategory c == cat

prop_char :: Char -> String -> Property
prop_char c = checkChar (char c) (== c) (Just $ showToken c)

prop_anyChar :: String -> Property
prop_anyChar = checkChar anyChar (const True) (Just "character")

prop_oneOf :: String -> String -> Property
prop_oneOf a = checkChar (oneOf a) (`elem` a) Nothing

prop_noneOf :: String -> String -> Property
prop_noneOf a = checkChar (noneOf a) (`notElem` a) Nothing

prop_string :: String -> String -> Property
prop_string a = checkString (string a) a (==) (showToken a)

prop_string' :: String -> String -> Property
prop_string' a = checkString (string' a) a test (showToken a)
  where test x y = toLower x == toLower y
