--
-- QuickCheck tests for Megaparsec's character parsers.
--
-- Copyright © 2015–2016 Megaparsec contributors
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
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Char (tests) where

import Data.Char
import Data.List (findIndex, isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Error

import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif

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
  , testProperty "char'"           prop_char'
  , testProperty "anyChar"         prop_anyChar
  , testProperty "oneOf"           prop_oneOf
  , testProperty "oneOf'"          prop_oneOf'
  , testProperty "noneOf"          prop_noneOf
  , testProperty "noneOf'"         prop_noneOf'
  , testProperty "string"          prop_string
  , testProperty "string'"         prop_string'_0
  , testProperty "string' (case)"  prop_string'_1 ]

instance Arbitrary GeneralCategory where
  arbitrary = elements [minBound..maxBound]

prop_newline :: String -> Property
prop_newline = checkChar newline (== '\n') (tkn '\n')

prop_crlf :: String -> Property
prop_crlf = checkString crlf "\r\n" (==)

prop_eol :: String -> Property
prop_eol s = checkParser eol r s
  where h = head s
        r | s == "\n"   = Right "\n"
          | s == "\r\n" = Right "\r\n"
          | null s      = posErr 0 s [ueof, elabel "end of line"]
          | h == '\n'   = posErr 1 s [utok (s !! 1), eeof]
          | h /= '\r'   = posErr 0 s [utok h, elabel "end of line"]
          | "\r\n" `isPrefixOf` s = posErr 2 s [utok (s !! 2), eeof]
          | otherwise   = posErr 0 s [ utoks (take 2 s)
                                     , utok '\r'
                                     , elabel "end of line" ]

prop_tab :: String -> Property
prop_tab = checkChar tab (== '\t') (tkn '\t')

prop_space :: String -> Property
prop_space s = checkParser space r s
  where r = case findIndex (not . isSpace) s of
              Just x  ->
                  let ch = s !! x
                  in posErr x s
                     [ utok ch
                     , utok ch
                     , elabel "white space"
                     , eeof ]
              Nothing -> Right ()

prop_controlChar :: String -> Property
prop_controlChar = checkChar controlChar isControl (lbl "control character")

prop_spaceChar :: String -> Property
prop_spaceChar = checkChar spaceChar isSpace (lbl "white space")

prop_upperChar :: String -> Property
prop_upperChar = checkChar upperChar isUpper (lbl "uppercase letter")

prop_lowerChar :: String -> Property
prop_lowerChar = checkChar lowerChar isLower (lbl "lowercase letter")

prop_letterChar :: String -> Property
prop_letterChar = checkChar letterChar isAlpha (lbl "letter")

prop_alphaNumChar :: String -> Property
prop_alphaNumChar = checkChar alphaNumChar isAlphaNum
  (lbl "alphanumeric character")

prop_printChar :: String -> Property
prop_printChar = checkChar printChar isPrint (lbl "printable character")

prop_digitChar :: String -> Property
prop_digitChar = checkChar digitChar isDigit (lbl "digit")

prop_octDigitChar :: String -> Property
prop_octDigitChar = checkChar octDigitChar isOctDigit (lbl "octal digit")

prop_hexDigitChar :: String -> Property
prop_hexDigitChar = checkChar hexDigitChar isHexDigit (lbl "hexadecimal digit")

prop_markChar :: String -> Property
prop_markChar = checkChar markChar isMark (lbl "mark character")

prop_numberChar :: String -> Property
prop_numberChar = checkChar numberChar isNumber (lbl "numeric character")

prop_punctuationChar :: String -> Property
prop_punctuationChar = checkChar punctuationChar isPunctuation (lbl "punctuation")

prop_symbolChar :: String -> Property
prop_symbolChar = checkChar symbolChar isSymbol (lbl "symbol")

prop_separatorChar :: String -> Property
prop_separatorChar = checkChar separatorChar isSeparator (lbl "separator")

prop_asciiChar :: String -> Property
prop_asciiChar = checkChar asciiChar isAscii (lbl "ASCII character")

prop_latin1Char :: String -> Property
prop_latin1Char = checkChar latin1Char isLatin1 (lbl "Latin-1 character")

prop_charCategory :: GeneralCategory -> String -> Property
prop_charCategory cat = checkChar (charCategory cat) p (lbl $ categoryName cat)
  where p c = generalCategory c == cat

prop_char :: Char -> String -> Property
prop_char c = checkChar (char c) (== c) (tkn c)

prop_char' :: Char -> String -> Property
prop_char' c s = checkParser (char' c) r s
  where h = head s
        l | isLower c = [c, toUpper c]
          | isUpper c = [c, toLower c]
          | otherwise = [c]
        r | null s         = posErr 0 s $ ueof : (etok <$> l)
          | length s == 1 && (h `elemi` l) = Right h
          | h `notElemi` l = posErr 0 s $ utok h : (etok <$> l)
          | otherwise      = posErr 1 s [utok (s !! 1), eeof]

prop_anyChar :: String -> Property
prop_anyChar = checkChar anyChar (const True) (lbl "character")

prop_oneOf :: String -> String -> Property
prop_oneOf a = checkChar (oneOf a) (`elem` a) Nothing

prop_oneOf' :: String -> String -> Property
prop_oneOf' a = checkChar (oneOf' a) (`elemi` a) Nothing

prop_noneOf :: String -> String -> Property
prop_noneOf a = checkChar (noneOf a) (`notElem` a) Nothing

prop_noneOf' :: String -> String -> Property
prop_noneOf' a = checkChar (noneOf' a) (`notElemi` a) Nothing

prop_string :: String -> String -> Property
prop_string a = checkString (string a) a (==)

prop_string'_0 :: String -> String -> Property
prop_string'_0 a = checkString (string' a) a casei

-- | Randomly change the case in the given string.

fuzzyCase :: String -> Gen String
fuzzyCase s = zipWith f s <$> vector (length s)
  where f k True  = if isLower k then toUpper k else toLower k
        f k False = k

prop_string'_1 :: String -> Property
prop_string'_1 a = forAll (fuzzyCase a) $ \s ->
  checkString (string' a) a casei s

-- | Case-insensitive equality test for characters.

casei :: Char -> Char -> Bool
casei x y = toLower x == toLower y

-- | Case-insensitive 'elem'.

elemi :: Char -> String -> Bool
elemi c = any (casei c)

-- | Case-insensitive 'notElem'.

notElemi :: Char -> String -> Bool
notElemi c = not . elemi c

tkn :: Char -> Maybe (ErrorItem Char)
tkn = Just . Tokens . (:|[])

lbl :: String -> Maybe (ErrorItem Char)
lbl = Just . Label . NE.fromList
