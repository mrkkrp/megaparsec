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

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Char
  ( readLitChar
  , showLitChar
  , isDigit
  , isAlphaNum
  , isSpace
  , toLower )
import Data.List (findIndices, isInfixOf, find)
import Data.Maybe (listToMaybe, maybeToList, isNothing, fromJust)
import Numeric (showInt, showHex, showOct, showSigned)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Error
import Text.Megaparsec.Lexer
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Text.Megaparsec.Char as C

import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (<*>))
#endif

tests :: Test
tests = testGroup "Lexer"
        [ testProperty "space combinator"            prop_space
        , testProperty "symbol combinator"           prop_symbol
        , testProperty "symbol' combinator"          prop_symbol'
        , testProperty "indentGuard combinator"      prop_indentGuard
        , testProperty "charLiteral"                 prop_charLiteral
        , testProperty "integer"                     prop_integer
        , testProperty "decimal"                     prop_decimal
        , testProperty "hexadecimal"                 prop_hexadecimal
        , testProperty "octal"                       prop_octal
        , testProperty "float 0"                     prop_float_0
        , testProperty "float 1"                     prop_float_1
        , testProperty "number"                      prop_number
        , testProperty "signed"                      prop_signed ]

newtype WhiteSpace = WhiteSpace
  { getWhiteSpace :: String }
  deriving (Show, Eq)

instance Arbitrary WhiteSpace where
  arbitrary = WhiteSpace . concat <$> listOf whiteUnit

newtype Symbol = Symbol
  { getSymbol :: String }
  deriving (Show, Eq)

instance Arbitrary Symbol where
  arbitrary = Symbol <$> ((++) <$> symbolName <*> whiteChars)

whiteUnit :: Gen String
whiteUnit = oneof [whiteChars, whiteLine, whiteBlock]

whiteChars :: Gen String
whiteChars = listOf $ elements "\t\n "

whiteLine :: Gen String
whiteLine = commentOut <$> arbitrary `suchThat` goodEnough
  where commentOut x = "//" ++ x ++ "\n"
        goodEnough x = '\n' `notElem` x

whiteBlock :: Gen String
whiteBlock = commentOut <$> arbitrary `suchThat` goodEnough
  where commentOut x = "/*" ++ x ++ "*/"
        goodEnough x = not $ "*/" `isInfixOf` x

symbolName :: Gen String
symbolName = listOf $ arbitrary `suchThat` isAlphaNum

sc :: Parser ()
sc = space (void C.spaceChar) l b
  where l = skipLineComment "//"
        b = skipBlockComment "/*" "*/"

sc' :: Parser ()
sc' = space (void $ C.oneOf " \t") empty empty

prop_space :: WhiteSpace -> Property
prop_space w = checkParser p r s
  where p = sc
        r = Right ()
        s = getWhiteSpace w

prop_symbol :: Symbol -> Maybe Char -> Property
prop_symbol = parseSymbol (symbol sc) id

prop_symbol' :: Symbol -> Maybe Char -> Property
prop_symbol' = parseSymbol (symbol' sc) (fmap toLower)

parseSymbol :: (String -> Parser String) -> (String -> String)
            -> Symbol -> Maybe Char -> Property
parseSymbol p' f s' t = checkParser p r s
  where p = p' (f g)
        r | g == s || isSpace (last s) = Right g
          | otherwise = posErr (length s - 1) s [uneCh (last s), exEof]
        g = takeWhile (not . isSpace) s
        s = getSymbol s' ++ maybeToList t

newtype IndLine = IndLine
  { getIndLine :: String }
  deriving (Show, Eq)

instance Arbitrary IndLine where
  arbitrary = IndLine . concat <$> sequence [spc, sym, spc, eol]
    where spc = listOf (elements " \t")
          sym = return "xxx"
          eol = return "\n"

prop_indentGuard :: IndLine -> IndLine -> IndLine -> Property
prop_indentGuard l0 l1 l2 = checkParser p r s
  where p  = ip (> 1) >>= \x -> sp >> ip (== x) >> sp >> ip (> x) >> sp
        ip = indentGuard sc'
        sp = void $ symbol sc' "xxx" <* C.eol
        r | f' l0 <= 1     = posErr 0 s msg'
          | f' l1 /= f' l0 = posErr (f l1 + g [l0]) s msg'
          | f' l2 <= f' l0 = posErr (f l2 + g [l0, l1]) s msg'
          | otherwise = Right ()
        msg' = [msg "incorrect indentation"]
        f    = length . takeWhile isSpace . getIndLine
        f' x = sourceColumn $ updatePosString defaultTabWidth (initialPos "") $
               take (f x) (getIndLine x)
        g xs = sum $ length . getIndLine <$> xs
        s    = concat $ getIndLine <$> [l0, l1, l2]

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
  where (r, s) = quasiCorrupted n' i showInt "decimal integer"

prop_hexadecimal :: NonNegative Integer -> Int -> Property
prop_hexadecimal n' i = checkParser hexadecimal r s
  where (r, s) = quasiCorrupted n' i showHex "hexadecimal integer"

prop_octal :: NonNegative Integer -> Int -> Property
prop_octal n' i = checkParser octal r s
  where (r, s) = quasiCorrupted n' i showOct "octal integer"

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
          | otherwise = posErr i s $ uneCh '?' :
                        (if i <= 0 then [exCh '+', exCh '-'] else []) ++
                        [exSpec $ if isNothing . find isDigit $ take i s
                                  then "integer"
                                  else "rest of integer"]
                        ++ [exEof | i > head (findIndices isDigit s)]
        z = let bar = showSigned showInt 0 n ""
            in if n < 0 || plus then bar else '+' : bar
        s = if i <= length z then take i z ++ "?" ++ drop i z else z

quasiCorrupted :: NonNegative Integer -> Int
               -> (Integer -> String -> String) -> String
               -> (Either ParseError Integer, String)
quasiCorrupted n' i shower l = (r, s)
  where n = getNonNegative n'
        r | i > length z = Right n
          | otherwise    = posErr i s $ uneCh '?' :
                           [ exEof | i > 0 ] ++
                           [if i <= 0 || null l
                            then exSpec l
                            else exSpec $ "rest of " ++ l]
        z = shower n ""
        s = if i <= length z then take i z ++ "?" ++ drop i z else z
