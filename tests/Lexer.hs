--
-- QuickCheck tests for Megaparsec's lexer.
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

{-# LANGUAGE TupleSections #-}

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
import Data.Maybe
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

import Prim () -- 'Arbitrary' instance for 'SourcePos'
import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (<*>), (<$))
#endif

tests :: Test
tests = testGroup "Lexer"
        [ testProperty "space combinator"       prop_space
        , testProperty "symbol combinator"      prop_symbol
        , testProperty "symbol' combinator"     prop_symbol'
        , testProperty "indentLevel"            prop_indentLevel
        , testProperty "indentGuard combinator" prop_indentGuard
        , testProperty "nonIndented combinator" prop_nonIndented
        , testProperty "indentBlock combinator" prop_indentBlock
        , testProperty "indentBlock (many)"     prop_indentMany
        , testProperty "charLiteral"            prop_charLiteral
        , testProperty "integer"                prop_integer
        , testProperty "decimal"                prop_decimal
        , testProperty "hexadecimal"            prop_hexadecimal
        , testProperty "octal"                  prop_octal
        , testProperty "float 0"                prop_float_0
        , testProperty "float 1"                prop_float_1
        , testProperty "number 0"               prop_number_0
        , testProperty "number 1"               prop_number_1
        , testProperty "number 2 (signed)"      prop_number_2
        , testProperty "signed"                 prop_signed ]

-- White space

mkWhiteSpace :: Gen String
mkWhiteSpace = concat <$> listOf whiteUnit
  where whiteUnit = oneof [whiteChars, whiteLine, whiteBlock]

mkSymbol :: Gen String
mkSymbol = (++) <$> symbolName <*> whiteChars

mkIndent :: String -> Int -> Gen String
mkIndent x n = (++) <$> mkIndent' x n <*> eol
  where eol = frequency [(5, return "\n"), (1, listOf1 (return '\n'))]

mkIndent' :: String -> Int -> Gen String
mkIndent' x n = concat <$> sequence [spc, sym, tra]
  where spc = frequency [(5, vectorOf n itm), (1, listOf itm)]
        tra = listOf itm
        itm = elements " \t"
        sym = return x

whiteChars :: Gen String
whiteChars = listOf (elements "\t\n ")

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

prop_space :: Property
prop_space = forAll mkWhiteSpace (checkParser p r)
  where p = sc
        r = Right ()

prop_symbol :: Maybe Char -> Property
prop_symbol t = forAll mkSymbol $ \s ->
  parseSymbol (symbol sc) id s t

prop_symbol' :: Maybe Char -> Property
prop_symbol' t = forAll mkSymbol $ \s ->
  parseSymbol (symbol' sc) (fmap toLower) s t

parseSymbol
  :: (String -> Parser String)
  -> (String -> String)
  -> String
  -> Maybe Char
  -> Property
parseSymbol p' f s' t = checkParser p r s
  where p = p' (f g)
        r | g == s || isSpace (last s) = Right g
          | otherwise = posErr (length s - 1) s [uneCh (last s), exEof]
        g = takeWhile (not . isSpace) s
        s = s' ++ maybeToList t

-- Indentation

prop_indentLevel :: SourcePos -> Property
prop_indentLevel pos = p /=\ sourceColumn pos
  where p = setPosition pos >> indentLevel

prop_indentGuard :: NonNegative Int -> Property
prop_indentGuard n =
  forAll ((,,) <$> mki <*> mki <*> mki) $ \(l0,l1,l2) ->
    let r | getCol l0 <= 1         = posErr 0 s ii
          | getCol l1 /= getCol l0 = posErr (getIndent l1 + g 1) s ii
          | getCol l2 <= getCol l0 = posErr (getIndent l2 + g 2) s ii
          | otherwise = Right ()
        fragments = [l0,l1,l2]
        g x = sum (length <$> take x fragments)
        s = concat fragments
    in checkParser p r s
  where mki = mkIndent sbla (getNonNegative n)
        p  = ip (> 1) >>= \x -> sp >> ip (== x) >> sp >> ip (> x) >> sp >> sc
        ip = indentGuard sc
        sp = void (symbol sc' sbla <* C.eol)

prop_nonIndented :: Property
prop_nonIndented = forAll (mkIndent sbla 0) $ \s ->
  let i = getIndent s
      r | i == 0    = Right sbla
        | otherwise = posErr i s ii
  in checkParser p r s
  where p = nonIndented sc (symbol sc sbla)

prop_indentBlock :: Maybe (Positive Int) -> Property
prop_indentBlock mn' = forAll mkBlock $ \(l0,l1,l2,l3,l4) ->
  let r | getCol l1 <= getCol l0 =
          posErr (getIndent l1 + g 1) s [uneCh (head sblb), exEof]
        | isJust mn && getCol l1 /= ib =
          posErr (getIndent l1 + g 1) s ii
        | getCol l2 <= getCol l1 =
          posErr (getIndent l2 + g 2) s ii
        | getCol l3 == getCol l2 =
          posErr (getIndent l3 + g 3) s [uneCh (head sblb), exStr sblc]
        | getCol l3 <= getCol l0 =
          posErr (getIndent l3 + g 3) s [uneCh (head sblb), exEof]
        | getCol l3 /= getCol l1 =
          posErr (getIndent l3 + g 3) s ii
        | getCol l4 <= getCol l3 =
          posErr (getIndent l4 + g 4) s ii
        | otherwise = Right (sbla, [(sblb, [sblc]), (sblb, [sblc])])
      fragments = [l0,l1,l2,l3,l4]
      g x = sum (length <$> take x fragments)
      s = concat fragments
  in checkParser p r s
  where mkBlock = do
          l0 <- mkIndent sbla 0
          l1 <- mkIndent sblb ib
          l2 <- mkIndent sblc (ib + 2)
          l3 <- mkIndent sblb ib
          l4 <- mkIndent' sblc (ib + 2)
          return (l0,l1,l2,l3,l4)
        p = lvla
        lvla = indentBlock sc $ IndentMany mn      (l sbla) lvlb <$ b sbla
        lvlb = indentBlock sc $ IndentSome Nothing (l sblb) lvlc <$ b sblb
        lvlc = indentBlock sc $ IndentNone                  sblc <$ b sblc
        b    = symbol sc'
        l x  = return . (x,)
        mn   = getPositive <$> mn'
        ib   = fromMaybe 2 mn

prop_indentMany :: Property
prop_indentMany = forAll (mkIndent sbla 0) (checkParser p r)
  where r = Right (sbla, [])
        p = lvla
        lvla = indentBlock sc $ IndentMany Nothing (l sbla) lvlb <$ b sbla
        lvlb = b sblb
        b    = symbol sc'
        l x  = return . (x,)

getIndent :: String -> Int
getIndent = length . takeWhile isSpace

getCol :: String -> Int
getCol x = sourceColumn $
  updatePosString defaultTabWidth (initialPos "") $ take (getIndent x) x

sbla, sblb, sblc :: String
sbla = "xxx"
sblb = "yyy"
sblc = "zzz"

ii :: [Message]
ii = [msg "incorrect indentation"]

-- Character and string literals

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

-- Numbers

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

prop_number_0 :: Either (NonNegative Integer) (NonNegative Double) -> Property
prop_number_0 n' = checkParser number r s
  where r = Right $ case n' of
                      Left  x -> Left  $ getNonNegative x
                      Right x -> Right $ getNonNegative x
        s = either (show . getNonNegative) (show . getNonNegative) n'

prop_number_1 :: Property
prop_number_1 = checkParser number r s
  where r = posErr 0 s [uneEof, exSpec "number"]
        s = ""

prop_number_2 :: Either Integer Double -> Property
prop_number_2 n = checkParser p (Right n) s
  where p = signed (hidden C.space) number
        s = either show show n

prop_signed :: Integer -> Int -> Bool -> Property
prop_signed n i plus = checkParser p r s
  where p = signed (hidden C.space) integer
        r | i > length z = Right n
          | otherwise = posErr i s $ uneCh '?' :
            (if i <= 0 then [exCh '+', exCh '-'] else []) ++
            [exSpec $ if isNothing . find isDigit $ take i s
                      then "integer"
                      else "rest of integer"] ++
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
          | otherwise    = posErr i s $ uneCh '?' :
            [ exEof | i > 0 ] ++
            [if i <= 0 || null l
             then exSpec l
             else exSpec $ "rest of " ++ l]
        z = shower n ""
        s = if i <= length z then take i z ++ "?" ++ drop i z else z
