{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Text.Megaparsec.Char.LexerSpec (spec) where

import Control.Monad
import qualified Data.CaseInsensitive as CI
import Data.Char hiding (ord)
import Data.List (isInfixOf)
import Data.Maybe
import Data.Scientific (Scientific, fromFloatDigits)
import Data.Void (Void)
import Numeric (showFFloatAlt, showHex, showInt, showIntAtBase, showOct)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import qualified Text.Megaparsec.Char as C
import Text.Megaparsec.Char.Lexer

#if !MIN_VERSION_base(4,13,0)
import Data.Semigroup ((<>))
#endif

spec :: Spec
spec = do
  describe "space" $
    it "consumes any sort of white space" $
      property $
        forAll mkWhiteSpace $ \s -> do
          prs scn s `shouldParse` ()
          prs' scn s `succeedsLeaving` ""

  describe "symbol" $
    context "when stream begins with the symbol" $
      it "parses the symbol and trailing whitespace" $
        property $
          forAll mkSymbol $ \s -> do
            let p = symbol scn y
                y = takeWhile (not . isSpace) s
            prs p s `shouldParse` y
            prs' p s `succeedsLeaving` ""

  describe "symbol'" $
    context "when stream begins with the symbol" $
      it "parses the symbol and trailing whitespace" $
        property $
          forAll mkSymbol $ \s -> do
            let p = symbol' scn y'
                y' = toUpper <$> y
                y = takeWhile (not . isSpace) s
            -- Rare tricky cases we don't want to deal with.
            when (CI.mk y' /= CI.mk y) discard
            prs p s `shouldParse` y
            prs' p s `succeedsLeaving` ""

  describe "skipLineComment" $ do
    context "when there is no newline at the end of line" $
      it "is picked up successfully" $ do
        let p = skipLineComment "//"
            s = "// this line comment doesn't have a newline at the end "
        prs p s `shouldParse` ()
        prs' p s `succeedsLeaving` ""
    it "inner characters are labelled properly" $ do
      let p = skipLineComment "//" <* empty
          s = "// here we go"
      prs p s `shouldFailWith` err (length s) (elabel "character")
      prs' p s `failsLeaving` ""

  describe "skipBlockComment" $
    it "skips a simple block comment" $ do
      let p = skipBlockComment "/*" "*/"
          s = "/* here we go */foo!"
      prs p s `shouldParse` ()
      prs' p s `succeedsLeaving` "foo!"

  describe "skipBlockCommentNested" $
    context "when it runs into nested block comments" $
      it "parses them all right" $ do
        let p =
              space
                (void C.spaceChar)
                empty
                (skipBlockCommentNested "/*" "*/")
                <* eof
            s = " /* foo bar /* baz */ quux */ "
        prs p s `shouldParse` ()
        prs' p s `succeedsLeaving` ""

  describe "indentLevel" $
    it "returns current indentation level (column)" $
      property $ \s w o -> do
        let p = do
              setTabWidth w
              setOffset o
              indentLevel
            c = sourceColumn (strSourcePos w (initialPos "") (take o s))
        prs p s `shouldParse` c
        prs' p s `succeedsLeaving` s

  describe "incorrectIndent" $
    it "signals correct parse error" $
      property $ \ord ref actual -> do
        let p :: Parser ()
            p = incorrectIndent ord ref actual
        prs p "" `shouldFailWith` errFancy 0 (ii ord ref actual)

  describe "indentGuard" $
    it "works as intended" $
      property $ \n -> do
        let mki = mkIndent sbla (getSmall $ getNonNegative n)
        forAll ((,,) <$> mki <*> mki <*> mki) $ \(l0, l1, l2) -> do
          let (col0, col1, col2) = (getCol l0, getCol l1, getCol l2)
              fragments = [l0, l1, l2]
              g x = sum (length <$> take x fragments)
              s = concat fragments
              p =
                ip GT pos1
                  >>= \x -> sp >> ip EQ x >> sp >> ip GT x >> sp >> scn
              ip = indentGuard scn
              sp = void (symbol sc sbla <* C.eol)
          if
              | col0 <= pos1 ->
                prs p s `shouldFailWith` errFancy 0 (ii GT pos1 col0)
              | col1 /= col0 ->
                prs p s `shouldFailWith` errFancy (getIndent l1 + g 1) (ii EQ col0 col1)
              | col2 <= col0 ->
                prs p s `shouldFailWith` errFancy (getIndent l2 + g 2) (ii GT col0 col2)
              | otherwise ->
                prs p s `shouldParse` ()

  describe "nonIdented" $
    it "works as intended" $
      property $
        forAll (mkIndent sbla 0) $ \s -> do
          let p = nonIndented scn (symbol scn sbla)
              i = getIndent s
          if i == 0
            then prs p s `shouldParse` sbla
            else prs p s `shouldFailWith` errFancy i (ii EQ pos1 (getCol s))

  describe "indentBlock" $ do
    it "works as indented" $
      property $ \mn'' -> do
        let mkBlock = do
              l0 <- mkIndent sbla 0
              l1 <- mkIndent sblb ib
              l2 <- mkIndent sblc (ib + 2)
              l3 <- mkIndent sblb ib
              l4 <- mkIndent' sblc (ib + 2)
              return (l0, l1, l2, l3, l4)
            ib = fromMaybe 2 mn'
            mn' = getSmall . getPositive <$> mn''
            mn = mkPos . fromIntegral <$> mn'
        forAll mkBlock $ \(l0, l1, l2, l3, l4) -> do
          let (col0, col1, col2, col3, col4) =
                (getCol l0, getCol l1, getCol l2, getCol l3, getCol l4)
              fragments = [l0, l1, l2, l3, l4]
              g x = sum (length <$> take x fragments)
              s = concat fragments
              p = lvla <* eof
              lvla = indentBlock scn $ IndentMany mn (l sbla) lvlb <$ b sbla
              lvlb = indentBlock scn $ IndentSome Nothing (l sblb) lvlc <$ b sblb
              lvlc = indentBlock scn $ IndentNone sblc <$ b sblc
              b = symbol sc
              l x = return . (x,)
              ib' = mkPos (fromIntegral ib)
          if
              | col1 <= col0 ->
                prs p s
                  `shouldFailWith` err (getIndent l1 + g 1) (utok (head sblb) <> eeof)
              | isJust mn && col1 /= ib' ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l1 + g 1) (ii EQ ib' col1)
              | col2 <= col1 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l2 + g 2) (ii GT col1 col2)
              | col3 == col2 ->
                prs p s
                  `shouldFailWith` err (getIndent l3 + g 3) (utoks sblb <> etoks sblc <> eeof)
              | col3 <= col0 ->
                prs p s
                  `shouldFailWith` err (getIndent l3 + g 3) (utok (head sblb) <> eeof)
              | col3 < col1 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l3 + g 3) (ii EQ col1 col3)
              | col3 > col1 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l3 + g 3) (ii EQ col2 col3)
              | col4 <= col3 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l4 + g 4) (ii GT col3 col4)
              | otherwise ->
                prs p s
                  `shouldParse` (sbla, [(sblb, [sblc]), (sblb, [sblc])])
    it "IndentMany works as intended (newline at the end)" $
      property $
        forAll ((<>) <$> mkIndent sbla 0 <*> mkWhiteSpaceNl) $ \s -> do
          let p = lvla
              lvla = indentBlock scn $ IndentMany Nothing (l sbla) lvlb <$ b sbla
              lvlb = b sblb
              b = symbol sc
              l x = return . (x,)
          prs p s `shouldParse` (sbla, [])
          prs' p s `succeedsLeaving` ""
    it "IndentMany works as intended (eof)" $
      property $
        forAll ((<>) <$> mkIndent sbla 0 <*> mkWhiteSpace) $ \s -> do
          let p = lvla
              lvla = indentBlock scn $ IndentMany Nothing (l sbla) lvlb <$ b sbla
              lvlb = b sblb
              b = symbol sc
              l x = return . (x,)
          prs p s `shouldParse` (sbla, [])
          prs' p s `succeedsLeaving` ""
    it "IndentMany works as intended (whitespace aligned precisely to the ref level)" $ do
      let p = lvla
          lvla = indentBlock scn $ IndentMany Nothing (l sbla) lvlb <$ b sbla
          lvlb = b sblb
          b = symbol sc
          l x = return . (x,)
          s = "aaa\n bbb\n "
      prs p s `shouldParse` (sbla, [sblb])
      prs' p s `succeedsLeaving` ""
    it "works with many and both IndentMany and IndentNone" $
      property $
        forAll ((<>) <$> mkIndent sbla 0 <*> mkWhiteSpaceNl) $ \s -> do
          let p1 = indentBlock scn $ IndentMany Nothing (l sbla) lvlb <$ b sbla
              p2 = indentBlock scn $ IndentNone sbla <$ b sbla
              lvlb = b sblb
              b = symbol sc
              l x = return . (x,)
          prs (many p1) s `shouldParse` [(sbla, [])]
          prs (many p2) s `shouldParse` [sbla]
          prs' (many p1) s `succeedsLeaving` ""
          prs' (many p2) s `succeedsLeaving` ""
    it "IndentSome expects the specified indentation level for first item" $ do
      let s = "aaa\n  bbb\n"
          p =
            indentBlock scn $
              IndentSome (Just (mkPos 5)) (l sbla) lvlb <$ symbol sc sbla
          lvlb = symbol sc sblb
          l x = return . (x,)
      prs p s
        `shouldFailWith` errFancy
          6
          (fancy $ ErrorIndentation EQ (mkPos 5) (mkPos 3))

  describe "lineFold" $
    it "works as intended" $
      property $ do
        let mkFold = do
              l0 <- mkInterspace sbla 0
              l1 <- mkInterspace sblb 1
              l2 <- mkInterspace sblc 1
              return (l0, l1, l2)
        forAll mkFold $ \(l0, l1, l2) -> do
          let p = lineFold scn $ \sc' -> do
                a <- symbol sc' sbla
                b <- symbol sc' sblb
                c <- symbol scn sblc
                return (a, b, c)
              getEnd x = last x == '\n'
              fragments = [l0, l1, l2]
              g x = sum (length <$> take x fragments)
              s = concat fragments
              (col0, col1, col2) = (getCol l0, getCol l1, getCol l2)
              (end0, end1) = (getEnd l0, getEnd l1)
          if
              | end0 && col1 <= col0 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l1 + g 1) (ii GT col0 col1)
              | end1 && col2 <= col0 ->
                prs p s
                  `shouldFailWith` errFancy (getIndent l2 + g 2) (ii GT col0 col2)
              | otherwise -> prs p s `shouldParse` (sbla, sblb, sblc)

  describe "charLiteral" $ do
    let p = charLiteral
    context "when stream begins with a literal character" $
      it "parses it" $
        property $ \ch -> do
          let s = showLitChar ch ""
          prs p s `shouldParse` ch
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with a literal character" $
      it "signals correct parse error" $
        do
          let s = "\\"
          prs p s `shouldFailWith` err 0 (utok '\\' <> elabel "literal character")
          prs' p s `failsLeaving` s
    context "when stream is empty" $
      it "signals correct parse error" $
        prs p "" `shouldFailWith` err 0 (ueof <> elabel "literal character")
    context "when given a long escape sequence" $
      it "parses it correctly" $
        property $ \s' -> do
          let s = "\\1114111\\&" ++ s'
          prs p s `shouldParse` '\1114111'
          prs' p s `succeedsLeaving` s'

  describe "decimal" $ do
    context "when stream begins with decimal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = decimal :: Parser Integer
              n = getNonNegative n'
              s = showInt n ""
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with decimal digits" $
      it "signals correct parse error" $
        property $ \a as ->
          not (isDigit a) ==> do
            let p = decimal :: Parser Integer
                s = a : as
            prs p s `shouldFailWith` err 0 (utok a <> elabel "integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (decimal :: Parser Integer) ""
          `shouldFailWith` err 0 (ueof <> elabel "integer")

  describe "binary" $ do
    context "when stream begins with binary digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = binary :: Parser Integer
              n = getNonNegative n'
              s = showIntAtBase 2 intToDigit n ""
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with binary digits" $
      it "signals correct parse error" $
        property $ \a as ->
          a /= '0' && a /= '1' ==> do
            let p = binary :: Parser Integer
                s = a : as
            prs p s
              `shouldFailWith` err 0 (utok a <> elabel "binary integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (binary :: Parser Integer) ""
          `shouldFailWith` err 0 (ueof <> elabel "binary integer")

  describe "octal" $ do
    context "when stream begins with octal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = octal :: Parser Integer
              n = getNonNegative n'
              s = showOct n ""
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with octal digits" $
      it "signals correct parse error" $
        property $ \a as ->
          not (isOctDigit a) ==> do
            let p = octal :: Parser Integer
                s = a : as
            prs p s
              `shouldFailWith` err 0 (utok a <> elabel "octal integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (octal :: Parser Integer) ""
          `shouldFailWith` err 0 (ueof <> elabel "octal integer")

  describe "hexadecimal" $ do
    context "when stream begins with hexadecimal digits" $
      it "they are parsed as an integer" $
        property $ \n' -> do
          let p = hexadecimal :: Parser Integer
              n = getNonNegative n'
              s = showHex n ""
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with hexadecimal digits" $
      it "signals correct parse error" $
        property $ \a as ->
          not (isHexDigit a) ==> do
            let p = hexadecimal :: Parser Integer
                s = a : as
            prs p s
              `shouldFailWith` err 0 (utok a <> elabel "hexadecimal integer")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (hexadecimal :: Parser Integer) ""
          `shouldFailWith` err 0 (ueof <> elabel "hexadecimal integer")

  describe "scientific" $ do
    context "when stream begins with a number" $
      it "parses it" $
        property $ \n' -> do
          let p = scientific :: Parser Scientific
              s =
                either
                  (show . getNonNegative)
                  (show . getNonNegative)
                  (n' :: Either (NonNegative Integer) (NonNegative Double))
          prs p s `shouldParse` case n' of
            Left x -> fromIntegral (getNonNegative x)
            Right x -> fromFloatDigits (getNonNegative x)
          prs' p s `succeedsLeaving` ""
    context "when fractional part is interrupted" $
      it "signals correct parse error" $
        property $ \(NonNegative n) -> do
          let p = scientific <* empty :: Parser Scientific
              s = showFFloatAlt Nothing (n :: Double) ""
          prs p s
            `shouldFailWith` err
              (length s)
              (etok 'E' <> etok 'e' <> elabel "digit")
          prs' p s `failsLeaving` ""
    context "when whole part is followed by a dot without valid fractional part" $
      it "parsing of fractional part is backtracked correctly" $
        property $ \(NonNegative n) -> do
          let p = scientific :: Parser Scientific
              s = showInt (n :: Integer) ".err"
          prs p s `shouldParse` fromIntegral n
          prs' p s `succeedsLeaving` ".err"
    context "when number is followed by something starting with 'e'" $
      it "parsing of exponent part is backtracked correctly" $
        property $ \(NonNegative n) -> do
          let p = scientific :: Parser Scientific
              s = showFFloatAlt Nothing (n :: Double) "err!"
          prs p s `shouldParse` fromFloatDigits n
          prs' p s `succeedsLeaving` "err!"
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (scientific :: Parser Scientific) ""
          `shouldFailWith` err 0 (ueof <> elabel "digit")

  describe "float" $ do
    context "when stream begins with a float" $
      it "parses it" $
        property $ \n' -> do
          let p = float :: Parser Double
              n = getNonNegative n'
              s = show n
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when stream does not begin with a float" $
      it "signals correct parse error" $
        property $ \a as ->
          not (isDigit a) ==> do
            let p = float :: Parser Double
                s = a : as
            prs p s
              `shouldFailWith` err 0 (utok a <> elabel "digit")
            prs' p s `failsLeaving` s
    context "when stream begins with an integer (decimal)" $
      it "signals correct parse error" $
        property $ \n' -> do
          let p = float :: Parser Double
              n = getNonNegative n'
              s = show (n :: Integer)
          prs p s
            `shouldFailWith` err
              (length s)
              (ueof <> etok '.' <> etok 'E' <> etok 'e' <> elabel "digit")
          prs' p s `failsLeaving` ""
    context "when number is followed by something starting with 'e'" $
      it "parsing of exponent part is backtracked correctly" $
        property $ \(NonNegative n) -> do
          let p = float :: Parser Double
              s = showFFloatAlt Nothing (n :: Double) "err!"
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` "err!"
    context "when stream is empty" $
      it "signals correct parse error" $
        prs (float :: Parser Double) ""
          `shouldFailWith` err 0 (ueof <> elabel "digit")
    context "when there is float with just exponent" $
      it "parses it all right" $
        do
          let p = float :: Parser Double
          prs p "123e3" `shouldParse` 123e3
          prs' p "123e3" `succeedsLeaving` ""
          prs p "123e+3" `shouldParse` 123e+3
          prs' p "123e+3" `succeedsLeaving` ""
          prs p "123e-3" `shouldParse` 123e-3
          prs' p "123e-3" `succeedsLeaving` ""

  describe "signed" $ do
    context "with integer" $
      it "parses signed integers" $
        property $ \n -> do
          let p :: Parser Integer
              p = signed (hidden C.space) decimal
              s = show n
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "with float" $
      it "parses signed floats" $
        property $ \n -> do
          let p :: Parser Double
              p = signed (hidden C.space) float
              s = show n
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "with scientific" $
      it "parses singed scientific numbers" $
        property $ \n -> do
          let p = signed (hidden C.space) scientific
              s = either show show (n :: Either Integer Double)
          prs p s `shouldParse` case n of
            Left x -> fromIntegral x
            Right x -> fromFloatDigits x
    context "when number is prefixed with plus sign" $
      it "parses the number" $
        property $ \n' -> do
          let p :: Parser Integer
              p = signed (hidden C.space) decimal
              n = getNonNegative n'
              s = '+' : show n
          prs p s `shouldParse` n
          prs' p s `succeedsLeaving` ""
    context "when number is prefixed with white space" $
      it "signals correct parse error" $
        property $ \n -> do
          let p :: Parser Integer
              p = signed (hidden C.space) decimal
              s = ' ' : show (n :: Integer)
          prs p s
            `shouldFailWith` err
              0
              (utok ' ' <> etok '+' <> etok '-' <> elabel "integer")
          prs' p s `failsLeaving` s
    context "when there is white space between sign and digits" $
      it "parses it all right" $ do
        let p :: Parser Integer
            p = signed (hidden C.space) decimal
            s = "- 123"
        prs p s `shouldParse` (-123)
        prs' p s `succeedsLeaving` ""

----------------------------------------------------------------------------
-- Helpers

mkWhiteSpace :: Gen String
mkWhiteSpace = concat <$> listOf whiteUnit
  where
    whiteUnit = oneof [whiteChars, whiteLine, whiteBlock]

mkWhiteSpaceNl :: Gen String
mkWhiteSpaceNl = (<>) <$> mkWhiteSpace <*> pure "\n"

mkSymbol :: Gen String
mkSymbol = (++) <$> symbolName <*> whiteChars

mkInterspace :: String -> Int -> Gen String
mkInterspace x n = oneof [si, mkIndent x n]
  where
    si = (++ x) <$> listOf (elements " \t")

mkIndent :: String -> Int -> Gen String
mkIndent x n = (++) <$> mkIndent' x n <*> eol
  where
    eol = frequency [(5, return "\n"), (1, (scaleDown . listOf1 . return) '\n')]

mkIndent' :: String -> Int -> Gen String
mkIndent' x n = concat <$> sequence [spc, sym, tra]
  where
    spc = frequency [(5, vectorOf n itm), (1, scaleDown (listOf itm))]
    tra = scaleDown (listOf itm)
    itm = elements " \t"
    sym = return x

whiteChars :: Gen String
whiteChars = scaleDown $ listOf (elements "\t\n ")

whiteLine :: Gen String
whiteLine = commentOut <$> arbitrary `suchThat` goodEnough
  where
    commentOut x = "//" ++ x ++ "\n"
    goodEnough x = '\n' `notElem` x

whiteBlock :: Gen String
whiteBlock = commentOut <$> arbitrary `suchThat` goodEnough
  where
    commentOut x = "/*" ++ x ++ "*/"
    goodEnough x = not $ "*/" `isInfixOf` x

symbolName :: Gen String
symbolName = listOf $ arbitrary `suchThat` isAlphaNum

sc :: Parser ()
sc = space (void $ takeWhile1P Nothing f) empty empty
  where
    f x = x == ' ' || x == '\t'

scn :: Parser ()
scn = space C.space1 l b
  where
    l = skipLineComment "//"
    b = skipBlockComment "/*" "*/"

getIndent :: String -> Int
getIndent = length . takeWhile isSpace

getCol :: String -> Pos
getCol x =
  sourceColumn
    . strSourcePos defaultTabWidth (initialPos "")
    $ takeWhile isSpace x

sbla, sblb, sblc :: String
sbla = "aaa"
sblb = "bbb"
sblc = "ccc"

ii :: Ordering -> Pos -> Pos -> EF Void
ii ord ref actual = fancy (ErrorIndentation ord ref actual)
