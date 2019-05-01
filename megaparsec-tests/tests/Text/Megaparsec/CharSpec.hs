{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Megaparsec.CharSpec (spec) where

import Control.Monad
import Data.Char
import Data.List (nub, partition, isPrefixOf)
import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.CaseInsensitive as CI

instance Arbitrary GeneralCategory where
  arbitrary = elements [minBound..maxBound]

spec :: Spec
spec = do

  describe "newline" $
    checkStrLit "newline" "\n" (pure <$> newline)

  describe "csrf" $
    checkStrLit "crlf newline" "\r\n" crlf

  describe "eol" $ do
    context "when stream begins with a newline" $
      it "succeeds returning the newline" $
        property $ \s -> do
          let s' = '\n' : s
          prs  eol s' `shouldParse`     "\n"
          prs' eol s' `succeedsLeaving` s
    context "when stream begins with CRLF sequence" $
      it "parses the CRLF sequence" $
        property $ \s -> do
          let s' = '\r' : '\n' : s
          prs  eol s' `shouldParse`     "\r\n"
          prs' eol s' `succeedsLeaving` s
    context "when stream begins with '\\r', but it's not followed by '\\n'" $
      it "signals correct parse error" $
        property $ \ch -> ch /= '\n' ==> do
          let s = ['\r',ch]
          prs eol s `shouldFailWith` err 0 (utoks s <> elabel "end of line")
    context "when input stream is '\\r'" $
      it "signals correct parse error" $
        prs eol "\r" `shouldFailWith` err 0
          (utok '\r' <> elabel "end of line")
    context "when stream does not begin with newline or CRLF sequence" $
      it "signals correct parse error" $
        property $ \ch s -> (ch `notElem` "\r\n") ==> do
          let s' = ch : s
          prs eol s' `shouldFailWith` err 0
            (utoks (take 2 s') <> elabel "end of line")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs eol "" `shouldFailWith` err 0
          (ueof <> elabel "end of line")

  describe "tab" $
    checkStrLit "tab" "\t" (pure <$> tab)

  describe "space" $
    it "consumes space up to first non-space character" $
      property $ \s' -> do
        let (s0,s1) = partition isSpace s'
            s = s0 ++ s1
        prs  space s `shouldParse` ()
        prs' space s `succeedsLeaving` s1

  describe "space1" $ do
    context "when stream does not start with a space character" $
      it "signals correct parse error" $
        property $ \ch s' -> not (isSpace ch) ==> do
          let (s0,s1) = partition isSpace s'
              s = ch : s0 ++ s1
          prs  space1 s `shouldFailWith` err 0 (utok ch <> elabel "white space")
          prs' space1 s `failsLeaving` s
    context "when stream starts with a space character" $
      it "consumes space up to first non-space character" $
        property $ \s' -> do
          let (s0,s1) = partition isSpace s'
              s = ' ' : s0 ++ s1
          prs  space1 s `shouldParse` ()
          prs' space1 s `succeedsLeaving` s1
    context "when stream is empty" $
      it "signals correct parse error" $
        prs space1 "" `shouldFailWith` err 0 (ueof <> elabel "white space")

  describe "controlChar" $
    checkCharPred "control character" isControl controlChar

  describe "spaceChar" $
    checkCharRange "white space" " \160\t\n\r\f\v" spaceChar

  describe "upperChar" $
    checkCharPred "uppercase letter" isUpper upperChar

  describe "lowerChar" $
    checkCharPred "lowercase letter" isLower lowerChar

  describe "letterChar" $
    checkCharPred "letter" isAlpha letterChar

  describe "alphaNumChar" $
    checkCharPred "alphanumeric character" isAlphaNum alphaNumChar

  describe "printChar" $
    checkCharPred "printable character" isPrint printChar

  describe "digitChar" $
    checkCharRange "digit" ['0'..'9'] digitChar

  describe "binDigitChar" $
    checkCharRange "binary digit" ['0'..'1'] binDigitChar

  describe "octDigitChar" $
    checkCharRange "octal digit" ['0'..'7'] octDigitChar

  describe "hexDigitChar" $
    checkCharRange "hexadecimal digit" (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']) hexDigitChar

  describe "markChar" $
    checkCharRange "mark character" "\71229\7398" markChar

  describe "numberChar" $
    let xs = "\185\178\179\188\189\190" ++ ['0'..'9']
    in checkCharRange "numeric character" xs numberChar

  describe "punctuationChar" $
    checkCharPred "punctuation" isPunctuation punctuationChar

  describe "symbolChar" $
    checkCharRange "symbol" "<>$£`~|×÷^®°¸¯=¬+¤±¢¨´©¥¦" symbolChar
  describe "separatorChar" $
    checkCharRange "separator" " \160" separatorChar

  describe "asciiChar" $
    checkCharPred "ASCII character" isAscii asciiChar

  describe "latin1Char" $ do
    context "when stream begins with Latin-1 character" $
      it "parses the Latin-1 character" $
        property $ \ch s -> isLatin1 ch ==> do
          let s' = ch : s
          prs  latin1Char s' `shouldParse`     ch
          prs' latin1Char s' `succeedsLeaving` s
    context "when stream does not begin with Latin-1 character" $
      it "signals correct parse error" $ do
        prs  latin1Char "б" `shouldFailWith`
          err 0 (utok 'б' <> elabel "Latin-1 character")
        prs' latin1Char "в" `failsLeaving`   "в"
    context "when stream is empty" $
      it "signals correct parse error" $
        prs latin1Char "" `shouldFailWith` err 0 (ueof <> elabel "Latin-1 character")

  describe "charCategory" $ do
    context "when parser corresponding to general category of next char is used" $
      it "succeeds" $
        property $ \ch s -> do
          let s' = ch : s
              g  = generalCategory ch
          prs  (charCategory g) s' `shouldParse`     ch
          prs' (charCategory g) s' `succeedsLeaving` s
    context "when parser's category does not match next character's category" $
      it "fails" $
        property $ \g ch s -> (generalCategory ch /= g) ==> do
          let s' = ch : s
          prs  (charCategory g) s' `shouldFailWith`
            err 0 (utok ch <> elabel (categoryName g))
          prs' (charCategory g) s' `failsLeaving` s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \g ->
          prs (charCategory g) "" `shouldFailWith`
            err 0 (ueof <> elabel (categoryName g))

  describe "char" $ do
    context "when stream begins with the character specified as argument" $
      it "parses the character" $
        property $ \ch s -> do
          let s' = ch : s
          prs  (char ch) s' `shouldParse` ch
          prs' (char ch) s' `succeedsLeaving` s
    context "when stream does not begin with the character specified as argument" $
      it "signals correct parse error" $
        property $ \ch ch' s -> ch /= ch' ==> do
          let s' = ch' : s
          prs  (char ch) s' `shouldFailWith` err 0 (utok ch' <> etok ch)
          prs' (char ch) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \ch ->
          prs  (char ch) "" `shouldFailWith` err 0 (ueof <> etok ch)

  describe "char'" $ do
    context "when stream begins with the character specified as argument" $ do
      it "parses the character" $
        property $ \ch s -> do
          let sl = toLower ch : s
              su = toUpper ch : s
              st = toTitle ch : s
          prs  (char' ch) sl `shouldParse`     toLower ch
          prs  (char' ch) su `shouldParse`     toUpper ch
          prs  (char' ch) st `shouldParse`     toTitle ch
          prs' (char' ch) sl `succeedsLeaving` s
          prs' (char' ch) su `succeedsLeaving` s
      context "when the character is not upper or lower" $
        -- See https://ghc.haskell.org/trac/ghc/ticket/14589
        it "matches it against a form obtained via one of the conversion functions" $
          property $ \s -> do
            let ch = '\9438'
                s' = '\9412' : s
            prs (char' ch) s' `shouldParse` '\9412'
            prs' (char' ch) s' `succeedsLeaving` s
    context "when stream does not begin with the character specified as argument" $ do
      it "signals correct parse error" $
        property $ \ch ch' s -> not (casei ch ch') ==> do
          let s' = ch' : s
              ms = utok ch' <> etok (toLower ch) <> etok (toUpper ch) <> etok (toTitle ch)
          prs  (char' ch) s' `shouldFailWith` err 0 ms
          prs' (char' ch) s' `failsLeaving`   s'
      context "when the character is not upper or lower" $
        it "lists correct options in the error message" $
          property $ \ch s -> not (casei '\9438' ch) ==> do
            let ms = utok ch <> etok '\9438' <> etok '\9412'
                s' = ch : s
            prs (char' '\9438') s' `shouldFailWith` err 0 ms
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \ch -> do
          let options = etok <$> [toLower ch, toTitle ch, toUpper ch]
              ms = ueof <> mconcat (nub options)
          prs  (char' ch) "" `shouldFailWith` err 0 ms

  describe "string" $ do
    context "when stream is prefixed with given string" $
      it "parses the string" $
        property $ \str s -> do
          let s' = str ++ s
          prs  (string str) s' `shouldParse`     str
          prs' (string str) s' `succeedsLeaving` s
    context "when stream is not prefixed with given string" $
      it "signals correct parse error" $
        property $ \str s -> not (str `isPrefixOf` s) ==> do
          let us = take (length str) s
          prs (string str) s `shouldFailWith` err 0 (utoks us <> etoks str)

  describe "string'" $ do
    context "when stream is prefixed with given string" $
      it "parses the string" $
        property $ \str s ->
          forAll (fuzzyCase str) $ \str' -> do
            let s' = str' ++ s
            -- Rare tricky cases we don't want to deal with.
            when (CI.mk str /= CI.mk str') discard
            prs  (string' str) s' `shouldParse`     str'
            prs' (string' str) s' `succeedsLeaving` s
    context "when stream is not prefixed with given string" $
      it "signals correct parse error" $
        property $ \str s -> not (str `isPrefixOfI` s) ==> do
          let us = take (length str) s
          prs  (string' str) s `shouldFailWith` err 0 (utoks us <> etoks str)

----------------------------------------------------------------------------
-- Helpers

checkStrLit :: String -> String -> Parser String -> SpecWith ()
checkStrLit name ts p = do
  context ("when stream begins with " ++ name) $
    it ("parses the " ++ name) $
      property $ \s -> do
        let s' = ts ++ s
        prs  p s' `shouldParse`     ts
        prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name) $
    it "signals correct parse error" $
      property $ \ch s -> ch /= head ts ==> do
       let s' = ch : s
           us = take (length ts) s'
       prs  p s' `shouldFailWith` err 0 (utoks us <> etoks ts)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err 0 (ueof <> etoks ts)

checkCharPred :: String -> (Char -> Bool) -> Parser Char -> SpecWith ()
checkCharPred name f p = do
  context ("when stream begins with " ++ name) $
    it ("parses the " ++ name) $
      property $ \ch s -> f ch ==> do
        let s' = ch : s
        prs  p s' `shouldParse`     ch
        prs' p s' `succeedsLeaving` s
  context ("when stream does not begin with " ++ name) $
    it "signals correct parse error" $
      property $ \ch s -> not (f ch) ==> do
       let s' = ch : s
       prs  p s' `shouldFailWith` err 0 (utok ch <> elabel name)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err 0 (ueof <> elabel name)

checkCharRange :: String -> String -> Parser Char -> SpecWith ()
checkCharRange name tchs p = do
  forM_ tchs $ \tch ->
    context ("when stream begins with " ++ showTokens sproxy (nes tch)) $
      it ("parses the " ++ showTokens sproxy (nes tch)) $
        property $ \s -> do
          let s' = tch : s
          prs  p s' `shouldParse`     tch
          prs' p s' `succeedsLeaving` s
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err 0 (ueof <> elabel name)

-- | Randomly change the case in the given string.

fuzzyCase :: String -> Gen String
fuzzyCase s = zipWith f s <$> vector (length s)
  where
    f k True  = if isLower k then toUpper k else toLower k
    f k False = k

-- | The 'isPrefixOf' function takes two 'String's and returns 'True' iff
-- the first list is a prefix of the second with case-insensitive
-- comparison.

isPrefixOfI :: String -> String -> Bool
isPrefixOfI [] _  =  True
isPrefixOfI _  [] =  False
isPrefixOfI (x:xs) (y:ys) = x `casei` y && isPrefixOf xs ys

-- | Case-insensitive equality test for characters.

casei :: Char -> Char -> Bool
casei x y =
  x == toLower y ||
  x == toUpper y ||
  x == toTitle y
