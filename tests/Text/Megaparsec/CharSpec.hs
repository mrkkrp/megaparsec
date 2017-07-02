{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Megaparsec.CharSpec (spec) where

import Control.Monad
import Data.Char
import Data.List (partition, isPrefixOf)
import Data.Monoid ((<>))
import Data.Void
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

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
          prs eol s `shouldFailWith` err posI (utoks s <> elabel "end of line")
    context "when input stream is '\\r'" $
      it "signals correct parse error" $
        prs eol "\r" `shouldFailWith` err posI
          (utok '\r' <> elabel "end of line")
    context "when stream does not begin with newline or CRLF sequence" $
      it "signals correct parse error" $
        property $ \ch s -> (ch `notElem` "\r\n") ==> do
          let s' = ch : s
          prs eol s' `shouldFailWith` err posI
            (utoks (take 2 s') <> elabel "end of line")
    context "when stream is empty" $
      it "signals correct parse error" $
        prs eol "" `shouldFailWith` err posI
          (ueof <> elabel "end of line")

  describe "tab" $
    checkStrLit "tab" "\t" (pure <$> tab)

  describe "space" $
    it "consumes it up to first non-space character" $
      property $ \s -> do
        let (s0,s1) = partition isSpace s
            s' = s0 ++ s1
        prs  space s' `shouldParse` ()
        prs' space s' `succeedsLeaving` s1

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

  describe "octDigitChar" $
    checkCharRange "octal digit" ['0'..'7'] octDigitChar

  describe "hexDigitChar" $
    checkCharRange "hexadecimal digit" (['0'..'9'] ++ ['a'..'f'] ++ ['A'..'F']) hexDigitChar

  describe "markChar" $
#if MIN_VERSION_base(4,9,0)
    checkCharRange "mark character" "\71229\7398" markChar
#else
    checkCharRange "mark character" "" markChar
#endif

  describe "numberChar" $
    let xs = "\185\178\179\188\189\190" ++ ['0'..'9']
    in checkCharRange "numeric character" xs numberChar

  describe "punctuationChar" $
    checkCharPred "punctuation" isPunctuation punctuationChar

  describe "symbolChar" $
#if MIN_VERSION_base(4,8,0)
    checkCharRange "symbol" "<>$£`~|×÷^®°¸¯=¬+¤±¢¨´©¥¦" symbolChar
#else
    checkCharRange "symbol" "<>$£`~|×÷^®°¸¯=¬+¤±¢¨´©¥¦§¶" symbolChar
#endif
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
          err posI (utok 'б' <> elabel "Latin-1 character")
        prs' latin1Char "в" `failsLeaving`   "в"
    context "when stream is empty" $
      it "signals correct parse error" $
        prs latin1Char "" `shouldFailWith` err posI (ueof <> elabel "Latin-1 character")

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
            err posI (utok ch <> elabel (categoryName g))
          prs' (charCategory g) s' `failsLeaving` s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \g ->
          prs (charCategory g) "" `shouldFailWith`
            err posI (ueof <> elabel (categoryName g))

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
          prs  (char ch) s' `shouldFailWith` err posI (utok ch' <> etok ch)
          prs' (char ch) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \ch ->
          prs  (char ch) "" `shouldFailWith` err posI (ueof <> etok ch)

  describe "char'" $ do
    context "when stream begins with the character specified as argument" $
      it "parses the character" $
        property $ \ch s -> do
          let sl = toLower ch : s
              su = toUpper ch : s
          prs  (char' ch) sl `shouldParse`     toLower ch
          prs  (char' ch) su `shouldParse`     toUpper ch
          prs' (char' ch) sl `succeedsLeaving` s
          prs' (char' ch) su `succeedsLeaving` s
    context "when stream does not begin with the character specified as argument" $
      it "signals correct parse error" $
        property $ \ch ch' s -> toLower ch /= toLower ch' ==> do
          let s' = ch' : s
              ms = utok ch' <> etok (toLower ch) <> etok (toUpper ch)
          prs  (char' ch) s' `shouldFailWith` err posI ms
          prs' (char' ch) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \ch -> do
          let ms = ueof <> etok (toLower ch) <> etok (toUpper ch)
          prs  (char' ch) "" `shouldFailWith` err posI ms

  describe "anyChar" $ do
    context "when stream is not empty" $
      it "succeeds consuming next character in the stream" $
        property $ \ch s -> do
          let s' = ch : s
          prs  anyChar s' `shouldParse`     ch
          prs' anyChar s' `succeedsLeaving` s
    context "when stream is empty" $
      it "signals correct parse error" $
        prs anyChar "" `shouldFailWith` err posI (ueof <> elabel "character")

  describe "oneOf" $ do
    context "when stream begins with one of specified characters" $
      it "parses the character" $
        property $ \chs' n s -> do
          let chs = getNonEmpty chs'
              ch  = chs !! (getNonNegative n `rem` length chs)
              s'  = ch : s
          prs  (oneOf chs) s' `shouldParse`     ch
          prs' (oneOf chs) s' `succeedsLeaving` s
    context "when stream does not begin with any of specified characters" $
      it "signals correct parse error" $
        property $ \chs ch s  -> ch `notElem` (chs :: String) ==> do
          let s' = ch : s
          prs  (oneOf chs) s' `shouldFailWith` err posI (utok ch)
          prs' (oneOf chs) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \chs ->
          prs (oneOf (chs :: String)) "" `shouldFailWith` err posI ueof

  describe "noneOf" $ do
    context "when stream does not begin with any of specified characters" $
      it "parses the character" $
        property $ \chs ch s  -> ch `notElem` (chs :: String) ==> do
          let s' = ch : s
          prs  (noneOf chs) s' `shouldParse`     ch
          prs' (noneOf chs) s' `succeedsLeaving` s
    context "when stream begins with one of specified characters" $
      it "signals correct parse error" $
        property $ \chs' n s -> do
          let chs = getNonEmpty chs'
              ch  = chs !! (getNonNegative n `rem` length chs)
              s'  = ch : s
          prs  (noneOf chs) s' `shouldFailWith` err posI (utok ch)
          prs' (noneOf chs) s' `failsLeaving`   s'
    context "when stream is empty" $
      it "signals correct parse error" $
        property $ \chs ->
          prs (noneOf (chs :: String)) "" `shouldFailWith` err posI ueof

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
          prs (string str) s `shouldFailWith`
            err posI (utoks us <> etoks str)

  describe "string'" $ do
    context "when stream is prefixed with given string" $
      it "parses the string" $
        property $ \str s ->
          forAll (fuzzyCase str) $ \str' -> do
            let s' = str' ++ s
            prs  (string' str) s' `shouldParse`     str'
            prs' (string' str) s' `succeedsLeaving` s
    context "when stream is not prefixed with given string" $
      it "signals correct parse error" $
        property $ \str s -> not (str `isPrefixOfI` s) ==> do
          let us = take (length str) s
          prs  (string' str) s `shouldFailWith`
            err posI (utoks us <> etoks str)

----------------------------------------------------------------------------
-- Helpers

checkStrLit :: String -> String -> Parsec Void String String -> SpecWith ()
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
       prs  p s' `shouldFailWith` err posI (utoks us <> etoks ts)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> etoks ts)

checkCharPred :: String -> (Char -> Bool) -> Parsec Void String Char -> SpecWith ()
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
       prs  p s' `shouldFailWith` err posI (utok ch <> elabel name)
       prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> elabel name)

checkCharRange :: String -> String -> Parsec Void String Char -> SpecWith ()
checkCharRange name tchs p = do
  forM_ tchs $ \tch ->
    context ("when stream begins with " ++ showTokens (nes tch)) $
      it ("parses the " ++ showTokens (nes tch)) $
        property $ \s -> do
          let s' = tch : s
          prs  p s' `shouldParse`     tch
          prs' p s' `succeedsLeaving` s
  -- context ("when stream does not begin with " ++ name) $
  --   it "signals correct parse error" $
  --     property $ \ch s -> ch `notElem` tchs ==> do
  --      let s' = ch : s
  --      prs  p s' `shouldFailWith` err posI (utok ch <> elabel name)
  --      prs' p s' `failsLeaving`   s'
  context "when stream is empty" $
    it "signals correct parse error" $
      prs p "" `shouldFailWith` err posI (ueof <> elabel name)

-- | Randomly change the case in the given string.

fuzzyCase :: String -> Gen String
fuzzyCase s = zipWith f s <$> vector (length s)
  where f k True  = if isLower k then toUpper k else toLower k
        f k False = k

-- | Case-insensitive equality test for characters.

casei :: Char -> Char -> Bool
casei x y = toUpper x == toUpper y

-- | The 'isPrefixOf' function takes two 'String's and returns 'True' iff
-- the first list is a prefix of the second with case-insensitive
-- comparison.

isPrefixOfI :: String -> String -> Bool
isPrefixOfI [] _  =  True
isPrefixOfI _  [] =  False
isPrefixOfI (x:xs) (y:ys) = x `casei` y && isPrefixOf xs ys
