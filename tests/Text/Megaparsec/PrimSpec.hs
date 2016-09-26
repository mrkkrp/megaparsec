--
-- Tests for Megaparsec's primitive parser combinators.
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

{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Text.Megaparsec.PrimSpec (spec) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char (toUpper, chr)
import Data.Foldable (asum, concat)
import Data.List (isPrefixOf, foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Proxy
import Data.Word (Word8)
import Prelude hiding (span, concat)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck hiding (label)
import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.String
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S
import qualified Data.ByteString.Char8       as B
import qualified Data.ByteString.Lazy.Char8  as BL
import qualified Data.List.NonEmpty          as NE
import qualified Data.Set                    as E
import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL

spec :: Spec
spec = do

  describe "non-String instances of Stream" $ do
    context "lazy ByteString" $ do
      it "unconses correctly" $
        property $ \ch' n -> do
          let p  = many (char ch) :: Parsec Dec BL.ByteString String
              s  = replicate (getNonNegative n) ch
              ch = byteToChar ch'
          parse p "" (BL.pack s) `shouldParse` s
      it "updates position like with String" $
        property $ \w pos ch ->
          updatePos (Proxy :: Proxy BL.ByteString) w pos ch `shouldBe`
          updatePos (Proxy :: Proxy String) w pos ch
    context "strict ByteString" $ do
      it "unconses correctly" $
        property $ \ch' n -> do
          let p  = many (char ch) :: Parsec Dec B.ByteString String
              s  = replicate (getNonNegative n) ch
              ch = byteToChar ch'
          parse p "" (B.pack s) `shouldParse` s
      it "updates position like with String" $
        property $ \w pos ch ->
          updatePos (Proxy :: Proxy B.ByteString) w pos ch `shouldBe`
          updatePos (Proxy :: Proxy String) w pos ch
    context "lazy Text" $ do
      it "unconses correctly" $
        property $ \ch n -> do
          let p = many (char ch) :: Parsec Dec TL.Text String
              s = replicate (getNonNegative n) ch
          parse p "" (TL.pack s) `shouldParse` s
      it "updates position like with String" $
        property $ \w pos ch ->
          updatePos (Proxy :: Proxy TL.Text) w pos ch `shouldBe`
          updatePos (Proxy :: Proxy String) w pos ch
    context "strict Text" $ do
      it "unconses correctly" $
        property $ \ch n -> do
          let p = many (char ch) :: Parsec Dec T.Text String
              s = replicate (getNonNegative n) ch
          parse p "" (T.pack s) `shouldParse` s
      it "updates position like with String" $
        property $ \w pos ch ->
          updatePos (Proxy :: Proxy T.Text) w pos ch `shouldBe`
          updatePos (Proxy :: Proxy String) w pos ch

  describe "position in custom stream" $ do

    describe "eof" $
      it "updates position in stream correctly" $
        property $ \st -> (not . null . stateInput) st ==> do
          let p = eof :: CustomParser ()
              h = head (stateInput st)
              apos = let (_:|z) = statePos st in spanStart h :| z
          runParser' p st `shouldBe`
            ( st { statePos = apos }
            , Left (err apos $ utok h <> eeof) )

    describe "token" $
      it "updates position in stream correctly" $
        property $ \st@State {..} span -> do
          let p = pSpan span
              h = head stateInput
              (apos, npos) =
                let z = NE.tail statePos
                in (spanStart h :| z, spanEnd h :| z)
          if | null stateInput -> runParser' p st `shouldBe`
               ( st
               , Left (err statePos $ ueof <> etok span) )
             | spanBody h == spanBody span -> runParser' p st `shouldBe`
               ( st { statePos = npos
                    , stateInput = tail stateInput }
               , Right span )
             | otherwise -> runParser' p st `shouldBe`
               ( st { statePos = apos}
               , Left (err apos $ utok h <> etok span))

    describe "tokens" $
      it "updates position is stream correctly" $
        property $ \st' ts -> forAll (incCoincidence st' ts) $ \st@State {..} -> do
          let p = tokens compareTokens ts :: CustomParser [Span]
              compareTokens x y = spanBody x == spanBody y
              updatePos' = updatePos (Proxy :: Proxy [Span]) stateTabWidth
              il = length . takeWhile id $ zipWith compareTokens stateInput ts
              tl = length ts
              consumed = take il stateInput
              (apos, npos) =
                let (pos:|z) = statePos
                in ( spanStart (head stateInput) :| z
                   , foldl' (\q t -> snd (updatePos' q t)) pos consumed :| z )
          if | null ts -> runParser' p st `shouldBe` (st, Right [])
             | null stateInput -> runParser' p st `shouldBe`
               ( st
               , Left (err statePos $ ueof <> etoks ts) )
             | il == tl -> runParser' p st `shouldBe`
               ( st { statePos   = npos
                    , stateInput = drop (length ts) stateInput }
               , Right consumed )
             | otherwise -> runParser' p st `shouldBe`
               ( st { statePos = apos }
               , Left (err apos $ utoks (take (il + 1) stateInput) <> etoks ts) )

  describe "ParsecT Functor instance" $ do
    it "obeys identity law" $
      property $ \n ->
        prs (fmap id (pure (n :: Int))) "" ===
        prs (id (pure n))               ""
    it "obeys composition law" $
      property $ \n m t ->
        let f = (+ m)
            g = (* t)
        in prs (fmap (f . g) (pure (n :: Int))) "" ===
           prs ((fmap f . fmap g) (pure n))     ""

  describe "ParsecT Applicative instance" $ do
    it "obeys identity law" $
      property $ \n ->
        prs (pure id <*> pure (n :: Int)) "" ===
        prs (pure n) ""
    it "obeys composition law" $
      property $ \n m t ->
        let u = pure (+ m)
            v = pure (* t)
            w = pure (n :: Int)
        in prs (pure (.) <*> u <*> v <*> w) "" ===
           prs (u <*> (v <*> w)) ""
    it "obeys homomorphism law" $
      property $ \x m ->
        let f = (+ m)
        in prs (pure f <*> pure (x :: Int)) "" ===
           prs (pure (f x)) ""
    it "obeys interchange law" $
      property $ \n y ->
        let u = pure (+ n)
        in prs (u <*> pure (y :: Int)) "" ===
           prs (pure ($ y) <*> u) ""
    describe "(<*>)" $
      context "when first parser succeeds without consuming" $
        context "when second parser fails consuming input" $
          it "fails consuming input" $ do
            let p = m <*> n
                m = return (\x -> 'a' : x)
                n = string "bc" <* empty
                s = "bc"
            prs  p s `shouldFailWith` err (posN (4 :: Int) s) mempty
            prs' p s `failsLeaving`   ""
    describe "(*>)" $
      it "works correctly" $
        property $ \n m ->
          let u = pure (+ (m :: Int))
              v = pure (n :: Int)
          in prs (u *> v) "" ===
             prs (pure (const id) <*> u <*> v) ""
    describe "(<*)" $
      it "works correctly" $
        property $ \n m ->
          let u = pure (m :: Int)
              v = pure (+ (n :: Int))
          in prs (u <* v) "" === prs (pure const <*> u <*> v) ""

  describe "ParsecT Alternative instance" $ do

    describe "empty" $
      it "always fails" $
        property $ \n ->
          prs (empty <|> pure n) "" `shouldParse` (n :: Integer)

    describe "(<|>)" $ do
      context "with two strings" $ do
        context "stream begins with the first string" $
          it "parses the string" $
            property $ \s0 s1 s -> not (s1 `isPrefixOf` s0) ==> do
              let s' = s0 ++ s
                  p = string s0 <|> string s1
              prs  p s' `shouldParse` s0
              prs' p s' `succeedsLeaving` s
        context "stream begins with the second string" $
          it "parses the string" $
            property $ \s0 s1 s -> not (s0 `isPrefixOf` s1) && not (s0 `isPrefixOf` s) ==> do
              let s' = s1 ++ s
                  p = string s0 <|> string s1
              prs  p s' `shouldParse` s1
              prs' p s' `succeedsLeaving` s
        context "when stream does not begin with either string" $
          it "signals correct error message" $
            property $ \s0 s1 s -> not (s0 `isPrefixOf` s) && not (s1 `isPrefixOf` s) ==> do
              let p = string s0 <|> string s1
                  z0' = toFirstMismatch (==) s0 s
                  z1' = toFirstMismatch (==) s1 s
              prs  p s `shouldFailWith` err posI
                (etoks s0 <>
                 etoks s1 <>
                 (if null s then ueof else mempty) <>
                 (if null z0' then mempty else utoks z0') <>
                 (if null z1' then mempty else utoks z1'))
      context "with two complex parsers" $ do
        context "when stream begins with matching character" $
          it "parses it" $
            property $ \a b -> a /= b ==> do
              let p = char a <|> (char b *> char a)
                  s = [a]
              prs  p s `shouldParse` a
              prs' p s `succeedsLeaving` ""
        context "when stream begins with only one matching character" $
          it "signals correct parse error" $
            property $ \a b c -> a /= b && a /= c ==> do
              let p = char a <|> (char b *> char a)
                  s = [b,c]
              prs  p s `shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok a)
              prs' p s `failsLeaving` [c]
        context "when stream begins with not matching character" $
          it "signals correct parse error" $
            property $ \a b c -> a /= b && a /= c && b /= c ==> do
              let p = char a <|> (char b *> char a)
                  s = [c,b]
              prs  p s `shouldFailWith` err posI (utok c <> etok a <> etok b)
              prs' p s `failsLeaving` s
        context "when stream is emtpy" $
          it "signals correct parse error" $
            property $ \a b -> do
              let p = char a <|> (char b *> char a)
              prs  p "" `shouldFailWith` err posI (ueof <> etok a <> etok b)
      it "associativity of fold over alternatives should not matter" $ do
        let p  = asum [empty, string ">>>", empty, return "foo"] <?> "bar"
            p' = bsum [empty, string ">>>", empty, return "foo"] <?> "bar"
            bsum = foldl (<|>) empty
            s  = ">>"
        prs p s `shouldBe` prs p' s

    describe "many" $ do
      context "when stream begins with things argument of many parses" $
        it "they are parsed" $
          property $ \a' b' c' -> do
            let [a,b,c] = getNonNegative <$> [a',b',c']
                p = many (char 'a')
                s = abcRow a b c
            prs  p s `shouldParse` replicate a 'a'
            prs' p s `succeedsLeaving` drop a s
      context "when stream does not begin with thing argument of many parses" $
        it "does nothing" $
          property $ \a' b' c' -> do
            let [a,b,c] = getNonNegative <$> [a',b',c']
                p = many (char 'd')
                s = abcRow a b c
            prs  p s `shouldParse` ""
            prs' p s `succeedsLeaving` s
      context "when stream is empty" $
        it "succeeds parsing nothing" $ do
          let p = many (char 'a')
          prs  p "" `shouldParse` ""
      context "when there are two many combinators in a row that parse nothing" $
        it "accumulated hints are reflected in parse error" $ do
          let p = many (char 'a') *> many (char 'b') *> eof
          prs p "c" `shouldFailWith` err posI
            (utok 'c' <> etok 'a' <> etok 'b' <> eeof)

    describe "some" $ do
      context "when stream begins with things argument of some parses" $
        it "they are parsed" $
          property $ \a' b' c' -> do
            let a = getPositive a'
                [b,c] = getNonNegative <$> [b',c']
                p = some (char 'a')
                s = abcRow a b c
            prs  p s `shouldParse` replicate a 'a'
            prs' p s `succeedsLeaving` drop a s
      context "when stream does not begin with thing argument of some parses" $
        it "signals correct parse error" $
          property $ \a' b' c' -> do
            let [a,b,c] = getNonNegative <$> [a',b',c']
                p = some (char 'd')
                s = abcRow a b c ++ "g"
            prs  p s `shouldFailWith` err posI (utok (head s) <> etok 'd')
            prs' p s `failsLeaving` s
      context "when stream is empty" $
        it "signals correct parse error" $
          property $ \ch -> do
            let p = some (char ch)
            prs  p "" `shouldFailWith` err posI (ueof <> etok ch)
    context "optional" $ do
      context "when stream begins with that optional thing" $
        it "parses it" $
          property $ \a b -> do
            let p = optional (char a) <* char b
                s = [a,b]
            prs  p s `shouldParse` Just a
            prs' p s `succeedsLeaving` ""
      context "when stream does not begin with that optional thing" $
        it "succeeds parsing nothing" $
          property $ \a b -> a /= b ==> do
            let p = optional (char a) <* char b
                s = [b]
            prs  p s `shouldParse` Nothing
            prs' p s `succeedsLeaving` ""
      context "when stream is empty" $
        it "succeeds parsing nothing" $
          property $ \a -> do
            let p = optional (char a)
            prs  p "" `shouldParse` Nothing

  describe "ParsecT Monad instance" $ do
    it "satisfies left identity law" $
      property $ \a k' -> do
        let k = return . (+ k')
            p = return (a :: Int) >>= k
        prs p "" `shouldBe` prs (k a) ""
    it "satisfies right identity law" $
      property $ \a -> do
        let m = return (a :: Int)
            p = m >>= return
        prs p "" `shouldBe` prs m ""
    it "satisfies associativity law" $
      property $ \m' k' h' -> do
        let m = return (m' :: Int)
            k = return . (+ k')
            h = return . (* h')
            p = m >>= (\x -> k x >>= h)
            p' = (m >>= k) >>= h
        prs p "" `shouldBe` prs p' ""
    it "fails signals correct parse error" $
      property $ \msg -> do
        let p = fail msg :: Parsec Dec String ()
        prs p "" `shouldFailWith` err posI (cstm (DecFail msg))
    it "pure is the same as return" $
      property $ \n ->
        prs (pure (n :: Int)) "" `shouldBe` prs (return n) ""
    it "(<*>) is the same as ap" $
      property $ \m' k' -> do
        let m = return (m' :: Int)
            k = return (+ k')
        prs (k <*> m) "" `shouldBe` prs (k `ap` m) ""

  describe "ParsecT MonadFail instance" $
    describe "fail" $
      it "signals correct parse error" $
        property $ \s msg -> do
          let p = void (fail msg)
          prs  p s `shouldFailWith` err posI (cstm $ DecFail msg)
          prs' p s `failsLeaving` s

  describe "ParsecT MonadIO instance" $
    it "liftIO works" $
      property $ \n -> do
        let p = liftIO (return n) :: ParsecT Dec String IO Integer
        runParserT p "" "" `shouldReturn` Right n

  describe "ParsecT MonadReader instance" $ do

    describe "ask" $
      it "returns correct value of context" $
        property $ \n -> do
          let p = ask :: ParsecT Dec String (Reader Integer) Integer
          runReader (runParserT p "" "") n `shouldBe` Right n

    describe "local" $
      it "modifies reader context correctly" $
        property $ \n k -> do
          let p = local (+ k) ask :: ParsecT Dec String (Reader Integer) Integer
          runReader (runParserT p "" "") n `shouldBe` Right (n + k)

  describe "ParsecT MonadState instance" $ do

    describe "get" $
      it "returns correct state value" $
        property $ \n -> do
          let p = L.get :: ParsecT Dec String (L.State Integer) Integer
          L.evalState (runParserT p "" "") n `shouldBe` Right n
    describe "put" $
      it "replaces state value" $
        property $ \a b -> do
          let p = L.put b :: ParsecT Dec String (L.State Integer) ()
          L.execState (runParserT p "" "") a `shouldBe` b

  describe "ParsecT MonadCont instance" $

    describe "callCC" $
      it "works properly" $
        property $ \a b -> do
          let p :: ParsecT Dec String (Cont (Either (ParseError Char Dec) Integer)) Integer
              p = callCC $ \e -> when (a > b) (e a) >> return b
          runCont (runParserT p "" "") id `shouldBe` Right (max a b)

  describe "ParsecT MonadError instance" $ do

    describe "throwError" $
      it "throws the error" $
        property $ \a b -> do
          let p :: ParsecT Dec String (Except Integer) Integer
              p = throwError a >> return b
          runExcept (runParserT p "" "") `shouldBe` Left a

    describe "catchError" $
      it "catches the error" $
        property $ \a b -> do
          let p :: ParsecT Dec String (Except Integer) Integer
              p = (throwError a >> return b) `catchError` handler
              handler e = return (e + b)
          runExcept (runParserT p "" "") `shouldBe` Right (Right $ a + b)

  describe "primitive combinators" $ do

    describe "unexpected" $
      it "signals correct parse error" $
        property $ \item -> do
          let p :: MonadParsec Dec String m => m ()
              p = void (unexpected item)
          grs p "" (`shouldFailWith` ParseError
            { errorPos        = posI
            , errorUnexpected = E.singleton item
            , errorExpected   = E.empty
            , errorCustom     = E.empty })

    describe "failure" $
      it "signals correct parse error" $
        property $ \us ps xs -> do
          let p :: MonadParsec Dec String m => m ()
              p = void (failure us ps xs)
          grs p "" (`shouldFailWith` ParseError
            { errorPos        = posI
            , errorUnexpected = us
            , errorExpected   = ps
            , errorCustom     = xs })

    describe "label" $ do
      context "when inner parser succeeds consuming input" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Dec String m => m Char
                  p = label lbl (char a) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
        context "inner parser produces hints" $
          it "replaces the last hint with “rest of <label>”" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Dec String m => m String
                  p = label lbl (many (char a)) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (elabel $ "rest of " ++ lbl))
              grs' p s (`failsLeaving` "")
      context "when inner parser consumes and fails" $
        it "reports parse error without modification" $
          property $ \lbl a b c -> not (null lbl) && b /= c ==> do
            let p :: MonadParsec Dec String m => m Char
                p = label lbl (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Dec String m => m Char
                  p = label lbl (return a) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
        context "inner parser produces hints" $
          it "replaces the last hint with given label" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Dec String m => m String
                  p = label lbl (many (char a)) <* empty
              grs p "" (`shouldFailWith` err posI (elabel lbl))
      context "when inner parser fails without consuming" $
        it "is mentioned in parse error via its label" $
          property $ \lbl -> not (null lbl) ==> do
            let p :: MonadParsec Dec String m => m ()
                p = label lbl empty
            grs p "" (`shouldFailWith` err posI (elabel lbl))

    describe "hidden" $ do
      context "when inner parser succeeds consuming input" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \a -> do
              let p :: MonadParsec Dec String m => m Char
                  p = hidden (char a) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
        context "inner parser produces hints" $
          it "hides the parser in the error message" $
            property $ \a -> do
              let p :: MonadParsec Dec String m => m String
                  p = hidden (many (char a)) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
      context "when inner parser consumes and fails" $
        it "reports parse error without modification" $
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Dec String m => m Char
                p = hidden (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \a -> do
              let p :: MonadParsec Dec String m => m Char
                  p = hidden (return a) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
        context "inner parser produces hints" $
          it "hides the parser in the error message" $
            property $ \a -> do
              let p :: MonadParsec Dec String m => m String
                  p = hidden (many (char a)) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
      context "when inner parser fails without consuming" $
        it "hides the parser in the error message" $ do
          let p :: MonadParsec Dec String m => m ()
              p = hidden empty
          grs p "" (`shouldFailWith` err posI mempty)

    describe "try" $ do
      context "when inner parser succeeds consuming" $
        it "try has no effect" $
          property $ \a -> do
            let p :: MonadParsec Dec String m => m Char
                p = try (char a)
                s = [a]
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` "")
      context "when inner parser fails consuming" $
        it "backtracks, it appears as if the parser has not consumed anything" $
        -- TODO also check that it backtracks state in general as well
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Dec String m => m Char
                p = try (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c]) -- FIXME it should leave entire input
      context "when inner parser succeeds without consuming" $
        it "try has no effect" $
          property $ \a -> do
            let p :: MonadParsec Dec String m => m Char
                p = try (return a)
            grs p "" (`shouldParse` a)
      context "when inner parser fails without consuming" $
        it "try has no effect" $ do
          let p :: MonadParsec Dec String m => m Char
              p = try empty
          grs p "" (`shouldFailWith` err posI mempty)

    describe "lookAhead" $ do
      context "when inner parser succeeds consuming" $
        it "result is returned but parser state is not changed" $
          property $ \a w -> do
            let p :: MonadParsec Dec String m => m Pos
                p = lookAhead (setTabWidth w *> char a) *> getTabWidth
                s = [a]
            grs  p s (`shouldParse` defaultTabWidth)
            grs' p s (`succeedsLeaving` s)
      context "when inner parser fails consuming" $
        it "error message is reported as usual" $
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Dec String m => m Char
                p = lookAhead (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $
        it "result is returned but parser state in not changed" $
          property $ \a w -> do
            let p :: MonadParsec Dec String m => m Pos
                p = lookAhead (setTabWidth w *> char a) *> getTabWidth
                s = [a]
            grs  p s (`shouldParse` defaultTabWidth)
            grs' p s (`succeedsLeaving` s)
      context "when inner parser fails without consuming" $
        it "error message is reported as usual" $ do
          let p :: MonadParsec Dec String m => m Char
              p = lookAhead empty
          grs p "" (`shouldFailWith` err posI mempty)

    describe "notFollowedBy" $ do
      context "when inner parser succeeds consuming" $
        it "signals correct parse error" $
          property $ \a w -> do
            let p :: MonadParsec Dec String m => m ()
                p = notFollowedBy (setTabWidth w <* char a)
                s = [a]
            grs  p s (`shouldFailWith` err posI (utok a))
            grs' p s (`failsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
      context "when inner parser fails consuming" $
        it "succeeds without consuming" $
          property $ \a b c w -> b /= c ==> do
            let p :: MonadParsec Dec String m => m ()
                p = notFollowedBy (setTabWidth w *> char a *> char b)
                s = [a,c]
            grs' p s (`succeedsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
      context "when inner parser succeeds without consuming" $
        it "signals correct parse error" $
          property $ \a w -> do
            let p :: MonadParsec Dec String m => m ()
                p = notFollowedBy (setTabWidth w *> return a)
                s = [a]
            grs  p s (`shouldFailWith` err posI (utok a))
            grs' p s (`failsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
      context "when inner parser fails without consuming" $
        it "succeeds without consuming" $
          property $ \w -> do
            let p :: MonadParsec Dec String m => m ()
                p = notFollowedBy (setTabWidth w *> empty)
            grs  p "" (`shouldParse` ())
            grs' p "" ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)

    describe "withRecovery" $ do
      context "when inner parser succeeds consuming" $
        it "the result is returned as usual" $
          property $ \a as -> do
            let p :: MonadParsec Dec String m => m (Maybe Char)
                p = withRecovery (const $ return Nothing) (pure <$> char a)
                s = a : as
            grs  p s (`shouldParse` Just a)
            grs' p s (`succeedsLeaving` as)
      context "when inner parser fails consuming" $ do
        context "when recovering parser succeeds consuming input" $
          it "its result is returned and position is advanced" $ -- TODO check hints
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ string (c : as))
                        (Right <$> char a <* char b)
                  -- p' :: MonadParsec Dec String m => m (Either (ParseError Char Dec) String)
                  -- p' = p <* empty
                  s = a : c : as
                  -- l = fromIntegral (length s)
              grs  p s (`shouldParse` Left (err (posN (1 :: Int) s) (utok c <> etok b)))
              grs' p s (`succeedsLeaving` "")
              -- grs p' s (`shouldFailWith` err (posN l s) mempty)
        context "when recovering parser fails consuming input" $
          it "the original parse error (and state) is reported" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ char b <* empty)
                        (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
              grs' p s (`failsLeaving` (c : as))
        context "when recovering parser succeeds without consuming" $
          it "its result is returned (and state)" $ -- TODO check hints
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (return . Left) (Right <$> char a <* char b)
                  -- p' :: MonadParsec Dec String m => m (Either (ParseError Char Dec) String)
                  -- p' = p <* empty
                  s = a : c : as
              grs  p s (`shouldParse` Left (err (posN (1 :: Int) s) (utok c <> etok b)))
              grs' p s (`succeedsLeaving` (c : as))
              -- grs p' s (`shouldFailWith` err (posN 1 s) (etok a))
        context "when recovering parser fails without consuming" $
          it "the original parse error (and state) is reported" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ empty)
                        (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
              grs' p s (`failsLeaving` (c : as))
      context "when inner parser succeeds without consuming" $
        it "the result is returned as usual" $
          property $ \a s -> do
            let p :: MonadParsec Dec String m => m (Maybe Char)
                p = withRecovery (const $ return Nothing) (return a)
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` s)
      context "when inner parser fails without consuming" $ do
        context "when recovering parser succeeds consuming input" $
          it "its result is returned and position is advanced" $ -- TODO check hints
            property $ \a as -> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ string s) empty
                  s = a : as
              grs  p s (`shouldParse` Left (err posI mempty))
              grs' p s (`succeedsLeaving` "")
        context "when recovering parser fails consuming input" $
          it "the original parse error (and state) is reported" $
            property $ \a b as -> a /= b ==> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ char a <* char b <* empty)
                        (Right <$> empty)
                  s = a : as
              grs  p s (`shouldFailWith` err posI mempty)
              grs' p s (`failsLeaving` s)
        context "when recovering parser succeeds without consuming" $
          it "its result is returned (and state)" $ -- TODO check hints
            property $ \s -> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (return . Left) empty
              grs  p s (`shouldParse` Left (err posI mempty))
              grs' p s (`succeedsLeaving` s)
        context "when recovering parser fails without consuming" $
          it "the original parse error (and state) is reported" $
            property $ \s -> do
              let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) Char)
                  p = withRecovery (\e -> Left e <$ empty) empty
              grs  p s (`shouldFailWith` err posI mempty)
              grs' p s (`failsLeaving` s)
      it "works in complex situations too" $
        property $ \a' b' c' -> do
          let p :: MonadParsec Dec String m => m (Either (ParseError Char Dec) String)
              p = let g = count' 1 3 . char in v <$>
                withRecovery (\e -> Left e <$ g 'b') (Right <$> g 'a') <*> g 'c'
              v (Right x) y = Right (x ++ y)
              v (Left  m) _ = Left m
              ma = if a < 3 then etok 'a' else mempty
              s = abcRow a b c
              [a,b,c] = getNonNegative <$> [a',b',c']
              f = flip shouldFailWith
              z = flip shouldParse
              r | a == 0 && b == 0 && c == 0 = f (err posI (ueof <> etok 'a'))
                | a == 0 && b == 0 && c >  3 = f (err posI (utok 'c' <> etok 'a'))
                | a == 0 && b == 0           = f (err posI (utok 'c' <> etok 'a'))
                | a == 0 && b >  3           = f (err (posN (3 :: Int) s) (utok 'b' <> etok 'a' <> etok 'c'))
                | a == 0 &&           c == 0 = f (err (posN b s) (ueof <> etok 'a' <> etok 'c'))
                | a == 0 &&           c >  3 = f (err (posN (b + 3) s) (utok 'c' <> eeof))
                | a == 0                     = z (Left (err posI (utok 'b' <> etok 'a')))
                | a >  3                     = f (err (posN (3 :: Int) s) (utok 'a' <> etok 'c'))
                |           b == 0 && c == 0 = f (err (posN a s) (ueof <> etok 'c' <> ma))
                |           b == 0 && c >  3 = f (err (posN (a + 3) s) (utok 'c' <> eeof))
                |           b == 0           = z (Right s)
                | otherwise                  = f (err (posN a s) (utok 'b' <> etok 'c' <> ma))
          grs (p <* eof) s r

    describe "eof" $ do
      context "when input stream is empty" $
        it "succeeds" $
          grs eof "" (`shouldParse` ())
      context "when input stream is not empty" $
        it "signals correct error message" $
          property $ \a as -> do
            let s = a : as
            grs  eof s (`shouldFailWith` err posI (utok a <> eeof))
            grs' eof s (`failsLeaving` s)

    describe "token" $ do
      let f x = E.singleton (Tokens $ nes x)
          testChar a x = if x == a then Right x else Left (f x, f a, E.empty)
      context "when supplied predicate is satisfied" $
        it "succeeds" $
          property $ \a as mtok -> do
            let p :: MonadParsec Dec String m => m Char
                p = token (testChar a) mtok
                s = a : as
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` as)
      context "when supplied predicate is not satisfied" $
        it "signals correct parse error" $
          property $ \a b as mtok -> a /= b ==> do
            let p :: MonadParsec Dec String m => m Char
                p = token (testChar b) mtok
                s = a : as
            grs  p s (`shouldFailWith` ParseError
              { errorPos        = posI
              , errorUnexpected = E.singleton (Tokens $ nes a)
              , errorExpected   = E.singleton (Tokens $ nes b)
              , errorCustom     = E.empty })
            grs' p s (`failsLeaving` s)
      context "when stream is empty" $
        it "signals correct parse error" $
          property $ \a mtok -> do
            let p :: MonadParsec Dec String m => m Char
                p = token (testChar a) mtok
            grs p "" (`shouldFailWith` ParseError
              { errorPos        = posI
              , errorUnexpected = E.singleton EndOfInput
              , errorExpected   = maybe E.empty (E.singleton . Tokens . nes) mtok
              , errorCustom     = E.empty })

    describe "tokens" $ do
      context "when stream is prefixed with given string" $
        it "parses the string" $
          property $ \str s -> do
            let p :: MonadParsec Dec String m => m String
                p = tokens (==) str
                s' = str ++ s
            grs  p s' (`shouldParse` str)
            grs' p s' (`succeedsLeaving` s)
      context "when stream is not prefixed with given string" $
        it "signals correct parse error" $
          property $ \str s -> not (str `isPrefixOf` s) ==> do
            let p :: MonadParsec Dec String m => m String
                p = tokens (==) str
                z = toFirstMismatch (==) str s
            grs  p s (`shouldFailWith` err posI (utoks z <> etoks str))
            grs' p s (`failsLeaving` s)

  describe "combinators for manipulating parser state" $ do

    describe "setPosition and getPosition" $
      it "sets position and gets it back" $
        property $ \st pos -> do
          let p :: Parser SourcePos
              p = setPosition pos >> getPosition
              f (State s (_:|xs) w) y = State s (y:|xs) w
          runParser' p st `shouldBe` (f st pos, Right pos)

    describe "pushPosition" $
      it "adds a layer to position stack and parser continues on that level" $
        property $ \st pos ->  do
          let p :: Parser ()
              p = pushPosition pos
          fst (runParser' p st) `shouldBe`
            st { statePos = NE.cons pos (statePos st) }

    describe "popPosition" $
      it "removes a layer from position stack" $
        property $ \st -> do
          let p :: Parser ()
              p = popPosition
              pos = statePos st
          fst (runParser' p st) `shouldBe`
            st { statePos = fromMaybe pos (snd (NE.uncons pos)) }

    describe "setInput and getInput" $
      it "sets input and gets it back" $
        property $ \s -> do
          let p = do
                st0 <- getInput
                guard (null st0)
                setInput s
                result <- string s
                st1 <- getInput
                guard (null st1)
                return result
          prs p "" `shouldParse` s

    describe "setTabWidth and getTabWidth" $
      it "sets tab width and gets it back" $
        property $ \w -> do
          let p = setTabWidth w >> getTabWidth
          prs p "" `shouldParse` w

    describe "setParserState and getParserState" $
      it "sets parser state and gets it back" $
        property $ \s1 s2 -> do
          let p :: MonadParsec Dec String m => m (State String)
              p = do
                st <- getParserState
                guard (st == State s posI defaultTabWidth)
                setParserState s1
                updateParserState (f s2)
                liftM2 const getParserState (setInput "")
              f (State s1' pos w) (State s2' _ _) = State (max s1' s2') pos w
              s = ""
          grs p s (`shouldParse` f s2 s1)

  describe "running a parser" $ do
    describe "parseMaybe" $
      it "returns result on success and Nothing on failure" $
        property $ \s s' -> do
          let p = string s' :: Parser String
          parseMaybe p s `shouldBe`
            if s == s' then Just s else Nothing

    describe "runParser'" $
      it "works" $
        property $ \st s -> do
          let p = string s
          runParser' p st `shouldBe` emulateStrParsing st s

    describe "runParserT'" $
      it "works" $
        property $ \st s -> do
          let p = string s
          runIdentity (runParserT' p st) `shouldBe` emulateStrParsing st s

  describe "MonadParsec instance of ReaderT" $ do

    describe "try" $
      it "generally works" $
        property $ \pre ch1 ch2 -> do
          let s1 = pre : [ch1]
              s2 = pre : [ch2]
              getS1 = asks fst
              getS2 = asks snd
              p = try (g =<< getS1) <|> (g =<< getS2)
              g = sequence . fmap char
              s = [pre]
          prs (runReaderT p (s1, s2)) s `shouldFailWith`
            err (posN (1 :: Int) s) (ueof <> etok ch1 <> etok ch2)

    describe "notFollowedBy" $
      it "generally works" $
        property $ \a' b' c' -> do
          let p = many (char =<< ask) <* notFollowedBy eof <* many anyChar
              [a,b,c] = getNonNegative <$> [a',b',c']
              s = abcRow a b c
          if b > 0 || c > 0
            then prs (runReaderT p 'a') s `shouldParse` replicate a 'a'
            else prs (runReaderT p 'a') s `shouldFailWith`
                   err (posN a s) (ueof <> etok 'a')

  describe "MonadParsec instance of lazy StateT" $ do

    describe "(<|>)" $
      it "generally works" $
        property $ \n -> do
          let p = L.put n >>
                ((L.modify (* 2) >> void (string "xxx")) <|> return ()) >> L.get
          prs (L.evalStateT p 0) "" `shouldParse` (n :: Integer)

    describe "lookAhead" $
      it "generally works" $
        property $ \n -> do
          let p = L.put n >> lookAhead (L.modify (* 2) >> eof) >> S.get
          prs (L.evalStateT p 0) "" `shouldParse` (n :: Integer)

    describe "notFollowedBy" $
      it "generally works" $
        property $ \n -> do
          let p = do
                L.put n
                let notEof = notFollowedBy (L.modify (* 2) >> eof)
                some (try (anyChar <* notEof)) <* char 'x'
          prs (L.runStateT p 0) "abx" `shouldParse` ("ab", n :: Integer)

  describe "MonadParsec instance of strict StateT" $ do

    describe "(<|>)" $
      it "generally works" $
        property $ \n -> do
          let p = S.put n >>
                ((S.modify (* 2) >> void (string "xxx")) <|> return ()) >> S.get
          prs (S.evalStateT p 0) "" `shouldParse` (n :: Integer)

    describe "lookAhead" $
      it "generally works" $
        property $ \n -> do
          let p = S.put n >> lookAhead (S.modify (* 2) >> eof) >> S.get
          prs (S.evalStateT p 0) "" `shouldParse` (n :: Integer)

    describe "notFollowedBy" $
      it "generally works" $
        property $ \n -> do
          let p = do
                S.put n
                let notEof = notFollowedBy (S.modify (* 2) >> eof)
                some (try (anyChar <* notEof)) <* char 'x'
          prs (S.runStateT p 0) "abx" `shouldParse` ("ab", n :: Integer)

  describe "MonadParsec instance of lazy WriterT" $
    it "generally works" $
      property $ \pre post -> do
        let loggedLetter = letterChar >>= \x -> L.tell [x] >> return x
            loggedEof    = eof >> L.tell "EOF"
            p = do
              L.tell pre
              cs <- L.censor (fmap toUpper) $
                some (try (loggedLetter <* notFollowedBy loggedEof))
              L.tell post
              void loggedLetter
              return cs
        prs (L.runWriterT p) "abx" `shouldParse` ("ab", pre ++ "AB" ++ post ++ "x")

  describe "MonadParsec instance of strict WriterT" $
    it "generally works" $
      property $ \pre post -> do
        let loggedLetter = letterChar >>= \x -> S.tell [x] >> return x
            loggedEof    = eof >> S.tell "EOF"
            p = do
              S.tell pre
              cs <- L.censor (fmap toUpper) $
                some (try (loggedLetter <* notFollowedBy loggedEof))
              S.tell post
              void loggedLetter
              return cs
        prs (S.runWriterT p) "abx" `shouldParse` ("ab", pre ++ "AB" ++ post ++ "x")

----------------------------------------------------------------------------
-- Helpers

byteToChar :: Word8 -> Char
byteToChar = chr . fromIntegral

-- | This data type represents tokens in custom input stream.

data Span = Span
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  , spanBody  :: NonEmpty Char
  } deriving (Eq, Ord, Show)

instance Stream [Span] where
  type Token [Span] = Span
  uncons [] = Nothing
  uncons (t:ts) = Just (t, ts)
  updatePos _ _ _ (Span start end _) = (start, end)

instance Arbitrary Span where
  arbitrary = do
    start <- arbitrary
    end   <- arbitrary `suchThat` (> start)
    Span start end <$>
#if !MIN_VERSION_QuickCheck(2,9,0)
      (NE.fromList . getNonEmpty <$> arbitrary)
#else
      arbitrary
#endif

instance ShowToken Span where
  showTokens ts = concat (NE.toList . spanBody <$> ts)

type CustomParser = Parsec Dec [Span]

pSpan :: Span -> CustomParser Span
pSpan span = token testToken (Just span)
  where
    f = E.singleton . Tokens . nes
    testToken x =
      if spanBody x == spanBody span
        then Right span
        else Left (f x, f span , E.empty)

incCoincidence :: State [Span] -> [Span] -> Gen (State [Span])
incCoincidence st ts = do
  n <- getSmall <$> arbitrary
  let (pre, post) = splitAt n (stateInput st)
      pre' = zipWith (\x t -> x { spanBody = spanBody t }) pre ts
  return st { stateInput = pre' ++ post }

emulateStrParsing
  :: State String
  -> String
  -> (State String, Either (ParseError Char Dec) String)
emulateStrParsing st@(State i (pos:|z) t) s =
  if l == length s
    then (State (drop l i) (updatePosString t pos s :| z) t, Right s)
    else (st, Left $ err (pos:|z) (etoks s <> utoks (take (l + 1) i)))
  where l = length (takeWhile id $ zipWith (==) s i)
