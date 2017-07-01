{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE RecursiveDo       #-}
{-# LANGUAGE TypeFamilies      #-}
{-# OPTIONS -fno-warn-orphans  #-}

module Text.MegaparsecSpec (spec) where

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Char (toUpper)
import Data.Foldable (asum, concat)
import Data.Function (on)
import Data.List (isPrefixOf)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe, listToMaybe, isJust)
import Data.Monoid
import Data.Proxy
import Data.Void
import Prelude hiding (span, concat)
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck hiding (label)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Control.Monad.RWS.Lazy      as L
import qualified Control.Monad.RWS.Strict    as S
import qualified Control.Monad.State.Lazy    as L
import qualified Control.Monad.State.Strict  as S
import qualified Control.Monad.Writer.Lazy   as L
import qualified Control.Monad.Writer.Strict as S
import qualified Data.List                   as DL
import qualified Data.List.NonEmpty          as NE
import qualified Data.Semigroup              as G
import qualified Data.Set                    as E

#if !MIN_VERSION_QuickCheck(2,8,2)
instance (Arbitrary a, Ord a) => Arbitrary (E.Set a) where
  arbitrary = E.fromList <$> arbitrary
  shrink    = fmap E.fromList . shrink . E.toList
#endif

spec :: Spec
spec = do

  -- TODO This should be moved in some form to Text.Megaparsec.StreamSpec

  -- describe "non-String instances of Stream" $ do
  --   context "lazy ByteString" $ do
  --     it "unconses correctly" $
  --       property $ \ch' n -> do
  --         let p  = many (char ch) :: Parsec Void BL.ByteString String
  --             s  = replicate (getNonNegative n) ch
  --             ch = byteToChar ch'
  --         parse p "" (BL.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy BL.ByteString) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "strict ByteString" $ do
  --     it "unconses correctly" $
  --       property $ \ch' n -> do
  --         let p  = many (char ch) :: Parsec Void B.ByteString String
  --             s  = replicate (getNonNegative n) ch
  --             ch = byteToChar ch'
  --         parse p "" (B.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy B.ByteString) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "lazy Text" $ do
  --     it "unconses correctly" $
  --       property $ \ch n -> do
  --         let p = many (char ch) :: Parsec Void TL.Text String
  --             s = replicate (getNonNegative n) ch
  --         parse p "" (TL.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy TL.Text) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch
  --   context "strict Text" $ do
  --     it "unconses correctly" $
  --       property $ \ch n -> do
  --         let p = many (char ch) :: Parsec Void T.Text String
  --             s = replicate (getNonNegative n) ch
  --         parse p "" (T.pack s) `shouldParse` s
  --     it "updates position like with String" $
  --       property $ \w pos ch ->
  --         updatePos (Proxy :: Proxy T.Text) w pos ch `shouldBe`
  --         updatePos (Proxy :: Proxy String) w pos ch

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

    describe "token" $ do
      context "when input stream is empty" $
        it "signals correct parse error" $
          property $ \st'@State {..} span -> do
            let p = pSpan span
                st = (st' :: State [Span]) { stateInput = [] }
            runParser' p st `shouldBe`
              ( st
              , Left (err statePos $ ueof <> etok span) )
      context "when head of stream matches" $
        it "updates parser state correctly" $
          property $ \st'@State {..} span -> do
            let p = pSpan span
                st = st' { stateInput = span : stateInput }
                npos = spanEnd span :| NE.tail statePos
            runParser' p st `shouldBe`
              ( st { statePos             = npos
                   , stateTokensProcessed = stateTokensProcessed + 1
                   , stateInput           = stateInput }
              , Right span )
      context "when head of stream does not match" $ do
        let checkIt s span =
              let ms = listToMaybe s
              in isJust ms && (spanBody <$> ms) /= Just (spanBody span)
        it "signals correct parse error" $
          property $ \st@State {..} span -> checkIt stateInput span ==> do
            let p = pSpan span
                h = head stateInput
                apos = spanStart h :| NE.tail statePos
            runParser' p st `shouldBe`
              ( st { statePos = apos }
              , Left (err apos $ utok h <> etok span))

    describe "tokens" $
      it "updates position is stream correctly" $
        property $ \st' ts -> forAll (incCoincidence st' ts) $ \st@State {..} -> do
          let p = tokens compareTokens ts :: CustomParser [Span]
              compareTokens x y = and $ zipWith compareToken x y
              compareToken = (==) `on` spanBody
              -- updatePos' = updatePos (Proxy :: Proxy [Span]) stateTabWidth

              il = length . takeWhile id $ zipWith compareToken stateInput ts
              tl = length ts

              consumed = take tl stateInput
              (apos, npos) =
                let (pos:|z) = statePos
                    pxy = Proxy :: Proxy [Span]
                in ( positionAt1 pxy pos (head stateInput) :| z
                   , advanceN pxy stateTabWidth pos consumed :| z )
          if | null ts -> runParser' p st `shouldBe` (st, Right [])
             | null stateInput -> runParser' p st `shouldBe`
               ( st
               , Left (err statePos $ ueof <> etoks ts) )
             | il == tl -> runParser' p st `shouldBe`
               ( st { statePos             = npos
                    , stateTokensProcessed = stateTokensProcessed + fromIntegral tl
                    , stateInput           = drop (length ts) stateInput }
               , Right consumed )
             | otherwise -> runParser' p st `shouldBe`
               ( st { statePos = apos }
               , Left (err apos $ utoks (take (il + 1) stateInput) <> etoks ts) )

    describe "getNextTokenPosition" $ do
      context "when input stream is empty" $
        it "returns Nothing" $
          property $ \st' -> do
            let p :: CustomParser (Maybe SourcePos)
                p = getNextTokenPosition
                st = (st' :: State [Span]) { stateInput = [] }
            runParser' p st `shouldBe` (st, Right Nothing)
      context "when input stream is not empty" $
        it "return the position of start of the next token" $
           property $ \st' h -> do
             let p :: CustomParser (Maybe SourcePos)
                 p = getNextTokenPosition
                 st = st' { stateInput = h : stateInput st' }
             runParser' p st `shouldBe` (st, (Right . Just . spanStart) h)

  describe "ParsecT Semigroup instance" $
    it "the associative operation works" $
      property $ \a b -> do
        let p = pure [a] G.<> pure [b]
        prs p "" `shouldParse` ([a,b] :: [Int])

  describe "ParsecT Monoid instance" $ do
    it "mempty works" $ do
      let p = mempty
      prs p "" `shouldParse` ([] :: [Int])
    it "mappend works" $
      property $ \a b -> do
        let p = pure [a] `mappend` pure [b]
        prs p "" `shouldParse` ([a,b] :: [Int])

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
      context "when the argument parser succeeds without consuming" $
        it "is run nevertheless" $
          property $ \n' -> do
            let n = getSmall (getNonNegative n') :: Integer
                p = void . many $ do
                  x <- S.get
                  if x < n then S.modify (+ 1) else empty
                v :: S.State Integer (Either (ParseError Char Void) ())
                v = runParserT p "" ""
            S.execState v 0 `shouldBe` n

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
        let p = fail msg :: Parsec Void String ()
        prs p "" `shouldFailWith` errFancy posI (fancy $ ErrorFail msg)
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
          prs  p s `shouldFailWith` errFancy posI (fancy $ ErrorFail msg)
          prs' p s `failsLeaving` s

  describe "ParsecT MonadIO instance" $
    it "liftIO works" $
      property $ \n -> do
        let p = liftIO (return n) :: ParsecT Void String IO Integer
        runParserT p "" "" `shouldReturn` Right n

  describe "ParsecT MonadFix instance" $
    it "withRange works" $ do
      let
        withRange
          :: (MonadParsec e s m, MonadFix m)
          => ((SourcePos,SourcePos) -> m a)
          -> m a
        withRange f = do
          Just p1 <- getNextTokenPosition
          rec
            r <- f (p1, p2)
            p2 <- getPosition
          return r
        p :: Parsec Void String (SourcePos,SourcePos)
        p = withRange $ \pp -> pp <$ string "ab"
      runParser p "" "abcd"
        `shouldBe` Right
        ( SourcePos "" (mkPos 1) (mkPos 1)
        , SourcePos "" (mkPos 1) (mkPos 3)
        )

  describe "ParsecT MonadReader instance" $ do

    describe "ask" $
      it "returns correct value of context" $
        property $ \n -> do
          let p = ask :: ParsecT Void String (Reader Integer) Integer
          runReader (runParserT p "" "") n `shouldBe` Right n

    describe "local" $
      it "modifies reader context correctly" $
        property $ \n k -> do
          let p = local (+ k) ask :: ParsecT Void String (Reader Integer) Integer
          runReader (runParserT p "" "") n `shouldBe` Right (n + k)

  describe "ParsecT MonadState instance" $ do

    describe "get" $
      it "returns correct state value" $
        property $ \n -> do
          let p = L.get :: ParsecT Void String (L.State Integer) Integer
          L.evalState (runParserT p "" "") n `shouldBe` Right n
    describe "put" $
      it "replaces state value" $
        property $ \a b -> do
          let p = L.put b :: ParsecT Void String (L.State Integer) ()
          L.execState (runParserT p "" "") a `shouldBe` b

  describe "ParsecT MonadCont instance" $

    describe "callCC" $
      it "works properly" $
        property $ \a b -> do
          let p :: ParsecT Void String (Cont (Either (ParseError Char Void) Integer)) Integer
              p = callCC $ \e -> when (a > b) (e a) >> return b
          runCont (runParserT p "" "") id `shouldBe` Right (max a b)

  describe "ParsecT MonadError instance" $ do

    describe "throwError" $
      it "throws the error" $
        property $ \a b -> do
          let p :: ParsecT Void String (Except Integer) Integer
              p = throwError a >> return b
          runExcept (runParserT p "" "") `shouldBe` Left a

    describe "catchError" $
      it "catches the error" $
        property $ \a b -> do
          let p :: ParsecT Void String (Except Integer) Integer
              p = (throwError a >> return b) `catchError` handler
              handler e = return (e + b)
          runExcept (runParserT p "" "") `shouldBe` Right (Right $ a + b)

  describe "primitive combinators" $ do

    describe "unexpected" $
      it "signals correct parse error" $
        property $ \item -> do
          let p :: MonadParsec Void String m => m ()
              p = void (unexpected item)
          grs p "" (`shouldFailWith` TrivialError posI (E.singleton item) E.empty)

    describe "match" $
      it "return consumed tokens along with the result" $
        property $ \str -> do
          let p  = match (string str)
          prs  p str `shouldParse`     (str,str)
          prs' p str `succeedsLeaving` ""

    describe "region" $ do
      context "when inner parser succeeds" $
        it "has no effect" $
          property $ \st e n -> do
            let p :: Parser Int
                p = region (const e) (pure n)
            runParser' p st `shouldBe` (st, Right (n :: Int))
      context "when inner parser fails" $
        it "the given function is used on the parse error" $
          property $ \st' e pos' -> do
            let p :: Parsec Int String Int
                p = region f $
                  case e of
                    TrivialError _ us ps -> failure us ps
                    FancyError   _ xs    -> fancyFailure   xs
                f (TrivialError pos us ps) = FancyError
                  (max pos pos')
                  (E.singleton . ErrorCustom $ E.size us + E.size ps)
                f (FancyError pos xs) = FancyError
                  (max pos pos')
                  (E.singleton . ErrorCustom $ E.size xs)
                r = FancyError
                  (max (errorPos e) pos')
                  (E.singleton . ErrorCustom $
                    case e of
                      TrivialError _ us ps -> E.size us + E.size ps
                      FancyError   _ xs    -> E.size xs )
                finalPos = max (errorPos e) pos'
                st = st' { statePos = errorPos e }
            runParser' p st `shouldBe` (st { statePos = finalPos }, Left r)

    describe "failure" $
      it "signals correct parse error" $
        property $ \us ps -> do
          let p :: MonadParsec Void String m => m ()
              p = void (failure us ps)
          grs p "" (`shouldFailWith` TrivialError posI us ps)

    describe "fancyFailure" $
      it "singals correct parse error" $
        property $ \xs -> do
          let p :: MonadParsec Void String m => m ()
              p = void (fancyFailure xs)
          grs p "" (`shouldFailWith` FancyError posI xs)

    describe "label" $ do
      context "when inner parser succeeds consuming input" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Void String m => m Char
                  p = label lbl (char a) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
        context "inner parser produces hints" $
          it "replaces the last hint with “the rest of <label>”" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Void String m => m String
                  p = label lbl (many (char a)) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (elabel $ "the rest of " ++ lbl))
              grs' p s (`failsLeaving` "")
      context "when inner parser consumes and fails" $
        it "reports parse error without modification" $
          property $ \lbl a b c -> not (null lbl) && b /= c ==> do
            let p :: MonadParsec Void String m => m Char
                p = label lbl (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Void String m => m Char
                  p = label lbl (return a) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
        context "inner parser produces hints" $
          it "replaces the last hint with given label" $
            property $ \lbl a -> not (null lbl) ==> do
              let p :: MonadParsec Void String m => m String
                  p = label lbl (many (char a)) <* empty
              grs p "" (`shouldFailWith` err posI (elabel lbl))
      context "when inner parser fails without consuming" $
        it "is mentioned in parse error via its label" $
          property $ \lbl -> not (null lbl) ==> do
            let p :: MonadParsec Void String m => m ()
                p = label lbl empty
            grs p "" (`shouldFailWith` err posI (elabel lbl))

    describe "hidden" $ do
      context "when inner parser succeeds consuming input" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \a -> do
              let p :: MonadParsec Void String m => m Char
                  p = hidden (char a) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
        context "inner parser produces hints" $
          it "hides the parser in the error message" $
            property $ \a -> do
              let p :: MonadParsec Void String m => m String
                  p = hidden (many (char a)) <* empty
                  s = [a]
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
              grs' p s (`failsLeaving` "")
      context "when inner parser consumes and fails" $
        it "reports parse error without modification" $
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Void String m => m Char
                p = hidden (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $ do
        context "inner parser does not produce any hints" $
          it "collection of hints remains empty" $
            property $ \a -> do
              let p :: MonadParsec Void String m => m Char
                  p = hidden (return a) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
        context "inner parser produces hints" $
          it "hides the parser in the error message" $
            property $ \a -> do
              let p :: MonadParsec Void String m => m String
                  p = hidden (many (char a)) <* empty
              grs p "" (`shouldFailWith` err posI mempty)
      context "when inner parser fails without consuming" $
        it "hides the parser in the error message" $ do
          let p :: MonadParsec Void String m => m ()
              p = hidden empty
          grs p "" (`shouldFailWith` err posI mempty)

    describe "try" $ do
      context "when inner parser succeeds consuming" $
        it "try has no effect" $
          property $ \a -> do
            let p :: MonadParsec Void String m => m Char
                p = try (char a)
                s = [a]
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` "")
      context "when inner parser fails consuming" $
        it "backtracks, it appears as if the parser has not consumed anything" $
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Void String m => m Char
                p = try (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` s)
      context "when inner parser succeeds without consuming" $
        it "try has no effect" $
          property $ \a -> do
            let p :: MonadParsec Void String m => m Char
                p = try (return a)
            grs p "" (`shouldParse` a)
      context "when inner parser fails without consuming" $
        it "try backtracks parser state anyway" $
          property $ \w -> do
            let p :: MonadParsec Void String m => m Char
                p = try (setTabWidth w *> empty)
            grs  p "" (`shouldFailWith` err posI mempty)
            grs' p "" ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)

    describe "lookAhead" $ do
      context "when inner parser succeeds consuming" $ do
        it "result is returned but parser state is not changed" $
          property $ \a w -> do
            let p :: MonadParsec Void String m => m Pos
                p = lookAhead (setTabWidth w *> char a) *> getTabWidth
                s = [a]
            grs  p s (`shouldParse` defaultTabWidth)
            grs' p s (`succeedsLeaving` s)
        it "hints are not preserved" $
          property $ \a -> do
            let p :: MonadParsec Void String m => m String
                p = lookAhead (many (char a)) <* empty
                s = [a]
            grs  p s (`shouldFailWith` err posI mempty)
            grs' p s (`failsLeaving` s)
      context "when inner parser fails consuming" $
        it "error message is reported as usual" $
          property $ \a b c -> b /= c ==> do
            let p :: MonadParsec Void String m => m Char
                p = lookAhead (char a *> char b)
                s = [a,c]
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
            grs' p s (`failsLeaving` [c])
      context "when inner parser succeeds without consuming" $ do
        it "result is returned but parser state in not changed" $
          property $ \a w -> do
            let p :: MonadParsec Void String m => m Pos
                p = lookAhead (setTabWidth w *> char a) *> getTabWidth
                s = [a]
            grs  p s (`shouldParse` defaultTabWidth)
            grs' p s (`succeedsLeaving` s)
        it "hints are not preserved" $
          property $ \a b -> a /= b ==> do
            let p :: MonadParsec Void String m => m String
                p = lookAhead (many (char a)) <* empty
                s = [b]
            grs  p s (`shouldFailWith` err posI mempty)
            grs' p s (`failsLeaving` s)
      context "when inner parser fails without consuming" $
        it "error message is reported as usual" $ do
          let p :: MonadParsec Void String m => m Char
              p = lookAhead empty
          grs p "" (`shouldFailWith` err posI mempty)

    describe "notFollowedBy" $ do
      context "when inner parser succeeds consuming" $
        it "signals correct parse error" $
          property $ \a w -> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (setTabWidth w <* char a)
                s = [a]
            grs  p s (`shouldFailWith` err posI (utok a))
            grs' p s (`failsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
      context "when inner parser fails consuming" $ do
        it "succeeds without consuming" $
          property $ \a b c w -> b /= c ==> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (setTabWidth w *> char a *> char b)
                s = [a,c]
            grs' p s (`succeedsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
        it "hints are not preserved" $
          property $ \a b -> a /= b ==> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (char b *> many (char a) <* char a) <* empty
                s = [b,b]
            grs  p s (`shouldFailWith` err posI mempty)
            grs' p s (`failsLeaving` s)
      context "when inner parser succeeds without consuming" $
        it "signals correct parse error" $
          property $ \a w -> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (setTabWidth w *> return a)
                s = [a]
            grs  p s (`shouldFailWith` err posI (utok a))
            grs' p s (`failsLeaving` s)
            grs' p s ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
      context "when inner parser fails without consuming" $ do
        it "succeeds without consuming" $
          property $ \w -> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (setTabWidth w *> empty)
            grs  p "" (`shouldParse` ())
            grs' p "" ((`shouldBe` defaultTabWidth) . stateTabWidth . fst)
        it "hints are not preserved" $
          property $ \a -> do
            let p :: MonadParsec Void String m => m ()
                p = notFollowedBy (many (char a) <* char a) <* empty
                s = ""
            grs  p s (`shouldFailWith` err posI mempty)
            grs' p s (`failsLeaving` s)

    describe "withRecovery" $ do
      context "when inner parser succeeds consuming" $
        it "the result is returned as usual" $
          property $ \a as -> do
            let p :: MonadParsec Void String m => m (Maybe Char)
                p = withRecovery (const $ return Nothing) (pure <$> char a)
                s = a : as
            grs  p s (`shouldParse` Just a)
            grs' p s (`succeedsLeaving` as)
      context "when inner parser fails consuming" $ do
        context "when recovering parser succeeds consuming input" $ do
          it "its result is returned and position is advanced" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ string (c : as))
                        (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldParse` Left (err (posN (1 :: Int) s) (utok c <> etok b)))
              grs' p s (`succeedsLeaving` "")
          it "hints are not preserved" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ string (c : as))
                        (Right <$> char a <* many (char b) <* char b) <* empty
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (length s) s) mempty)
              grs' p s (`failsLeaving` "")
        context "when recovering parser fails consuming input" $
          it "the original parse error (and state) is reported" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ char c <* empty)
                        (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
              grs' p s (`failsLeaving` (c : as))
        context "when recovering parser succeeds without consuming" $ do
          it "its result is returned (and state)" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (return . Left) (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldParse` Left (err (posN (1 :: Int) s) (utok c <> etok b)))
              grs' p s (`succeedsLeaving` (c : as))
          it "original hints are preserved" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (return . Left)
                        (Right <$> char a <* many (char b) <* char b) <* empty
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (etok b))
              grs' p s (`failsLeaving` (c:as))
        context "when recovering parser fails without consuming" $
          it "the original parse error (and state) is reported" $
            property $ \a b c as -> b /= c ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ empty)
                        (Right <$> char a <* char b)
                  s = a : c : as
              grs  p s (`shouldFailWith` err (posN (1 :: Int) s) (utok c <> etok b))
              grs' p s (`failsLeaving` (c : as))
      context "when inner parser succeeds without consuming" $
        it "the result is returned as usual" $
          property $ \a s -> do
            let p :: MonadParsec Void String m => m (Maybe Char)
                p = withRecovery (const $ return Nothing) (return a)
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` s)
      context "when inner parser fails without consuming" $ do
        context "when recovering parser succeeds consuming input" $
          it "its result is returned and position is advanced" $
            property $ \a as -> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ string s) empty
                  s = a : as
              grs  p s (`shouldParse` Left (err posI mempty))
              grs' p s (`succeedsLeaving` "")
        context "when recovering parser fails consuming input" $
          it "the original parse error (and state) is reported" $
            property $ \a b as -> a /= b ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ char a <* char b <* empty)
                        (Right <$> empty)
                  s = a : as
              grs  p s (`shouldFailWith` err posI mempty)
              grs' p s (`failsLeaving` s)
        context "when recovering parser succeeds without consuming" $ do
          it "its result is returned (and state)" $
            property $ \s -> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (return . Left) empty
              grs  p s (`shouldParse` Left (err posI mempty))
              grs' p s (`succeedsLeaving` s)
          it "original hints are preserved" $
            property $ \a b as -> a /= b ==> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) String)
                  p = withRecovery (return . Left)
                        (Right <$> many (char a) <* empty) <* empty
                  s = b : as
              grs  p s (`shouldFailWith` err posI (etok a))
              grs' p s (`failsLeaving` s)
        context "when recovering parser fails without consuming" $
          it "the original parse error (and state) is reported" $
            property $ \s -> do
              let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                  p = withRecovery (\e -> Left e <$ empty) empty
              grs  p s (`shouldFailWith` err posI mempty)
              grs' p s (`failsLeaving` s)
      it "works in complex situations too" $
        property $ \a' b' c' -> do
          let p :: MonadParsec Void String m => m (Either (ParseError Char Void) String)
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

    describe "observing" $ do
      context "when inner parser succeeds consuming" $
        it "returns its result in Right" $
          property $ \a as -> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                p = observing (char a)
                s = a : as
            grs  p s (`shouldParse` Right a)
            grs' p s (`succeedsLeaving` as)
      context "when inner parser fails consuming" $ do
        it "returns its parse error in Left preserving state" $
          property $ \a b c as -> b /= c ==> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                p = observing (char a *> char b)
                s = a : c : as
            grs  p s (`shouldParse` Left (err (posN (1 :: Int) s) (utok c <> etok b)))
            grs' p s (`succeedsLeaving` (c:as))
        it "does not create any hints" $
          property $ \a b c as -> b /= c ==> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                p = observing (char a *> char b) *> empty
                s = a : c : as
            grs  p s (`shouldFailWith` err (posN (1 :: Int) s) mempty)
            grs' p s (`failsLeaving` (c:as))
      context "when inner parser succeeds without consuming" $
        it "returns its result in Right" $
          property $ \a s -> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                p = observing (return a)
            grs  p s (`shouldParse` Right a)
            grs' p s (`succeedsLeaving` s)
      context "when inner parser fails without consuming" $ do
        it "returns its parse error in Left preserving state" $
          property $ \s -> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) ())
                p = observing empty
            grs  p s (`shouldParse` Left (err posI mempty))
            grs' p s (`succeedsLeaving` s)
        it "creates correct hints" $
          property $ \a b as -> a /= b ==> do
            let p :: MonadParsec Void String m => m (Either (ParseError Char Void) Char)
                p = observing (char a) <* empty
                s = b : as
            grs  p s (`shouldFailWith` err posI (etok a))
            grs' p s (`failsLeaving` (b:as))

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
          testChar a x = if x == a then Right x else Left (f x, f a)
      context "when supplied predicate is satisfied" $
        it "succeeds" $
          property $ \a as mtok -> do
            let p :: MonadParsec Void String m => m Char
                p = token (testChar a) mtok
                s = a : as
            grs  p s (`shouldParse` a)
            grs' p s (`succeedsLeaving` as)
      context "when supplied predicate is not satisfied" $
        it "signals correct parse error" $
          property $ \a b as mtok -> a /= b ==> do
            let p :: MonadParsec Void String m => m Char
                p = token (testChar b) mtok
                s = a : as
                us = E.singleton (Tokens $ nes a)
                ps = E.singleton (Tokens $ nes b)
            grs  p s (`shouldFailWith` TrivialError posI us ps)
            grs' p s (`failsLeaving` s)
      context "when stream is empty" $
        it "signals correct parse error" $
          property $ \a mtok -> do
            let p :: MonadParsec Void String m => m Char
                p = token (testChar a) mtok
                us = E.singleton EndOfInput
                ps = maybe E.empty (E.singleton . Tokens . nes) mtok
            grs p "" (`shouldFailWith` TrivialError posI us ps)

    describe "tokens" $ do
      context "when stream is prefixed with given string" $
        it "parses the string" $
          property $ \str s -> do
            let p :: MonadParsec Void String m => m String
                p = tokens (==) str
                s' = str ++ s
            grs  p s' (`shouldParse` str)
            grs' p s' (`succeedsLeaving` s)
      context "when stream is not prefixed with given string" $
        it "signals correct parse error" $
          property $ \str s -> not (str `isPrefixOf` s) ==> do
            let p :: MonadParsec Void String m => m String
                p = tokens (==) str
                z = toFirstMismatch (==) str s
            grs  p s (`shouldFailWith` err posI (utoks z <> etoks str))
            grs' p s (`failsLeaving` s)

  describe "combinators for manipulating parser state" $ do

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

    describe "setPosition and getPosition" $
      it "sets position and gets it back" $
        property $ \st pos -> do
          let p :: Parser SourcePos
              p = setPosition pos >> getPosition
              f (State s (_:|xs) tp w) y = State s (y:|xs) tp w
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

    describe "setTokensProcessed and getTokensProcessed" $
      it "sets number of processed toknes and gets it back" $
        property $ \tp -> do
          let p = setTokensProcessed tp >> getTokensProcessed
          prs p "" `shouldParse` tp

    describe "setTabWidth and getTabWidth" $
      it "sets tab width and gets it back" $
        property $ \w -> do
          let p = setTabWidth w >> getTabWidth
          prs p "" `shouldParse` w

    describe "setParserState and getParserState" $
      it "sets parser state and gets it back" $
        property $ \s1 s2 -> do
          let p :: MonadParsec Void String m => m (State String)
              p = do
                st <- getParserState
                guard (st == State s posI 0 defaultTabWidth)
                setParserState s1
                updateParserState (f s2)
                liftM2 const getParserState (setInput "")
              f (State s1' pos tp w) (State s2' _ _ _) = State (max s1' s2') pos tp w
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

    describe "observing" $ do
      context "when inner parser succeeds" $
        it "can affect state" $
          property $ \m n -> do
            let p = do
                  L.put m
                  observing (L.modify (+ n))
            prs (L.execStateT p 0) "" `shouldParse` (m + n :: Integer)
      context "when inner parser fails" $
        it "cannot affect state" $
          property $ \m n -> do
            let p = do
                  L.put m
                  observing (L.modify (+ n) <* empty)
            prs (L.execStateT p 0) "" `shouldParse` (m :: Integer)

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

    describe "observing" $ do
      context "when inner parser succeeds" $
        it "can affect state" $
          property $ \m n -> do
            let p = do
                  S.put m
                  observing (L.modify (+ n))
            prs (S.execStateT p 0) "" `shouldParse` (m + n :: Integer)
      context "when inner parser fails" $
        it "cannot affect state" $
          property $ \m n -> do
            let p = do
                  S.put m
                  observing (L.modify (+ n) <* empty)
            prs (S.execStateT p 0) "" `shouldParse` (m :: Integer)

  describe "MonadParsec instance of lazy WriterT" $ do

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

    describe "lookAhead" $
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = lookAhead (L.tell [w])
          prs (L.runWriterT p) "" `shouldParse` ((), mempty :: [Int])

    describe "notFollowedBy" $
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = notFollowedBy (L.tell [w] <* char 'a')
          prs (L.runWriterT p) "" `shouldParse` ((), mempty :: [Int])

    describe "observing" $ do
      context "when inner parser succeeds" $
        it "can affect log" $
          property $ \n -> do
            let p = observing (L.tell $ Sum n)
            prs (L.execWriterT p) "" `shouldParse` (Sum n :: Sum Integer)
      context "when inner parser fails" $
        it "cannot affect log" $
          property $ \n -> do
            let p = observing (L.tell (Sum n) <* empty)
            prs (L.execWriterT p) "" `shouldParse` (mempty :: Sum Integer)

  describe "MonadParsec instance of strict WriterT" $ do

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

    describe "lookAhead" $
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = lookAhead (S.tell [w])
          prs (S.runWriterT p) "" `shouldParse` ((), mempty :: [Int])

    describe "notFollowedBy" $
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = notFollowedBy (S.tell [w] <* char 'a')
          prs (S.runWriterT p) "" `shouldParse` ((), mempty :: [Int])

    describe "observing" $ do
      context "when inner parser succeeds" $
        it "can affect log" $
          property $ \n -> do
            let p = observing (S.tell $ Sum n)
            prs (S.execWriterT p) "" `shouldParse` (Sum n :: Sum Integer)
      context "when inner parser fails" $
        it "cannot affect log" $
          property $ \n -> do
            let p = observing (S.tell (Sum n) <* empty)
            prs (S.execWriterT p) "" `shouldParse` (mempty :: Sum Integer)

  describe "MonadParsec instance of lazy RWST" $ do

    describe "label" $
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = label "a" ((,) <$> L.ask <*> L.get)
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "try" $
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = try ((,) <$> L.ask <*> L.get)
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "lookAhead" $ do
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = lookAhead ((,) <$> L.ask <*> L.get)
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = lookAhead (L.tell [w])
          prs (L.runRWST p (0 :: Int) (0 :: Int)) "" `shouldParse`
            ((), 0, mempty :: [Int])
      it "does not allow to influence state outside it" $
        property $ \s0 s1 -> (s0 /= s1) ==> do
          let p = lookAhead (L.put s1)
          prs (L.runRWST p (0 :: Int) (s0 :: Int)) "" `shouldParse`
            ((), s0, mempty :: [Int])

    describe "notFollowedBy" $ do
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = notFollowedBy (L.tell [w] <* char 'a')
          prs (L.runRWST p (0 :: Int) (0 :: Int)) "" `shouldParse`
            ((), 0, mempty :: [Int])
      it "does not allow to influence state outside it" $
        property $ \s0 s1 -> (s0 /= s1) ==> do
          let p = notFollowedBy (L.put s1 <* char 'a')
          prs (L.runRWST p (0 :: Int) (s0 :: Int)) "" `shouldParse`
            ((), s0, mempty :: [Int])

    describe "withRecovery" $ do
      it "allows main parser to access reader context and state inside it" $
        property $ \r s -> do
          let p = withRecovery (const empty) ((,) <$> L.ask <*> L.get)
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])
      it "allows recovering parser to access reader context and state inside it" $
        property $ \r s -> do
          let p = withRecovery (\_ -> (,) <$> L.ask <*> L.get) empty
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "observing" $ do
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = observing ((,) <$> L.ask <*> L.get)
          prs (L.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            (Right (r, s), s, mempty :: [Int])
      context "when the inner parser fails" $
        it "backtracks state" $
          property $ \r s0 s1 -> (s0 /= s1) ==> do
            let p = observing (L.put s1 <* empty)
            prs (L.runRWST p (r :: Int) (s0 :: Int)) "" `shouldParse`
              (Left (err posI mempty), s0, mempty :: [Int])

  describe "MonadParsec instance of strict RWST" $ do

    describe "label" $
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = label "a" ((,) <$> S.ask <*> S.get)
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "try" $
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = try ((,) <$> S.ask <*> S.get)
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "lookAhead" $ do
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = lookAhead ((,) <$> S.ask <*> S.get)
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = lookAhead (S.tell [w])
          prs (S.runRWST p (0 :: Int) (0 :: Int)) "" `shouldParse`
            ((), 0, mempty :: [Int])
      it "does not allow to influence state outside it" $
        property $ \s0 s1 -> (s0 /= s1) ==> do
          let p = lookAhead (S.put s1)
          prs (S.runRWST p (0 :: Int) (s0 :: Int)) "" `shouldParse`
            ((), s0, mempty :: [Int])

    describe "notFollowedBy" $ do
      it "discards what writer tells inside it" $
        property $ \w -> do
          let p = notFollowedBy (S.tell [w] <* char 'a')
          prs (S.runRWST p (0 :: Int) (0 :: Int)) "" `shouldParse`
            ((), 0, mempty :: [Int])
      it "does not allow to influence state outside it" $
        property $ \s0 s1 -> (s0 /= s1) ==> do
          let p = notFollowedBy (S.put s1 <* char 'a')
          prs (S.runRWST p (0 :: Int) (s0 :: Int)) "" `shouldParse`
            ((), s0, mempty :: [Int])

    describe "withRecovery" $ do
      it "allows main parser to access reader context and state inside it" $
        property $ \r s -> do
          let p = withRecovery (const empty) ((,) <$> S.ask <*> S.get)
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])
      it "allows recovering parser to access reader context and state inside it" $
        property $ \r s -> do
          let p = withRecovery (\_ -> (,) <$> S.ask <*> S.get) empty
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            ((r, s), s, mempty :: [Int])

    describe "observing" $ do
      it "allows to access reader context and state inside it" $
        property $ \r s -> do
          let p = observing ((,) <$> S.ask <*> S.get)
          prs (S.runRWST p (r :: Int) (s :: Int)) "" `shouldParse`
            (Right (r, s), s, mempty :: [Int])
      context "when the inner parser fails" $
        it "backtracks state" $
          property $ \r s0 s1 -> (s0 /= s1) ==> do
            let p = observing (S.put s1 <* empty)
            prs (S.runRWST p (r :: Int) (s0 :: Int)) "" `shouldParse`
              (Left (err posI mempty), s0, mempty :: [Int])

  describe "dbg" $ do
    -- NOTE We don't test properties here to avoid flood of debugging output
    -- when the test runs.
    context "when inner parser succeeds consuming input" $ do
      it "has no effect on how parser works" $ do
        let p = dbg "char" (char 'a')
            s = "ab"
        prs  p s `shouldParse` 'a'
        prs' p s `succeedsLeaving` "b"
      it "its hints are preserved" $ do
        let p = dbg "many chars" (many (char 'a')) <* empty
            s = "abcd"
        prs  p s `shouldFailWith` err (posN (1 :: Int) s) (etok 'a')
        prs' p s `failsLeaving` "bcd"
    context "when inner parser fails consuming input" $
      it "has no effect on how parser works" $ do
        let p = dbg "chars" (char 'a' *> char 'c')
            s = "abc"
        prs  p s `shouldFailWith` err (posN (1 :: Int) s) (utok 'b' <> etok 'c')
        prs' p s `failsLeaving` "bc"
    context "when inner parser succeeds without consuming" $ do
      it "has no effect on how parser works" $ do
        let p = dbg "return" (return 'a')
            s = "abc"
        prs  p s `shouldParse` 'a'
        prs' p s `succeedsLeaving` s
      it "its hints are preserved" $ do
        let p = dbg "many chars" (many (char 'a')) <* empty
            s = "bcd"
        prs  p s `shouldFailWith` err posI (etok 'a')
        prs' p s `failsLeaving` "bcd"
    context "when inner parser fails without consuming" $
      it "has no effect on how parser works" $ do
        let p = dbg "empty" (void empty)
            s = "abc"
        prs  p s `shouldFailWith` err posI mempty
        prs' p s `failsLeaving` s

----------------------------------------------------------------------------
-- Helpers

-- | This data type represents tokens in custom input stream.

data Span = Span
  { spanStart :: SourcePos
  , spanEnd   :: SourcePos
  , spanBody  :: NonEmpty Char
  } deriving (Eq, Ord, Show)

instance Stream [Span] where
  type Token [Span] = Span
  type Tokens [Span] = [Span]
  tokenToChunk Proxy = pure
  tokensToChunk Proxy = id
  chunkToTokens Proxy = id
  chunkLength Proxy = length
  chunkEmpty Proxy = null
  positionAt1 Proxy _ (Span start _ _) = start
  positionAtN Proxy pos [] = pos
  positionAtN Proxy _ (Span start _ _:_) = start
  advance1 Proxy _ _ (Span _ end _) = end
  advanceN Proxy _ pos [] = pos
  advanceN Proxy _ _ ts =
    let Span _ end _ = last ts in end
  take1_ [] = Nothing
  take1_ (t:ts) = Just (t, ts)
  takeN_ n s
    | n <= 0   = Just ([], s)
    | null s   = Nothing
    | otherwise = Just (splitAt n s)
  takeWhile_ = DL.span

instance Arbitrary Span where
  arbitrary = do
    start <- arbitrary
    end   <- arbitrary `suchThat` (> start)
    Span start end <$>
      (NE.fromList . getNonEmpty <$> arbitrary)

instance ShowToken Span where
  showTokens ts = concat (NE.toList . spanBody <$> ts)

type CustomParser = Parsec Void [Span]

pSpan :: Span -> CustomParser Span
pSpan span = token testToken (Just span)
  where
    f = E.singleton . Tokens . nes
    testToken x =
      if spanBody x == spanBody span
        then Right span
        else Left (f x, f span)

incCoincidence :: State [Span] -> [Span] -> Gen (State [Span])
incCoincidence st ts = do
  n <- getSmall <$> arbitrary
  let (pre, post) = splitAt n (stateInput st)
      pre' = zipWith (\x t -> x { spanBody = spanBody t }) pre ts
  return st { stateInput = pre' ++ post }

emulateStrParsing
  :: State String
  -> String
  -> (State String, Either (ParseError Char Void) String)
emulateStrParsing st@(State i (pos:|z) tp w) s =
  if l == length s
    then (State (drop l i) (updatePosString w pos s :| z) (tp + fromIntegral l) w, Right s)
    else (st, Left $ err (pos:|z) (etoks s <> utoks (take (l + 1) i)))
  where l = length (takeWhile id $ zipWith (==) s i)
