{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Text.Megaparsec.DebugSpec (spec) where

import Control.Exception (evaluate)
import Control.Monad
import Control.Monad.RWS (MonadRWS)
import qualified Control.Monad.RWS.Lazy as L
import qualified Control.Monad.RWS.Strict as S
import Control.Monad.State (MonadState (..), modify)
import qualified Control.Monad.State.Lazy as L
import qualified Control.Monad.State.Strict as S
import Control.Monad.Writer (MonadWriter (..))
import qualified Control.Monad.Writer.Lazy as L
import qualified Control.Monad.Writer.Strict as S
import Data.Void
import GHC.IO.Handle
import System.IO (stderr)
import System.IO.Temp
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug

spec :: Spec
spec = do
  describe "dbg" $ do
    context "when inner parser succeeds consuming input" $ do
      it "has no effect on how parser works" $ do
        let p :: MonadParsecDbg Void String m => m Char
            p = dbg "char" (char 'a')
            s = "ab"
        shouldStderr p s "char> IN: \"ab\"\nchar> MATCH (COK): 'a'\nchar> VALUE: 'a'\n\n"
        grs p s (`shouldParse` 'a')
        grs' p s (`succeedsLeaving` "b")
      it "its hints are preserved" $ do
        let p :: MonadParsecDbg Void String m => m String
            p = dbg "many chars" (many (char 'a')) <* empty
            s = "abcd"
        shouldStderr p s "many chars> IN: \"abcd\"\nmany chars> MATCH (COK): 'a'\nmany chars> VALUE: \"a\"\n\n"
        grs p s (`shouldFailWith` err 1 (etok 'a'))
        grs' p s (`failsLeaving` "bcd")
    context "when inner parser fails consuming input" $
      it "has no effect on how parser works" $ do
        let p :: MonadParsecDbg Void String m => m Char
            p = dbg "chars" (char 'a' *> char 'c')
            s = "abc"
        shouldStderr p s "chars> IN: \"abc\"\nchars> MATCH (CERR): 'a'\nchars> ERROR:\nchars> offset=1:\nchars> unexpected 'b'\nchars> expecting 'c'\n\n"
        grs p s (`shouldFailWith` err 1 (utok 'b' <> etok 'c'))
        grs' p s (`failsLeaving` "bc")
    context "when inner parser succeeds without consuming" $ do
      it "has no effect on how parser works" $ do
        let p :: MonadParsecDbg Void String m => m Char
            p = dbg "return" (return 'a')
            s = "abc"
        shouldStderr p s "return> IN: \"abc\"\nreturn> MATCH (EOK): <EMPTY>\nreturn> VALUE: 'a'\n\n"
        grs p s (`shouldParse` 'a')
        grs' p s (`succeedsLeaving` s)
      it "its hints are preserved" $ do
        let p :: MonadParsecDbg Void String m => m String
            p = dbg "many chars" (many (char 'a')) <* empty
            s = "bcd"
        shouldStderr p s "many chars> IN: \"bcd\"\nmany chars> MATCH (EOK): <EMPTY>\nmany chars> VALUE: \"\"\n\n"
        grs p s (`shouldFailWith` err 0 (etok 'a'))
        grs' p s (`failsLeaving` "bcd")
    context "when inner parser fails without consuming" $
      it "has no effect on how parser works" $ do
        let p :: MonadParsecDbg Void String m => m ()
            p = dbg "empty" (void empty)
            s = "abc"
        shouldStderr p s "empty> IN: \"abc\"\nempty> MATCH (EERR): <EMPTY>\nempty> ERROR:\nempty> offset=0:\nempty> unknown parse error\n\n"
        grs p s (`shouldFailWith` err 0 mempty)
        grs' p s (`failsLeaving` s)
    let p1, p2 :: (MonadParsecDbg Void String m, MonadWriter [Int] m) => m ()
        p1 = tell [0] >> dbg "a" (single 'a' >> tell [1])
        p2 = do
          void $ dbg "a" (single 'a')
          tell [0]
          void $ dbg "b" (single 'b')
          dbg "c" $ do
            void (single 'c')
            tell [1]
            void (single 'd')
            tell [2]
        s1 = "a"
        s2 = "abcd"
        stderr1 = "a> IN: 'a'\na> MATCH (COK): 'a'\na> VALUE: () (LOG: [1])\n\n"
        stderr2 = "a> IN: \"abcd\"\na> MATCH (COK): 'a'\na> VALUE: 'a' (LOG: [])\n\nb> IN: \"bcd\"\nb> MATCH (COK): 'b'\nb> VALUE: 'b' (LOG: [])\n\nc> IN: \"cd\"\nc> MATCH (COK): \"cd\"\nc> VALUE: () (LOG: [1,2])\n\n"
        r1 = ((), [0, 1])
        r2 = ((), [0, 1, 2])
    context "Lazy WriterT instance of MonadParsecDbg" $ do
      it "example 1" $ do
        shouldStderr (L.runWriterT p1) s1 stderr1
        prs (L.runWriterT p1) s1 `shouldParse` r1
      it "example 2" $ do
        shouldStderr (L.runWriterT p2) s2 stderr2
        prs (L.runWriterT p2) s2 `shouldParse` r2
    context "Strict WriterT instance of MonadParsecDbg" $ do
      it "example 1" $ do
        shouldStderr (S.runWriterT p1) s1 stderr1
        prs (S.runWriterT p1) s1 `shouldParse` r1
      it "example 2" $ do
        shouldStderr (S.runWriterT p2) s2 stderr2
        prs (S.runWriterT p2) s2 `shouldParse` r2
    let p3, p4 :: (MonadParsecDbg Void String m, MonadState Int m) => m ()
        p3 = modify succ >> dbg "a" (single 'a' >> modify succ)
        p4 = do
          void $ dbg "a" (single 'a')
          modify succ
          void $ dbg "b" (single 'b')
          dbg "c" $ do
            void (single 'c')
            modify succ
            void (single 'd')
            modify succ
        s3 = "a"
        s4 = "abcd"
        stderr3 = "a> IN: 'a'\na> MATCH (COK): 'a'\na> VALUE: () (STATE: 2)\n\n"
        stderr4 = "a> IN: \"abcd\"\na> MATCH (COK): 'a'\na> VALUE: 'a' (STATE: 0)\n\nb> IN: \"bcd\"\nb> MATCH (COK): 'b'\nb> VALUE: 'b' (STATE: 1)\n\nc> IN: \"cd\"\nc> MATCH (COK): \"cd\"\nc> VALUE: () (STATE: 3)\n\n"
        r3 = ((), 2)
        r4 = ((), 3)
    context "Lazy StateT instance of MonadParsecDbg" $ do
      it "example 3" $ do
        shouldStderr (L.runStateT p3 0) s3 stderr3
        prs (L.runStateT p3 0) s3 `shouldParse` r3
      it "example 4" $ do
        shouldStderr (L.runStateT p4 0) s4 stderr4
        prs (L.runStateT p4 0) s4 `shouldParse` r4
    context "Strict StateT instance of MonadParsecDbg" $ do
      it "example 3" $ do
        shouldStderr (S.runStateT p3 0) s3 stderr3
        prs (S.runStateT p3 0) s3 `shouldParse` r3
      it "example 4" $ do
        shouldStderr (S.runStateT p4 0) s4 stderr4
        prs (S.runStateT p4 0) s4 `shouldParse` r4
    let p5 :: (MonadParsecDbg Void String m, MonadRWS () [Int] Int m) => m ()
        p5 = do
          tell [0]
          modify succ
          dbg "a" (single 'a' >> tell [1] >> modify succ)
        s5 = "a"
        stderr5 = "a> IN: 'a'\na> MATCH (COK): 'a'\na> VALUE: () (STATE: 2) (LOG: [1])\n\n"
        stderr7 = "a> IN: 'a'\na> MATCH (COK): 'a'\na> VALUE: () (LOG: [1]) (STATE: 2)\n\n"
        r5 = ((), 2, [0, 1])
        p6 :: (MonadParsecDbg Void String m, MonadWriter [Int] m, MonadState Int m) => m ()
        p6 = do
          tell [0]
          modify succ
          dbg "a" (single 'a' >> tell [1] >> modify succ)
        r6 = (((), 2), [0, 1])
        r7 = (((), [0, 1]), 2)
    context "Lazy RWST instance of MonadParsecDbg" $ do
      it "example 5" $ do
        shouldStderr (L.runRWST p5 () 0) s5 stderr5
        prs (L.runRWST p5 () 0) s5 `shouldParse` r5
      it "example 6" $ do
        shouldStderr (L.runWriterT (L.runStateT p6 0)) s5 stderr5
        prs (L.runWriterT (L.runStateT p6 0)) s5 `shouldParse` r6
      it "example 7" $ do
        shouldStderr (L.runStateT (L.runWriterT p6) 0) s5 stderr7
        prs (L.runStateT (L.runWriterT p6) 0) s5 `shouldParse` r7
    context "Strict RWST instance of MonadParsecDbg" $ do
      it "example 5" $ do
        shouldStderr (S.runRWST p5 () 0) s5 stderr5
        prs (S.runRWST p5 () 0) s5 `shouldParse` r5
      it "example 6" $ do
        shouldStderr (S.runWriterT (S.runStateT p6 0)) s5 stderr5
        prs (S.runWriterT (S.runStateT p6 0)) s5 `shouldParse` r6
      it "example 7" $ do
        shouldStderr (S.runStateT (S.runWriterT p6) 0) s5 stderr7
        prs (S.runStateT (S.runWriterT p6) 0) s5 `shouldParse` r7

----------------------------------------------------------------------------
-- Helpers

-- | Check that running the given parser on the input prints the expected
-- string to the 'stderr'.
shouldStderr ::
  -- | The parser to test
  Parser a ->
  -- | Input for the parser
  String ->
  -- | The expected 'stderr' output
  String ->
  Expectation
shouldStderr p s expectedStderr = do
  hFlush stderr
  withSystemTempFile "megaparsec-dbg-tests" $ \tempPath tempHandle -> do
    hDuplicateTo tempHandle stderr
    void (evaluate (parse p "" s))
    hFlush stderr
    hClose tempHandle
    capturedStderr <- readFile tempPath
    capturedStderr `shouldBe` expectedStderr
