
module Bugs.Bug39 (main) where

import Data.Either (isLeft, isRight)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

shouldFail :: [String]
shouldFail = [" 1", " +1", " -1"]

shouldSucceed :: [String]
shouldSucceed = ["1", "+1", "-1", "+ 1 ", "- 1 ", "1 "]

integer :: Parser Integer
integer = L.integer' (L.makeLexer L.defaultLang)

testBatch :: Assertion
testBatch = mapM_ (f testFail)    shouldFail >>
            mapM_ (f testSucceed) shouldSucceed
    where f           t a = t (parse integer "" a) a
          testFail    x a = assertBool
                            ("Should fail on " ++ show a) (isLeft x)
          testSucceed x a = assertBool
                            ("Should succeed on " ++ show a) (isRight x)

main :: Test
main = testCase "Lexer should fail on leading whitespace (#39)" testBatch
