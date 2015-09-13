
module Bugs.Bug39 (main) where

import Control.Applicative (empty)
import Control.Monad (void)
import Data.Either (isLeft, isRight)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

shouldFail :: [String]
shouldFail = [" 1", " +1", " -1"]

shouldSucceed :: [String]
shouldSucceed = ["1", "+1", "-1", "+ 1 ", "- 1 ", "1 "]

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme $ L.signed sc L.integer

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
