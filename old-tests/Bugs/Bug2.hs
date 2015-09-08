
module Bugs.Bug2 (main) where

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Text.Megaparsec.Lexer as L

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

stringLiteral :: Parser String
stringLiteral = lexeme $ char '"' >> manyTill L.charLiteral (char '"')

main :: Test
main =
  testCase "Control Char Parsing (#2)" $
  parseString "\"test\\^Bstring\"" @?= "test\^Bstring"
 where
   parseString :: String -> String
   parseString input =
      case parse stringLiteral "Example" input of
        Left{} -> error "Parse failure"
        Right str -> str
