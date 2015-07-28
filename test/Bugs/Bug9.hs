
module Bugs.Bug9 (main) where

import Text.MegaParsec
import Text.MegaParsec.Language (haskellStyle)
import Text.MegaParsec.String (Parser)
import Text.MegaParsec.Expr
import qualified Text.MegaParsec.Token as P

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Util

data Expr = Const Integer | Op Expr Expr deriving Show

main :: Test
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'", "expecting operator or end of input"]
  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analysis

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = buildExpressionParser table (Const <$> integer)
  where
        table = [[ Infix (Op <$ reserved ">>>") AssocLeft ]]
        lexer = P.makeTokenParser haskellStyle { P.reservedOpNames = [">>>"] }
        integer    = P.integer    lexer
        reserved   = P.reserved   lexer
