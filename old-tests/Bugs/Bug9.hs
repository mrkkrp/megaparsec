
module Bugs.Bug9 (main) where

import Text.Megaparsec
import Text.Megaparsec.Language (haskellStyle)
import Text.Megaparsec.String (Parser)
import Text.Megaparsec.Expr
import qualified Text.Megaparsec.Token as P

import Test.HUnit hiding (Test)
import Test.Framework
import Test.Framework.Providers.HUnit

import Util

data Expr = Const Integer | Op Expr Expr deriving Show

main :: Test
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'", "expecting end of input or operator"]
  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analysis

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = makeExprParser table (Const <$> integer)
  where
        table = [[ Infix (Op <$ reserved ">>>") AssocLeft ]]
        lexer = P.makeLexer haskellStyle { P.reservedOpNames = [">>>"] }
        integer    = P.integer    lexer
        reserved   = P.reserved   lexer
