
module Bugs.Bug9 (main) where

import Control.Applicative (empty)
import Control.Monad (void)

import Text.Megaparsec
import Text.Megaparsec.Expr
import Text.Megaparsec.String (Parser)
import qualified Text.Megaparsec.Lexer as L

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit hiding (Test)

import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (<$))
#endif

data Expr = Const Integer | Op Expr Expr deriving Show

main :: Test
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'", "expecting end of input or operator"]
  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analysis

sc :: Parser ()
sc = L.space (void spaceChar) empty empty

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

integer :: Parser Integer
integer = lexeme L.integer

operator :: String -> Parser String
operator = try . L.symbol sc

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = makeExprParser (Const <$> integer) table
  where table = [[ InfixL (Op <$ operator ">>>") ]]
