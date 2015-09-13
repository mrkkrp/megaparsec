
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

data Expr = Const Integer | Op Expr Expr deriving Show

main :: Test
main =
  testCase "Tracing of current position in error message (#9)"
  $ result @?= ["unexpected '>'", "expecting end of input or operator"]
  where
    result :: [String]
    result = parseErrors parseTopLevel "4 >> 5"

-- Syntax analysis

sc :: Stream s m Char => ParsecT s u m ()
sc = L.space (void spaceChar) empty empty

lexeme :: Stream s m Char => ParsecT s u m a -> ParsecT s u m a
lexeme = L.lexeme sc

integer :: Stream s m Char => ParsecT s u m Integer
integer = lexeme L.integer

operator :: Stream s m Char => String -> ParsecT s u m String
operator = try . L.symbol sc

parseTopLevel :: Parser Expr
parseTopLevel = parseExpr <* eof

parseExpr :: Parser Expr
parseExpr = makeExprParser (Const <$> integer) table
  where table = [[ InfixL (Op <$ operator ">>>") ]]
