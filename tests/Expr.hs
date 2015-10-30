-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's expression parsers.
--
-- Copyright © 2015 Megaparsec contributors
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
-- * Neither the names of the copyright holders nor the names of
--   contributors may be used to endorse or promote products derived from
--   this software without specific prior written permission.
--
-- This software is provided by the copyright holders “as is” and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

module Expr (tests) where

import Control.Applicative (some, (<|>))

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Expr
import Text.Megaparsec.Prim

import Util

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
#endif

tests :: Test
tests = testGroup "Expression parsers"
        [ testProperty "correctness of expression parser" prop_correctness ]

-- Algebraic structures to build abstract syntax tree of our expression.

data Node
  = Val Integer   -- ^ literal value
  | Neg Node      -- ^ negation (prefix unary)
  | Fac Node      -- ^ factorial (postfix unary)
  | Mod Node Node -- ^ modulo
  | Sum Node Node -- ^ summation (addition)
  | Sub Node Node -- ^ subtraction
  | Pro Node Node -- ^ product
  | Div Node Node -- ^ division
  | Exp Node Node -- ^ exponentiation
    deriving (Eq, Show)

instance Enum Node where
  fromEnum (Val _)   = 0
  fromEnum (Neg _)   = 0
  fromEnum (Fac _)   = 0
  fromEnum (Mod _ _) = 0
  fromEnum (Exp _ _) = 1
  fromEnum (Pro _ _) = 2
  fromEnum (Div _ _) = 2
  fromEnum (Sum _ _) = 3
  fromEnum (Sub _ _) = 3
  toEnum   _         = error "Oops!"

instance Ord Node where
  x `compare` y = fromEnum x `compare` fromEnum y

showNode :: Node -> String
showNode (Val x)     = show x
showNode n@(Neg x)   = "-" ++ showGT n x
showNode n@(Fac x)   = showGT n x ++ "!"
showNode n@(Mod x y) = showGE n x ++ " % " ++ showGE n y
showNode n@(Sum x y) = showGT n x ++ " + " ++ showGE n y
showNode n@(Sub x y) = showGT n x ++ " - " ++ showGE n y
showNode n@(Pro x y) = showGT n x ++ " * " ++ showGE n y
showNode n@(Div x y) = showGT n x ++ " / " ++ showGE n y
showNode n@(Exp x y) = showGE n x ++ " ^ " ++ showGT n y

showGT :: Node -> Node -> String
showGT parent node = (if node > parent then showCmp else showNode) node

showGE :: Node -> Node -> String
showGE parent node = (if node >= parent then showCmp else showNode) node

showCmp :: Node -> String
showCmp node = (if fromEnum node == 0 then showNode else inParens) node

inParens :: Node -> String
inParens x = "(" ++ showNode x ++ ")"

instance Arbitrary Node where
  arbitrary = sized arbitraryN0

arbitraryN0 :: Int -> Gen Node
arbitraryN0 n = frequency [ (1, Mod <$> leaf <*> leaf)
                          , (9, arbitraryN1 n) ]
  where leaf = arbitraryN1 (n `div` 2)

arbitraryN1 :: Int -> Gen Node
arbitraryN1 n =
  frequency [ (1, Neg <$> arbitraryN2 n)
            , (1, Fac <$> arbitraryN2 n)
            , (7, arbitraryN2 n)]

arbitraryN2 :: Int -> Gen Node
arbitraryN2 0 = Val . getNonNegative <$> arbitrary
arbitraryN2 n = elements [Sum,Sub,Pro,Div,Exp] <*> leaf <*> leaf
  where leaf = arbitraryN0 (n `div` 2)

-- Some helpers are put here since we don't want to depend on
-- "Text.Megaparsec.Lexer".

lexeme :: MonadParsec s m Char => m a -> m a
lexeme p = p <* hidden space

symbol :: MonadParsec s m Char => String -> m String
symbol = lexeme . string

parens :: MonadParsec s m Char => m a -> m a
parens = between (symbol "(") (symbol ")")

integer :: MonadParsec s m Char => m Integer
integer = lexeme (read <$> some digitChar <?> "integer")

-- Here we use table of operators that makes use of all features of
-- 'makeExprParser'. Then we generate abstract syntax tree (AST) of complex
-- but valid expressions and render them to get their textual
-- representation.

expr :: MonadParsec s m Char => m Node
expr = makeExprParser term table

term :: MonadParsec s m Char => m Node
term = parens expr <|> (Val <$> integer) <?> "term"

table :: MonadParsec s m Char => [[Operator m Node]]
table = [ [ Prefix  (symbol "-" *> pure Neg)
          , Postfix (symbol "!" *> pure Fac)
          , InfixN  (symbol "%" *> pure Mod) ]
        , [ InfixR  (symbol "^" *> pure Exp) ]
        , [ InfixL  (symbol "*" *> pure Pro)
          , InfixL  (symbol "/" *> pure Div) ]
        , [ InfixL  (symbol "+" *> pure Sum)
          , InfixL  (symbol "-" *> pure Sub)] ]

prop_correctness :: Node -> Property
prop_correctness node = checkParser expr (Right node) (showNode node)
