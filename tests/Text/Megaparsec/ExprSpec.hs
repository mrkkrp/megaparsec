{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Text.Megaparsec.ExprSpec (spec) where

import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.Hspec.Megaparsec.AdHoc
import Test.QuickCheck
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Expr

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*), (<*>), (*>), pure)
#endif

spec :: Spec
spec =
  describe "makeExprParser" $ do
    context "when given valid rendered AST" $
      it "can parse it back" $
        property $ \node -> do
          let s = showNode node
          prs  expr s `shouldParse`     node
          prs' expr s `succeedsLeaving` ""
    context "when stream in empty" $
      it "signals correct parse error" $
        prs (expr <* eof) "" `shouldFailWith` err posI (ueof <> elabel "term")
    context "when term is missing" $
      it "signals correct parse error" $ do
        let p = expr <* eof
            n = 1 :: Integer
        prs p "-" `shouldFailWith` err (posN n "-") (ueof <> elabel "term")
        prs p "(" `shouldFailWith` err (posN n "(") (ueof <> elabel "term")
        prs p "*" `shouldFailWith` err posI (utok '*' <> elabel "term")
    context "operator is missing" $
      it "signals correct parse error" $
        property $ \a b -> do
          let p = expr <* eof
              a' = inParens a
              n  = length a' + 1
              s  = a'  ++ " " ++ inParens b
              c  = s !! n
          if c == '-'
            then prs p s `shouldParse` Sub a b
            else prs p s `shouldFailWith`
                 err (posN n s) (utok c <> eeof <> elabel "operator")

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

lexeme :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
lexeme p = p <* hidden space

symbol :: (MonadParsec e s m, Token s ~ Char) => String -> m String
symbol = lexeme . string

parens :: (MonadParsec e s m, Token s ~ Char) => m a -> m a
parens = between (symbol "(") (symbol ")")

integer :: (MonadParsec e s m, Token s ~ Char) => m Integer
integer = lexeme (read <$> some digitChar <?> "integer")

-- Here we use a table of operators that makes use of all features of
-- 'makeExprParser'. Then we generate abstract syntax tree (AST) of complex
-- but valid expressions and render them to get their textual
-- representation.

expr :: (MonadParsec e s m, Token s ~ Char) => m Node
expr = makeExprParser term table

term :: (MonadParsec e s m, Token s ~ Char) => m Node
term = parens expr <|> (Val <$> integer) <?> "term"

table :: (MonadParsec e s m, Token s ~ Char) => [[Operator m Node]]
table = [ [ Prefix  (symbol "-" *> pure Neg)
          , Postfix (symbol "!" *> pure Fac)
          , InfixN  (symbol "%" *> pure Mod) ]
        , [ InfixR  (symbol "^" *> pure Exp) ]
        , [ InfixL  (symbol "*" *> pure Pro)
          , InfixL  (symbol "/" *> pure Div) ]
        , [ InfixL  (symbol "+" *> pure Sum)
          , InfixL  (symbol "-" *> pure Sub)] ]
