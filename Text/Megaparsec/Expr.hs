-- |
-- Module      :  Text.Megaparsec.Expr
-- Copyright   :  © 2015–2017 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A helper module to parse expressions. It can build a parser given a table
-- of operators.

module Text.Megaparsec.Expr
  ( Operator (..)
  , makeExprParser )
where

import Control.Applicative ((<|>))

import Text.Megaparsec.Combinator
import Text.Megaparsec.Prim

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix or postfix. A binary
-- operator has also an associated associativity.

data Operator m a
  = InfixN  (m (a -> a -> a)) -- ^ Non-associative infix
  | InfixL  (m (a -> a -> a)) -- ^ Left-associative infix
  | InfixR  (m (a -> a -> a)) -- ^ Right-associative infix
  | Prefix  (m (a -> a))      -- ^ Prefix
  | Postfix (m (a -> a))      -- ^ Postfix

-- | @makeExprParser term table@ builds an expression parser for terms
-- @term@ with operators from @table@, taking the associativity and
-- precedence specified in @table@ into account.
--
-- @table@ is a list of @[Operator m a]@ lists. The list is ordered in
-- descending precedence. All operators in one list have the same precedence
-- (but may have different associativity).
--
-- Prefix and postfix operators of the same precedence associate to the left
-- (i.e. if @++@ is postfix increment, than @-2++@ equals @-1@, not @-3@).
--
-- Unary operators of the same precedence can only occur once (i.e. @--2@ is
-- not allowed if @-@ is prefix negate). If you need to parse several prefix
-- or postfix operators in a row, (like C pointers — @**i@) you can use this
-- approach:
--
-- > manyUnaryOp = foldr1 (.) <$> some singleUnaryOp
--
-- This is not done by default because in some cases you don't want to allow
-- repeating prefix or postfix operators.
--
-- If you want to have an operator that is a prefix of another operator in
-- the table, use the following (or similar) wrapper instead of plain
-- 'symbol':
--
-- > op n = (lexeme . try) (string n <* notFollowedBy punctuationChar)
--
-- @makeExprParser@ takes care of all the complexity involved in building an
-- expression parser. Here is an example of an expression parser that
-- handles prefix signs, postfix increment and basic arithmetic:
--
-- > expr = makeExprParser term table <?> "expression"
-- >
-- > term = parens expr <|> integer <?> "term"
-- >
-- > table = [ [ prefix  "-"  negate
-- >           , prefix  "+"  id ]
-- >         , [ postfix "++" (+1) ]
-- >         , [ binary  "*"  (*)
-- >           , binary  "/"  div  ]
-- >         , [ binary  "+"  (+)
-- >           , binary  "-"  (-)  ] ]
-- >
-- > binary  name f = InfixL  (f <$ symbol name)
-- > prefix  name f = Prefix  (f <$ symbol name)
-- > postfix name f = Postfix (f <$ symbol name)

makeExprParser :: MonadParsec e s m
  => m a               -- ^ Term parser
  -> [[Operator m a]]  -- ^ Operator table, see 'Operator'
  -> m a               -- ^ Resulting expression parser
makeExprParser = foldl addPrecLevel

-- | @addPrecLevel p ops@ adds the ability to parse operators in table @ops@
-- to parser @p@.

addPrecLevel :: MonadParsec e s m => m a -> [Operator m a] -> m a
addPrecLevel term ops =
  term' >>= \x -> choice [ras' x, las' x, nas' x, return x] <?> "operator"
  where (ras, las, nas, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops
        term' = pTerm (choice prefix) term (choice postfix)
        ras'  = pInfixR (choice ras) term'
        las'  = pInfixL (choice las) term'
        nas'  = pInfixN (choice nas) term'

-- | @pTerm prefix term postfix@ parses a term with @term@ surrounded by
-- optional prefix and postfix unary operators. Parsers @prefix@ and
-- @postfix@ are allowed to fail, in this case 'id' is used.

pTerm :: MonadParsec e s m => m (a -> a) -> m a -> m (a -> a) -> m a
pTerm prefix term postfix = do
  pre  <- option id (hidden prefix)
  x    <- term
  post <- option id (hidden postfix)
  return . post . pre $ x

-- | @pInfixN op p x@ parses non-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.

pInfixN :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixN op p x = do
  f <- op
  y <- p
  return $ f x y

-- | @pInfixL op p x@ parses left-associative infix operator @op@, then term
-- with parser @p@, then returns result of the operator application on @x@
-- and the term.

pInfixL :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixL op p x = do
  f <- op
  y <- p
  let r = f x y
  pInfixL op p r <|> return r

-- | @pInfixR op p x@ parses right-associative infix operator @op@, then
-- term with parser @p@, then returns result of the operator application on
-- @x@ and the term.

pInfixR :: MonadParsec e s m => m (a -> a -> a) -> m a -> a -> m a
pInfixR op p x = do
  f <- op
  y <- p >>= \r -> pInfixR op p r <|> return r
  return $ f x y

type Batch m a =
  ( [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a -> a)]
  , [m (a -> a)]
  , [m (a -> a)] )

-- | A helper to separate various operators (binary, unary, and according to
-- associativity) and return them in a tuple.

splitOp :: Operator m a -> Batch m a -> Batch m a
splitOp (InfixR  op) (r, l, n, pre, post) = (op:r, l, n, pre, post)
splitOp (InfixL  op) (r, l, n, pre, post) = (r, op:l, n, pre, post)
splitOp (InfixN  op) (r, l, n, pre, post) = (r, l, op:n, pre, post)
splitOp (Prefix  op) (r, l, n, pre, post) = (r, l, n, op:pre, post)
splitOp (Postfix op) (r, l, n, pre, post) = (r, l, n, pre, op:post)
