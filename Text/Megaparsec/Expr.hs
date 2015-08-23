-- |
-- Module      :  Text.Megaparsec.Expr
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- A helper module to parse expressions. Builds a parser given a table of
-- operators.

module Text.Megaparsec.Expr
  ( Assoc (..)
  , Operator (..)
  , OperatorTable
  , makeExprParser )
where

import Control.Applicative ((<|>))
import Data.List (foldl')

import Text.Megaparsec.Combinator
import Text.Megaparsec.Prim

-- | This data type specifies the associativity of operators: left, right
-- or none.

data Assoc
  = AssocNone
  | AssocLeft
  | AssocRight

-- | This data type specifies operators that work on values of type @a@.
-- An operator is either binary infix or unary prefix or postfix. A binary
-- operator has also an associated associativity.

data Operator s u m a
  = Infix   (ParsecT s u m (a -> a -> a)) Assoc
  | Prefix  (ParsecT s u m (a -> a))
  | Postfix (ParsecT s u m (a -> a))

-- | An @OperatorTable s u m a@ is a list of @Operator s u m a@ lists. The
-- list is ordered in descending precedence. All operators in one list have
-- the same precedence (but may have a different associativity).

type OperatorTable s u m a = [[Operator s u m a]]

-- | @makeExprParser table term@ builds an expression parser for terms
-- @term@ with operators from @table@, taking the associativity and
-- precedence specified in @table@ into account. Prefix and postfix
-- operators of the same precedence can only occur once (i.e. @--2@ is not
-- allowed if @-@ is prefix negate). Prefix and postfix operators of the
-- same precedence associate to the left (i.e. if @++@ is postfix increment,
-- than @-2++@ equals @-1@, not @-3@).
--
-- The @makeExprParser@ takes care of all the complexity involved in
-- building expression parser. Here is an example of an expression parser
-- that handles prefix signs, postfix increment and basic arithmetic.
--
-- > expr = makeExprParser table term <?> "expression"
-- >
-- > term = parens expr <|> natural <?> "simple expression"
-- >
-- > table = [ [ prefix  "-"  negate
-- >           , prefix  "+"  id ]
-- >         , [ postfix "++" (+1) ]
-- >         , [ binary  "*"  (*) AssocLeft
-- >           , binary  "/"  div AssocLeft ]
-- >         , [ binary  "+"  (+) AssocLeft
-- >           , binary  "-"  (-) AssocLeft ] ]
-- >
-- > binary  name fun assoc = Infix   (reservedOp name >> return fun) assoc
-- > prefix  name fun       = Prefix  (reservedOp name >> return fun)
-- > postfix name fun       = Postfix (reservedOp name >> return fun)

makeExprParser :: Stream s m t => OperatorTable s u m a ->
                         ParsecT s u m a -> ParsecT s u m a
makeExprParser ops simpleExpr = foldl' makeParser simpleExpr ops

makeParser :: (Foldable t, Stream s m t1) =>
              ParsecT s u m b -> t (Operator s u m b) -> ParsecT s u m b
makeParser term ops =
  termP >>= \x -> rasP x <|> lasP x <|> nasP x <|> return x <?> "operator"
  where (ras, las, nas, prefix, postfix) = foldr splitOp ([],[],[],[],[]) ops

        rasOp     = choice ras
        lasOp     = choice las
        nasOp     = choice nas
        prefixOp  = choice prefix  <?> ""
        postfixOp = choice postfix <?> ""

        ambigious assoc op =
          try $ op >> fail ("ambiguous use of a " ++ assoc
                            ++ " associative operator")

        ambigiousRight = ambigious "right" rasOp
        ambigiousLeft  = ambigious "left"  lasOp
        ambigiousNon   = ambigious "non"   nasOp

        termP = do
          pre  <- prefixP
          x    <- term
          post <- postfixP
          return $ post (pre x)

        postfixP = postfixOp <|> return id
        prefixP  = prefixOp  <|> return id

        rasP x = do { f <- rasOp; y <- termP >>= rasP1; return (f x y)}
                 <|> ambigiousLeft
                 <|> ambigiousNon

        rasP1 x = rasP x <|> return x

        lasP x = do { f <- lasOp; y <- termP; lasP1 (f x y) }
                 <|> ambigiousRight
                 <|> ambigiousNon

        lasP1 x = lasP x <|> return x

        nasP x = do
          f <- nasOp
          y <- termP
          ambigiousRight <|> ambigiousLeft <|> ambigiousNon <|> return (f x y)

splitOp :: Operator s u m a ->
           ( [ParsecT s u m (a -> a -> a)]
           , [ParsecT s u m (a -> a -> a)]
           , [ParsecT s u m (a -> a -> a)]
           , [ParsecT s u m (a -> a)]
           , [ParsecT s u m (a -> a)] ) ->
          ( [ParsecT s u m (a -> a -> a)]
          , [ParsecT s u m (a -> a -> a)]
          , [ParsecT s u m (a -> a -> a)]
          , [ParsecT s u m (a -> a)]
          , [ParsecT s u m (a -> a)] )
splitOp (Infix op assoc) (ras, las, nas, prefix, postfix) =
  case assoc of
    AssocNone  -> (ras,    las,    op:nas, prefix, postfix)
    AssocLeft  -> (ras,    op:las, nas,    prefix, postfix)
    AssocRight -> (op:ras, las,    nas,    prefix, postfix)
splitOp (Prefix  op) (ras, las, nas, prefix, postfix) =
  (ras, las, nas, op:prefix, postfix)
splitOp (Postfix op) (ras, las, nas, prefix, postfix) =
  (ras, las, nas, prefix, op:postfix)
