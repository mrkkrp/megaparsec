-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Expr
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  non-portable
-- 
-- A helper module to parse \"expressions\".
-- Builds a parser given a table of operators and associativities.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Text.Parsec.Expr
    ( Assoc(..), Operator(..), OperatorTable
    , buildExpressionParser
    ) where

import Text.Parsec.Prim
import Text.Parsec.Combinator

-----------------------------------------------------------
-- Assoc and OperatorTable
-----------------------------------------------------------
data Assoc                = AssocNone
                          | AssocLeft
                          | AssocRight

data Operator s t u m a   = Infix (ParsecT s u m (a -> a -> a)) Assoc
                          | Prefix (ParsecT s u m (a -> a))
                          | Postfix (ParsecT s u m (a -> a))

type OperatorTable s t u m a = [[Operator s t u m a]]



-----------------------------------------------------------
-- Convert an OperatorTable and basic term parser into
-- a full fledged expression parser
-----------------------------------------------------------
buildExpressionParser :: (Stream s m t)
                      => OperatorTable s t u m a
                      -> ParsecT s u m a
                      -> ParsecT s u m a
buildExpressionParser operators simpleExpr
    = foldl (makeParser) simpleExpr operators
    where
      makeParser term ops
        = let (rassoc,lassoc,nassoc
               ,prefix,postfix)      = foldr splitOp ([],[],[],[],[]) ops

              rassocOp   = choice rassoc
              lassocOp   = choice lassoc
              nassocOp   = choice nassoc
              prefixOp   = choice prefix  <?> ""
              postfixOp  = choice postfix <?> ""

              ambigious assoc op= try $
                                  do{ op; fail ("ambiguous use of a " ++ assoc
                                                 ++ " associative operator")
                                    }

              ambigiousRight    = ambigious "right" rassocOp
              ambigiousLeft     = ambigious "left" lassocOp
              ambigiousNon      = ambigious "non" nassocOp

              termP      = do{ pre  <- prefixP
                             ; x    <- term
                             ; post <- postfixP
                             ; return (post (pre x))
                             }

              postfixP   = postfixOp <|> return id

              prefixP    = prefixOp <|> return id

              rassocP x  = do{ f <- rassocOp
                             ; y  <- do{ z <- termP; rassocP1 z }
                             ; return (f x y)
                             }
                           <|> ambigiousLeft
                           <|> ambigiousNon
                           -- <|> return x

              rassocP1 x = rassocP x  <|> return x

              lassocP x  = do{ f <- lassocOp
                             ; y <- termP
                             ; lassocP1 (f x y)
                             }
                           <|> ambigiousRight
                           <|> ambigiousNon
                           -- <|> return x

              lassocP1 x = lassocP x <|> return x

              nassocP x  = do{ f <- nassocOp
                             ; y <- termP
                             ;    ambigiousRight
                              <|> ambigiousLeft
                              <|> ambigiousNon
                              <|> return (f x y)
                             }
                           -- <|> return x

           in  do{ x <- termP
                 ; rassocP x <|> lassocP  x <|> nassocP x <|> return x
                   <?> "operator"
                 }


      splitOp (Infix op assoc) (rassoc,lassoc,nassoc,prefix,postfix)
        = case assoc of
            AssocNone  -> (rassoc,lassoc,op:nassoc,prefix,postfix)
            AssocLeft  -> (rassoc,op:lassoc,nassoc,prefix,postfix)
            AssocRight -> (op:rassoc,lassoc,nassoc,prefix,postfix)

      splitOp (Prefix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,op:prefix,postfix)

      splitOp (Postfix op) (rassoc,lassoc,nassoc,prefix,postfix)
        = (rassoc,lassoc,nassoc,prefix,op:postfix)
