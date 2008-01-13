-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Combinator
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the LICENSE file)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  portable
-- 
-- Commonly used generic combinators
-- 
-----------------------------------------------------------------------------

module Text.Parsec.Combinator
    ( choice
    , count
    , between
    , option, optionMaybe, optional
    , skipMany1
    , many1
    , sepBy, sepBy1
    , endBy, endBy1
    , sepEndBy, sepEndBy1
    , chainl, chainl1
    , chainr, chainr1
    , eof, notFollowedBy
    -- tricky combinators
    , manyTill, lookAhead, anyToken
    ) where

import Control.Monad
import Text.Parsec.Prim

choice :: (Stream s m t) => [ParsecT s u m a] -> ParsecT s u m a
choice ps           = foldr (<|>) mzero ps

option :: (Stream s m t) => a -> ParsecT s u m a -> ParsecT s u m a
option x p          = p <|> return x

optionMaybe :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (Maybe a)
optionMaybe p       = option Nothing (liftM Just p)

optional :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
optional p          = do{ p; return ()} <|> return ()

between :: (Stream s m t) => ParsecT s u m open -> ParsecT s u m close
            -> ParsecT s u m a -> ParsecT s u m a
between open close p
                    = do{ open; x <- p; close; return x }


skipMany1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m ()
skipMany1 p         = do{ p; skipMany p }
{-
skipMany p          = scan
                    where
                      scan  = do{ p; scan } <|> return ()
-}

many1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m [a]
many1 p             = do{ x <- p; xs <- many p; return (x:xs) }
{-
many p              = scan id
                    where
                      scan f    = do{ x <- p
                                    ; scan (\tail -> f (x:tail))
                                    }
                                <|> return (f [])
-}

sepBy1,sepBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy p sep         = sepBy1 p sep <|> return []
sepBy1 p sep        = do{ x <- p
                        ; xs <- many (sep >> p)
                        ; return (x:xs)
                        }

sepEndBy1, sepEndBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep     = do{ x <- p
                        ; do{ sep
                            ; xs <- sepEndBy p sep
                            ; return (x:xs)
                            }
                          <|> return [x]
                        }

sepEndBy p sep      = sepEndBy1 p sep <|> return []


endBy1,endBy :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 p sep        = many1 (do{ x <- p; sep; return x })
endBy p sep         = many (do{ x <- p; sep; return x })

count :: (Stream s m t) => Int -> ParsecT s u m a -> ParsecT s u m [a]
count n p           | n <= 0    = return []
                    | otherwise = sequence (replicate n p)


chainr,chainl :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op x       = chainr1 p op <|> return x
chainl p op x       = chainl1 p op <|> return x

chainr1,chainl1 :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op        = do{ x <- p; rest x }
                    where
                      rest x    = do{ f <- op
                                    ; y <- p
                                    ; rest (f x y)
                                    }
                                <|> return x

chainr1 p op        = scan
                    where
                      scan      = do{ x <- p; rest x }

                      rest x    = do{ f <- op
                                    ; y <- scan
                                    ; return (f x y)
                                    }
                                <|> return x

-----------------------------------------------------------
-- Tricky combinators
-----------------------------------------------------------
anyToken :: (Stream s m t, Show t) => ParsecT s u m t
anyToken            = tokenPrim show (\pos tok toks -> pos) Just

eof :: (Stream s m t, Show t) => ParsecT s u m ()
eof                 = notFollowedBy anyToken <?> "end of input"

notFollowedBy :: (Stream s m t, Show t) => ParsecT s u m t -> ParsecT s u m ()
notFollowedBy p     = try (do{ c <- p; unexpected (show [c]) }
                           <|> return ()
                          )

manyTill :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end      = scan
                    where
                      scan  = do{ end; return [] }
                            <|>
                              do{ x <- p; xs <- scan; return (x:xs) }


lookAhead :: (Stream s m t) => ParsecT s u m a -> ParsecT s u m a
lookAhead p         = do{ state <- getParserState
                        ; x <- p
                        ; setParserState state
                        ; return x
                        }
