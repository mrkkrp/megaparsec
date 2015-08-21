-- |
-- Module      :  Text.Megaparsec.Combinator
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Commonly used generic combinators.

module Text.Megaparsec.Combinator
  ( between
  , chainl
  , chainl1
  , chainr
  , chainr1
  , choice
  , count
  , count'
  , endBy
  , endBy1
  , eof
  , manyTill
  , option
  , sepBy
  , sepBy1
  , sepEndBy
  , sepEndBy1
  , skipMany
  , skipSome )
where

import Control.Applicative ((<|>), many, some, optional)
import Control.Monad

import Text.Megaparsec.Prim

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- > braces = between (symbol "{") (symbol "}")

between :: Stream s m t => ParsecT s u m open ->
           ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
between open close p = open *> p <* close

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are zero occurrences of @p@, the value @x@ is returned.

chainl :: Stream s m t => ParsecT s u m a ->
          ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl p op x = chainl1 p op <|> return x

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. This parser can for example be used to eliminate left recursion
-- which typically occurs in expression grammars.
--
-- > expr   = term   `chainl1` addop
-- > term   = factor `chainl1` mulop
-- > factor = parens expr <|> integer
-- >
-- > mulop  = (symbol "*" >> return (*))
-- >      <|> (symbol "/" >> return (div))
-- >
-- > addop  = (symbol "+" >> return (+))
-- >      <|> (symbol "-" >> return (-))

chainl1 :: Stream s m t =>
           ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op = p >>= rest
  where rest x = ((($ x) <$> op <*> p) >>= rest) <|> return x

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are no occurrences of @p@, the value @x@ is returned.

chainr :: Stream s m t => ParsecT s u m a ->
          ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op x = chainr1 p op <|> return x

-- | @chainr1 p op@ parses /one/ or more occurrences of |p|,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@.

chainr1 :: Stream s m t =>
           ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr1 p op = p >>= rest
  where rest x = (($ x) <$> op <*> chainr1 p op) <|> return x

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding parser.

choice :: Stream s m t => [ParsecT s u m a] -> ParsecT s u m a
choice = foldr (<|>) mzero

-- | @ count m n p@ parses from @m@ to @n@ occurrences of @p@. If @m@ is
-- not positive or @m > n@, the parser equals to @return []@. Returns a list
-- of parsed values.

count :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
count m n p
  | n <= 0 || m > n = return []
  | m > 0           = (:) <$> p <*> count (pred m) (pred n) p
  | otherwise       = do
      result <- optional p
      case result of
        Nothing -> return []
        Just x  -> (x:) <$> count 0 (pred n) p

-- | @count\' n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of @n@
-- values.
--
-- This parser is defined in terms of 'count', like this:
--
-- > count' n = count n n

count' :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
count' n = count n n

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semi

endBy :: Stream s m t =>
         ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy p sep = many (p <* sep)

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: Stream s m t =>
          ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
endBy1 p sep = some (p <* sep)

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@. This
-- parser can be used to scan comments:
--
-- > simpleComment = string "<!--" >> manyTill anyChar (try $ string "-->")
--
-- Note that we need to use 'try' since parsers @anyChar@ and @string
-- \"-->\"@ overlap and @string \"-->\"@ could consume input before failing.

manyTill :: Stream s m t =>
            ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
manyTill p end = (end *> return []) <|> ((:) <$> p <*> manyTill p end)

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value returned
-- by @p@.
--
-- > priority = option 0 (digitToInt <$> digit)

option :: Stream s m t => a -> ParsecT s u m a -> ParsecT s u m a
option x p = p <|> return x

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` (symbol ",")

sepBy :: Stream s m t =>
         ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.

sepBy1 :: Stream s m t =>
          ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@, i.e. C-style statements. Returns
-- a list of values returned by @p@.
--
-- > statements = statement `sepEndBy` semicolon

sepEndBy :: Stream s m t =>
            ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy p sep = sepEndBy1 p sep <|> return []

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

sepEndBy1 :: Stream s m t =>
             ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep = p >>= \x -> ((x:) <$> (sep *> sepEndBy p sep)) <|> return [x]

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- > spaces = skipMany space

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany p = void $ many p

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
-- its result.

skipSome :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
skipSome p = void $ some p
