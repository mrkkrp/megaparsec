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
  , choice
  , count
  , count'
  , endBy
  , endBy1
  , manyTill
  , someTill
  , option
  , sepBy
  , sepBy1
  , skipMany
  , skipSome
    -- Deprecated combinators
  , chainl
  , chainl1
  , chainr
  , chainr1
  , sepEndBy
  , sepEndBy1 )
where

import Control.Applicative ((<|>), many, some, optional)
import Control.Monad
import Data.Foldable (asum)

import Text.Megaparsec.Prim

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- > braces = between (symbol "{") (symbol "}")

between :: Stream s m t => ParsecT s u m open ->
           ParsecT s u m close -> ParsecT s u m a -> ParsecT s u m a
between open close p = open *> p <* close

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding parser.

choice :: (Foldable f, Stream s m t) => f (ParsecT s u m a) -> ParsecT s u m a
choice = asum

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of @n@
-- values.
--
-- This parser is defined in terms of 'count'', like this:
--
-- > count n = count' n n

count :: Stream s m t => Int -> ParsecT s u m a -> ParsecT s u m [a]
count n = count' n n

-- | @count\' m n p@ parses from @m@ to @n@ occurrences of @p@. If @n@ is
-- not positive or @m > n@, the parser equals to @return []@. Returns a list
-- of parsed values.
--
-- Please note that @m@ /may/ be negative, in this case effect is the same
-- as if it were equal to zero.

count' :: Stream s m t => Int -> Int -> ParsecT s u m a -> ParsecT s u m [a]
count' m n p
  | n <= 0 || m > n = return []
  | m > 0           = (:) <$> p <*> count' (pred m) (pred n) p
  | otherwise       = do
      result <- optional p
      case result of
        Nothing -> return []
        Just x  -> (x:) <$> count' 0 (pred n) p

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon

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
manyTill p end = (end *> return []) <|> someTill p end

-- | @someTill p end@ works similarly to @manyTill p end@, but @p@ should
-- succeed at least once.

someTill :: Stream s m t =>
            ParsecT s u m a -> ParsecT s u m end -> ParsecT s u m [a]
someTill p end = (:) <$> p <*> manyTill p end

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value returned
-- by @p@.
--
-- > priority = option 0 (digitToInt <$> digitChar)

option :: Stream s m t => a -> ParsecT s u m a -> ParsecT s u m a
option x p = p <|> return x

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma

sepBy :: Stream s m t =>
         ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy p sep = sepBy1 p sep <|> return []

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.

sepBy1 :: Stream s m t =>
          ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- > space = skipMany spaceChar

skipMany :: ParsecT s u m a -> ParsecT s u m ()
skipMany p = void $ many p

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
-- its result.

skipSome :: Stream s m t => ParsecT s u m a -> ParsecT s u m ()
skipSome p = void $ some p

-- Deprecated combinators

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are zero occurrences of @p@, the value @x@ is returned.

{-# DEPRECATED chainl "Use \"Text.Megaparsec.Expr\" instead." #-}

chainl :: Stream s m t =>
          ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainl p op x = chainl1 p op <|> return x

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. This parser can for example be used to eliminate left recursion
-- which typically occurs in expression grammars.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainl1 "Use \"Text.Megaparsec.Expr\" instead." #-}

chainl1 :: Stream s m t =>
           ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainl1 p op = p >>= rest
  where rest x = ((($ x) <$> op <*> p) >>= rest) <|> return x

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are no occurrences of @p@, the value @x@ is returned.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainr "Use \"Text.Megaparsec.Expr\" instead." #-}

chainr :: Stream s m t =>
          ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> a -> ParsecT s u m a
chainr p op x = chainr1 p op <|> return x

-- | @chainr1 p op@ parses /one/ or more occurrences of |p|,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainr1 "Use \"Text.Megaparsec.Expr\" instead." #-}

chainr1 :: Stream s m t =>
           ParsecT s u m a -> ParsecT s u m (a -> a -> a) -> ParsecT s u m a
chainr1 p op = p >>= rest
  where rest x = (($ x) <$> op <*> chainr1 p op) <|> return x

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

{-# DEPRECATED sepEndBy "Use @sepBy p sep <* optional sep@ instead." #-}

sepEndBy :: Stream s m t =>
            ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy p sep = sepBy p sep <* optional sep

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

{-# DEPRECATED sepEndBy1 "Use @sepBy1 p sep <* optional sep@ instead." #-}

sepEndBy1 :: Stream s m t =>
             ParsecT s u m a -> ParsecT s u m sep -> ParsecT s u m [a]
sepEndBy1 p sep = sepBy1 p sep <* optional sep
