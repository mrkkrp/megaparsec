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
-- Commonly used generic combinators. Note that all combinators works with
-- any 'Alternative' instances.

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

import Control.Applicative
import Control.Monad (void)
import Data.Foldable (asum)

-- | @between open close p@ parses @open@, followed by @p@ and @close@.
-- Returns the value returned by @p@.
--
-- > braces = between (symbol "{") (symbol "}")

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close
{-# INLINE between #-}

-- | @choice ps@ tries to apply the parsers in the list @ps@ in order,
-- until one of them succeeds. Returns the value of the succeeding parser.

choice :: (Foldable f, Alternative m) => f (m a) -> m a
choice = asum
{-# INLINE choice #-}

-- | @count n p@ parses @n@ occurrences of @p@. If @n@ is smaller or
-- equal to zero, the parser equals to @return []@. Returns a list of @n@
-- values.
--
-- This parser is defined in terms of 'count'', like this:
--
-- > count n = count' n n

count :: Alternative m => Int -> m a -> m [a]
count n = count' n n
{-# INLINE count #-}

-- | @count\' m n p@ parses from @m@ to @n@ occurrences of @p@. If @n@ is
-- not positive or @m > n@, the parser equals to @return []@. Returns a list
-- of parsed values.
--
-- Please note that @m@ /may/ be negative, in this case effect is the same
-- as if it were equal to zero.

count' :: Alternative m => Int -> Int -> m a -> m [a]
count' m n p
  | n <= 0 || m > n = pure []
  | m > 0           = (:) <$> p <*> count' (pred m) (pred n) p
  | otherwise       =
      let f t ts = maybe [] (:ts) t
      in f <$> optional p <*> count' 0 (pred n) p

-- | @endBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.
--
-- > cStatements = cStatement `endBy` semicolon

endBy :: Alternative m => m a -> m sep -> m [a]
endBy p sep = many (p <* sep)
{-# INLINE endBy #-}

-- | @endBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- and ended by @sep@. Returns a list of values returned by @p@.

endBy1 :: Alternative m => m a -> m sep -> m [a]
endBy1 p sep = some (p <* sep)
{-# INLINE endBy1 #-}

-- | @manyTill p end@ applies parser @p@ /zero/ or more times until
-- parser @end@ succeeds. Returns the list of values returned by @p@. This
-- parser can be used to scan comments:
--
-- > simpleComment = string "<!--" >> manyTill anyChar (try $ string "-->")
--
-- Note that we need to use 'try' since parsers @anyChar@ and @string
-- \"-->\"@ overlap and @string \"-->\"@ could consume input before failing.

manyTill :: Alternative m => m a -> m end -> m [a]
manyTill p end = (end *> pure []) <|> someTill p end
{-# INLINE manyTill #-}

-- | @someTill p end@ works similarly to @manyTill p end@, but @p@ should
-- succeed at least once.

someTill :: Alternative m => m a -> m end -> m [a]
someTill p end = (:) <$> p <*> manyTill p end
{-# INLINE someTill #-}

-- | @option x p@ tries to apply parser @p@. If @p@ fails without
-- consuming input, it returns the value @x@, otherwise the value returned
-- by @p@.
--
-- > priority = option 0 (digitToInt <$> digitChar)

option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x
{-# INLINE option #-}

-- | @sepBy p sep@ parses /zero/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.
--
-- > commaSep p = p `sepBy` comma

sepBy :: Alternative m => m a -> m sep -> m [a]
sepBy p sep = sepBy1 p sep <|> pure []
{-# INLINE sepBy #-}

-- | @sepBy1 p sep@ parses /one/ or more occurrences of @p@, separated
-- by @sep@. Returns a list of values returned by @p@.

sepBy1 :: Alternative m => m a -> m sep -> m [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)
{-# INLINE sepBy1 #-}

-- | @skipMany p@ applies the parser @p@ /zero/ or more times, skipping
-- its result.
--
-- > space = skipMany spaceChar

skipMany :: Alternative m => m a -> m ()
skipMany p = void $ many p
{-# INLINE skipMany #-}

-- | @skipSome p@ applies the parser @p@ /one/ or more times, skipping
-- its result.

skipSome :: Alternative m => m a -> m ()
skipSome p = void $ some p
{-# INLINE skipSome #-}

-- Deprecated combinators

-- | @chainl p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are zero occurrences of @p@, the value @x@ is returned.

{-# DEPRECATED chainl "Use \"Text.Megaparsec.Expr\" instead." #-}

chainl :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainl p op x = chainl1 p op <|> pure x
{-# INLINE chainl #-}

-- | @chainl1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /left/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. This parser can for example be used to eliminate left recursion
-- which typically occurs in expression grammars.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainl1 "Use \"Text.Megaparsec.Expr\" instead." #-}

chainl1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainl1 p op = scan
  where scan = flip id <$> p <*> rst
        rst  = (\f y g x -> g (f x y)) <$> op <*> p <*> rst <|> pure id
{-# INLINE chainl1 #-}

-- | @chainr p op x@ parses /zero/ or more occurrences of @p@,
-- separated by @op@ Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@. If there are no occurrences of @p@, the value @x@ is returned.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainr "Use \"Text.Megaparsec.Expr\" instead." #-}

chainr :: Alternative m => m a -> m (a -> a -> a) -> a -> m a
chainr p op x = chainr1 p op <|> pure x
{-# INLINE chainr #-}

-- | @chainr1 p op@ parses /one/ or more occurrences of @p@,
-- separated by @op@. Returns a value obtained by a /right/ associative
-- application of all functions returned by @op@ to the values returned by
-- @p@.
--
-- Consider using "Text.Megaparsec.Expr" instead.

{-# DEPRECATED chainr1 "Use \"Text.Megaparsec.Expr\" instead." #-}

chainr1 :: Alternative m => m a -> m (a -> a -> a) -> m a
chainr1 p op = scan where
  scan = flip id <$> p <*> rst
  rst  = (flip <$> op <*> scan) <|> pure id
{-# INLINE chainr1 #-}

-- | @sepEndBy p sep@ parses /zero/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

{-# DEPRECATED sepEndBy "Use @sepBy p sep <* optional sep@ instead." #-}

sepEndBy :: Alternative m => m a -> m sep -> m [a]
sepEndBy p sep = sepBy p sep <* optional sep
{-# INLINE sepEndBy #-}

-- | @sepEndBy1 p sep@ parses /one/ or more occurrences of @p@,
-- separated and optionally ended by @sep@. Returns a list of values
-- returned by @p@.

{-# DEPRECATED sepEndBy1 "Use @sepBy1 p sep <* optional sep@ instead." #-}

sepEndBy1 :: Alternative m => m a -> m sep -> m [a]
sepEndBy1 p sep = sepBy1 p sep <* optional sep
{-# INLINE sepEndBy1 #-}
