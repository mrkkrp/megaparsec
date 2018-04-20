-- |
-- Module      :  Text.Megaparsec.Perm
-- Copyright   :  © 2015–2018 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  FreeBSD
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- This module implements permutation parsers. The algorithm is described
-- in: /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and
-- Doaitse Swierstra. Published as a functional pearl at the Haskell
-- Workshop 2001.

{-# LANGUAGE ExistentialQuantification #-}

module Text.Megaparsec.Perm
  ( PermParser
  , makePermParser
  , (<$$>)
  , (<$?>)
  , (<||>)
  , (<|?>) )
where

import Text.Megaparsec

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

-- | The type @PermParser s m a@ denotes a permutation parser that, when
-- converted by the 'makePermParser' function, produces instance of
-- 'MonadParsec' @m@ that parses @s@ stream and returns a value of type @a@
-- on success.
--
-- Normally, a permutation parser is first build with special operators like
-- ('<||>') and than transformed into a normal parser using
-- 'makePermParser'.

data PermParser s m a = Perm (Maybe a) [Branch s m a]

data Branch s m a = forall b. Branch (PermParser s m (b -> a)) (m b)

-- | The parser @makePermParser perm@ parses a permutation of parser
-- described by @perm@. For example, suppose we want to parse a permutation
-- of: an optional string of @a@'s, the character @b@ and an optional @c@.
-- This can be described by:
--
-- > test = makePermParser $
-- >          (,,) <$?> ("", some (char 'a'))
-- >               <||> char 'b'
-- >               <|?> ('_', char 'c')

makePermParser :: MonadParsec e s m
  => PermParser s m a -- ^ Given permutation parser
  -> m a              -- ^ Normal parser built from it
makePermParser (Perm def xs) = choice (fmap branch xs ++ empty')
  where empty' =
          case def of
            Nothing -> []
            Just x  -> [return x]
        branch (Branch perm p) = flip ($) <$> p <*> makePermParser perm

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- not allowed to accept empty input—use the optional combinator ('<$?>')
-- instead.
--
-- If the function @f@ takes more than one parameter, the type variable @b@
-- is instantiated to a functional type which combines nicely with the adds
-- parser @p@ to the ('<||>') combinator. This results in stylized code
-- where a permutation parser starts with a combining function @f@ followed
-- by the parsers. The function @f@ gets its parameters in the order in
-- which the parsers are specified, but actual input can be in any order.

(<$$>) :: MonadParsec e s m
  => (a -> b)          -- ^ Function to use on result of parsing
  -> m a               -- ^ Normal parser
  -> PermParser s m b  -- ^ Permutation parser build from it
f <$$> p = newperm f <||> p

-- | The expression @f \<$?> (x, p)@ creates a fresh permutation parser
-- consisting of parser @p@. The final result of the permutation parser is
-- the function @f@ applied to the return value of @p@. The parser @p@ is
-- optional—if it cannot be applied, the default value @x@ will be used
-- instead.

(<$?>) :: MonadParsec e s m
  => (a -> b)          -- ^ Function to use on result of parsing
  -> (a, m a)          -- ^ Default value and parser
  -> PermParser s m b  -- ^ Permutation parser
f <$?> xp = newperm f <|?> xp

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation parser
-- @perm@. The parser @p@ is not allowed to accept empty input—use the
-- optional combinator ('<|?>') instead. Returns a new permutation parser
-- that includes @p@.

(<||>) :: MonadParsec e s m
  => PermParser s m (a -> b) -- ^ Given permutation parser
  -> m a               -- ^ Parser to add (should not accept empty input)
  -> PermParser s m b  -- ^ Resulting parser
(<||>) = add

-- | The expression @perm \<||> (x, p)@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is optional—if it cannot be applied, the
-- default value @x@ will be used instead. Returns a new permutation parser
-- that includes the optional parser @p@.

(<|?>) :: MonadParsec e s m
  => PermParser s m (a -> b) -- ^ Given permutation parser
  -> (a, m a)          -- ^ Default value and parser
  -> PermParser s m b  -- ^ Resulting parser
perm <|?> (x, p) = addopt perm x p

newperm :: (a -> b) -> PermParser s m (a -> b)
newperm f = Perm (Just f) []

add :: MonadParsec e s m => PermParser s m (a -> b) -> m a -> PermParser s m b
add perm@(Perm _mf fs) p = Perm Nothing (first : fmap insert fs)
  where first = Branch perm p
        insert (Branch perm' p') = Branch (add (mapPerms flip perm') p) p'

addopt :: MonadParsec e s m
  => PermParser s m (a -> b)
  -> a
  -> m a
  -> PermParser s m b
addopt perm@(Perm mf fs) x p = Perm (fmap ($ x) mf) (first : fmap insert fs)
  where first   = Branch perm p
        insert (Branch perm' p') = Branch (addopt (mapPerms flip perm') x p) p'

mapPerms :: MonadParsec e s m
  => (a -> b)
  -> PermParser s m a
  -> PermParser s m b
mapPerms f (Perm x xs) = Perm (fmap f x) (fmap mapBranch xs)
  where mapBranch (Branch perm p) = Branch (mapPerms (f .) perm) p
