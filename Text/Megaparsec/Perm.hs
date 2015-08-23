-- |
-- Module      :  Text.Megaparsec.Perm
-- Copyright   :  © 2015 Megaparsec contributors
--                © 2007 Paolo Martini
--                © 1999–2001 Daan Leijen
-- License     :  BSD3
--
-- Maintainer  :  Mark Karpov <markkarpov@opmbx.org>
-- Stability   :  experimental
-- Portability :  non-portable (uses existentially quantified data constructors)
--
-- This module implements permutation parsers. The algorithm is described
-- in: /Parsing Permutation Phrases/, by Arthur Baars, Andres Loh and
-- Doaitse Swierstra. Published as a functional pearl at the Haskell
-- Workshop 2001.

module Text.Megaparsec.Perm
  ( PermParser
  , makePermParser
  , (<$$>)
  , (<$?>)
  , (<||>)
  , (<|?>) )
where

import Control.Monad.Identity

import Text.Megaparsec.Combinator (choice)
import Text.Megaparsec.Prim

infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>

-- | The type @PermParser s u a@ denotes a permutation parser that,
-- when converted by the 'makePermParser' function, parses @s@ stream with
-- user state @u@ and returns a value of type @a@ on success.
--
-- Normally, a permutation parser is first build with special operators like
-- ('<||>') and than transformed into a normal parser using
-- 'makePermParser'.

data PermParser s u a = Perm (Maybe a) [Branch s u a]

data Branch s u a = forall b. Branch (PermParser s u (b -> a)) (Parsec s u b)

-- | The parser @makePermParser perm@ parses a permutation of parser described
-- by @perm@. For example, suppose we want to parse a permutation of: an
-- optional string of @a@'s, the character @b@ and an optional @c@. This can
-- be described by:
--
-- > test = makePermParser $
-- >          (,,) <$?> ("", some (char 'a'))
-- >               <||> char 'b'
-- >               <|?> ('_', char 'c')

makePermParser :: Stream s Identity t => PermParser s u a -> Parsec s u a
makePermParser (Perm def xs) = choice (fmap branch xs ++ empty)
  where empty = case def of
                  Nothing -> []
                  Just x  -> [return x]
        branch (Branch perm p) = flip ($) <$> p <*> makePermParser perm

-- | The expression @f \<$$> p@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- not allowed to accept empty input — use the optional combinator ('<$?>')
-- instead.
--
-- If the function @f@ takes more than one parameter, the type variable @b@
-- is instantiated to a functional type which combines nicely with the adds
-- parser @p@ to the ('<||>') combinator. This results in stylized code
-- where a permutation parser starts with a combining function @f@ followed
-- by the parsers. The function @f@ gets its parameters in the order in
-- which the parsers are specified, but actual input can be in any order.

(<$$>) :: Stream s Identity t => (a -> b) -> Parsec s u a -> PermParser s u b
f <$$> p = newperm f <||> p

-- | The expression @f \<$?> (x, p)@ creates a fresh permutation parser
-- consisting of parser @p@. The the final result of the permutation parser
-- is the function @f@ applied to the return value of @p@. The parser @p@ is
-- optional — if it cannot be applied, the default value @x@ will be used
-- instead.

(<$?>) :: Stream s Identity t =>
          (a -> b) -> (a, Parsec s u a) -> PermParser s u b
f <$?> xp = newperm f <|?> xp

-- | The expression @perm \<||> p@ adds parser @p@ to the permutation
-- parser @perm@. The parser @p@ is not allowed to accept empty input — use
-- the optional combinator ('<|?>') instead. Returns a new permutation
-- parser that includes @p@.

(<||>) :: Stream s Identity t =>
          PermParser s u (a -> b) -> Parsec s u a -> PermParser s u b
(<||>) = add

-- | The expression @perm \<||> (x, p)@ adds parser @p@ to the
-- permutation parser @perm@. The parser @p@ is optional — if it cannot be
-- applied, the default value @x@ will be used instead. Returns a new
-- permutation parser that includes the optional parser @p@.

(<|?>) :: Stream s Identity t =>
          PermParser s u (a -> b) -> (a, Parsec s u a) -> PermParser s u b
perm <|?> (x, p) = addopt perm x p

newperm :: Stream s Identity t => (a -> b) -> PermParser s u (a -> b)
newperm f = Perm (Just f) []

add :: Stream s Identity t =>
       PermParser s u (a -> b) -> Parsec s u a -> PermParser s u b
add perm@(Perm _mf fs) p = Perm Nothing (first : fmap insert fs)
  where first = Branch perm p
        insert (Branch perm' p') = Branch (add (mapPerms flip perm') p) p'

addopt :: Stream s Identity t =>
          PermParser s u (a -> b) -> a -> Parsec s u a -> PermParser s u b
addopt perm@(Perm mf fs) x p = Perm (fmap ($ x) mf) (first : fmap insert fs)
  where first   = Branch perm p
        insert (Branch perm' p') = Branch (addopt (mapPerms flip perm') x p) p'

mapPerms :: Stream s Identity t =>
            (a -> b) -> PermParser s u a -> PermParser s u b
mapPerms f (Perm x xs) = Perm (fmap f x) (fmap mapBranch xs)
  where mapBranch (Branch perm p) = Branch (mapPerms (f .) perm) p
