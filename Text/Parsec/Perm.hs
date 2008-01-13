-----------------------------------------------------------------------------
-- |
-- Module      :  Text.Parsec.Perm
-- Copyright   :  (c) Daan Leijen 1999-2001, (c) Paolo Martini 2007
-- License     :  BSD-style (see the file libraries/parsec/LICENSE)
-- 
-- Maintainer  :  paolo@nemail.it
-- Stability   :  provisional
-- Portability :  non-portable (uses existentially quantified data constructors)
-- 
-- This module implements permutation parsers. The algorithm used
-- is fairly complex since we push the type system to its limits :-)
-- The algorithm is described in:
-- 
-- /Parsing Permutation Phrases,/
-- by Arthur Baars, Andres Loh and Doaitse Swierstra.
-- Published as a functional pearl at the Haskell Workshop 2001.
-- 
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Text.Parsec.Perm
    ( PermParser  -- abstract

    , permute
    , (<||>), (<$$>)
    , (<|?>), (<$?>)
    ) where

import Text.Parsec
import Text.Parsec.String

import Control.Monad.Identity

{---------------------------------------------------------------

---------------------------------------------------------------}
infixl 1 <||>, <|?>
infixl 2 <$$>, <$?>


{---------------------------------------------------------------
  test -- parse a permutation of
  * an optional string of 'a's
  * a required 'b'
  * an optional 'c'
---------------------------------------------------------------}
test input
  = parse (do{ x <- ptest; eof; return x }) "" input

ptest :: Parser (String,Char,Char)
ptest
  = permute $
    (,,) <$?> ("",many1 (char 'a'))
         <||> char 'b'
         <|?> ('_',char 'c')


{---------------------------------------------------------------
  Building a permutation parser
---------------------------------------------------------------}
(<||>) :: (Stream s Identity tok) => StreamPermParser s tok st (a -> b) -> Parsec s st a -> StreamPermParser s tok st b
(<||>) perm p     = add perm p

(<$$>) :: (Stream s Identity tok) => (a -> b) -> Parsec s st a -> StreamPermParser s tok st b
(<$$>) f p        = newperm f <||> p

(<|?>) :: (Stream s Identity tok) => StreamPermParser s tok st (a -> b) -> (a, Parsec s st a) -> StreamPermParser s tok st b
(<|?>) perm (x,p) = addopt perm x p

(<$?>) :: (Stream s Identity tok) => (a -> b) -> (a, Parsec s st a) -> StreamPermParser s tok st b
(<$?>) f (x,p)    = newperm f <|?> (x,p)



{---------------------------------------------------------------
  The permutation tree
---------------------------------------------------------------}
type PermParser tok st a = StreamPermParser String tok st a
data StreamPermParser s tok st a = Perm (Maybe a) [StreamBranch s tok st a]
type Branch tok st a = StreamBranch String tok st a
data StreamBranch s tok st a = forall b. Branch (StreamPermParser s tok st (b -> a)) (Parsec s st b)
-- data Branch tok st a     = forall b. Branch (PermParser tok st (b -> a)) (GenParser tok st b)


-- transform a permutation tree into a normal parser
permute :: (Stream s Identity tok) => StreamPermParser s tok st a -> Parsec s st a
permute (Perm def xs)
  = choice (map branch xs ++ empty)
  where
    empty
      = case def of
          Nothing -> []
          Just x  -> [return x]

    branch (Branch perm p)
      = do{ x <- p
          ; f <- permute perm
          ; return (f x)
          }

-- build permutation trees
newperm :: (Stream s Identity tok) => (a -> b) -> StreamPermParser s tok st (a -> b)
newperm f
  = Perm (Just f) []

add :: (Stream s Identity tok) => StreamPermParser s tok st (a -> b) -> Parsec s st a -> StreamPermParser s tok st b
add perm@(Perm mf fs) p
  = Perm Nothing (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (add (mapPerms flip perm') p) p'

addopt :: (Stream s Identity tok) => StreamPermParser s tok st (a -> b) -> a -> Parsec s st a -> StreamPermParser s tok st b
addopt perm@(Perm mf fs) x p
  = Perm (fmap ($ x) mf) (first:map insert fs)
  where
    first   = Branch perm p
    insert (Branch perm' p')
            = Branch (addopt (mapPerms flip perm') x p) p'


mapPerms :: (Stream s Identity tok) => (a -> b) -> StreamPermParser s tok st a -> StreamPermParser s tok st b
mapPerms f (Perm x xs)
  = Perm (fmap f x) (map (mapBranch f) xs)
  where
    mapBranch f (Branch perm p)
      = Branch (mapPerms (f.) perm) p
