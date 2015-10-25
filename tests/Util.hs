-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec, utility functions for parser testing.
--
-- Copyright © 2015 Megaparsec contributors
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- This software is provided by the copyright holders "as is" and any
-- express or implied warranties, including, but not limited to, the implied
-- warranties of merchantability and fitness for a particular purpose are
-- disclaimed. In no event shall the copyright holders be liable for any
-- direct, indirect, incidental, special, exemplary, or consequential
-- damages (including, but not limited to, procurement of substitute goods
-- or services; loss of use, data, or profits; or business interruption)
-- however caused and on any theory of liability, whether in contract,
-- strict liability, or tort (including negligence or otherwise) arising in
-- any way out of the use of this software, even if advised of the
-- possibility of such damage.

module Util
  ( checkParser
  , simpleParse
  , checkChar
  , checkString
  , (/=\)
  , (!=!)
  , abcRow
  , abcRow'
  , posErr
  , uneCh
  , uneStr
  , uneSpec
  , uneEof
  , exCh
  , exStr
  , exSpec
  , exEof
  , msg
  , showToken )
where

import Data.Maybe (maybeToList)

import Test.QuickCheck

import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken
import Text.Megaparsec.String

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*))
#endif

-- | @checkParser p r s@ tries to run parser @p@ on input @s@ to parse
-- entire @s@. Result of the parsing is compared with expected result @r@,
-- it should match, otherwise the property doesn't hold and the test fails.

checkParser :: (Eq a, Show a)
            => Parser a -> Either ParseError a -> String -> Property
checkParser p r s = simpleParse p s === r

-- | @simpleParse p s@ runs parser @p@ on input @s@ and returns corresponding
-- result of type @Either ParseError a@, where @a@ is type of parsed
-- value. This parser tries to parser end of file too and name of input file
-- is always empty string.

simpleParse :: Parser a -> String -> Either ParseError a
simpleParse p = parse (p <* eof) ""

-- | @checkChar p test label s@ runs parser @p@ on input @s@ and checks if
-- the parser correctly parses single character that satisfies @test@. The
-- character may be labelled, in this case @label@ is used to check quality
-- of error messages.

checkChar :: Parser Char -> (Char -> Bool)
          -> Maybe String -> String -> Property
checkChar p f l' s = checkParser p r s
  where h = head s
        l = exSpec <$> maybeToList l'
        r | null s = posErr 0 s (uneEof : l)
          | length s == 1 && f h = Right h
          | not (f h) = posErr 0 s (uneCh h : l)
          | otherwise = posErr 1 s [uneCh (s !! 1), exEof]

-- | @checkString p a test label s@ runs parser @p@ on input @s@ and checks if
-- the result is equal to @a@ and also quality of error messages. @test@ is
-- used to compare tokens. @label@ is used as expected representation of
-- parser's result in error messages.

checkString :: Parser String -> String -> (Char -> Char -> Bool)
            -> String -> String -> Property
checkString p a' test l s' = checkParser p (w a' 0 s') s'
  where w [] _ []    = Right s'
        w [] i (s:_) = posErr i s' [uneCh s, exEof]
        w _  0 []    = posErr 0 s' [uneEof, exSpec l]
        w _  i []    = posErr 0 s' [uneStr (take i s'), exSpec l]
        w (a:as) i (s:ss)
          | test a s  = w as i' ss
          | otherwise = posErr 0 s' [uneStr (take i' s'), exSpec l]
            where i'  = succ i

infix 4 /=\   -- preserve whitespace on automatic trim

-- | @p /=\\ x@ runs parser @p@ on empty input and compares its result
-- (which should be successful) with @x@. Succeeds when the result is equal
-- to @x@, prints counterexample on failure.

(/=\) :: (Eq a, Show a) => Parser a -> a -> Property
p /=\ x = simpleParse p "" === Right x

infix 4 !=!

-- | @n !=! m@ represents property that holds when results of running @n@
-- and @m@ parsers are identical. This is useful when checking monad laws
-- for example.

(!=!) :: (Eq a, Show a) => Parser a -> Parser a -> Property
n !=! m = simpleParse n "" === simpleParse m ""

-- | @abcRow a b c@ generates string consisting of character “a” repeated
-- @a@ times, character “b” repeated @b@ times, and finally character “c”
-- repeated @c@ times.

abcRow :: Int -> Int -> Int -> String
abcRow a b c = replicate a 'a' ++ replicate b 'b' ++ replicate c 'c'

-- | @abcRow' a b c@ generates string that includes character “a” if @a@ is
-- 'True', then optionally character “b” if @b@ is 'True', then character
-- “c” if @c@ is 'True'.

abcRow' :: Bool -> Bool -> Bool -> String
abcRow' a b c = abcRow (fromEnum a) (fromEnum b) (fromEnum c)

-- | @posErr pos s ms@ is an easy way to model result of parser that
-- fails. @pos@ is how many tokens (characters) has been consumed before
-- failure. @s@ is input of the parser. @ms@ is a list, collection of
-- 'Message's. See 'uneStr', 'uneCh', 'uneSpec', 'exStr', 'exCh', and
-- 'exSpec' for easy ways to create error messages.

posErr :: Int -> String -> [Message] -> Either ParseError a
posErr pos s = Left . foldr addErrorMessage (newErrorUnknown errPos)
  where errPos = updatePosString defaultTabWidth (initialPos "") (take pos s)

-- | @uneCh s@ returns message created with 'Unexpected' constructor that
-- tells the system that char @s@ is unexpected.

uneCh :: Char -> Message
uneCh s = Unexpected $ showToken s

-- | @uneStr s@ returns message created with 'Unexpected' constructor that
-- tells the system that string @s@ is unexpected.

uneStr :: String -> Message
uneStr s = Unexpected $ showToken s

-- | @uneSpec s@ returns message created with 'Unexpected' constructor that
-- tells the system that @s@ is unexpected. This is different from 'uneStr'
-- in that it doesn't use 'showToken' but rather pass its argument unaltered
-- allowing for “special” labels.

uneSpec :: String -> Message
uneSpec = Unexpected

-- | @uneEof@ represents message “unexpected end of input”.

uneEof :: Message
uneEof = Unexpected "end of input"

-- | @exCh s@ returns message created with 'Expected' constructor that tells
-- the system that character @s@ is expected.

exCh :: Char -> Message
exCh s = Expected $ showToken s

-- | @exStr s@ returns message created with 'Expected' constructor that tells
-- the system that string @s@ is expected.

exStr :: String -> Message
exStr s = Expected $ showToken s

-- | @exSpec s@ returns message created with 'Expected' constructor that tells
-- the system that @s@ is expected. This is different from 'exStr' in that
-- it doesn't use 'showToken' but rather pass its argument unaltered
-- allowing for “special” labels.

exSpec :: String -> Message
exSpec = Expected

-- | @exEof@ represents message “expecting end of input”.

exEof :: Message
exEof = Expected "end of input"

-- | @msg s@ return message created with 'Message' constructor.

msg :: String -> Message
msg = Message
