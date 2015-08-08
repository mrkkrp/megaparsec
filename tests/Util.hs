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
    , posErr
    , suneStr
    , suneCh
    , uneStr
    , uneCh
    , exStr
    , exCh
    , exSpec )
where

import Test.QuickCheck

import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim
import Text.Megaparsec.ShowToken
import Text.Megaparsec.String

-- | @checkParser p s r@ tries to run parser @p@ on input @s@ to parse
-- entire @s@. Result of the parsing is compared with expected result @r@,
-- it should match, otherwise the property doesn't hold and the test fails.

checkParser :: (Eq a, Show a) =>
               Parser a -> String -> Either ParseError a -> Property
checkParser p s r = parse (p <* eof) "" s === r

-- | @posErr pos s ms@ is an easy way to model result of parser that
-- fails. @pos@ is how many tokens (characters) has been consumed before
-- failure. @s@ is input of the parser. @ms@ is a list, collection of
-- 'Message's. See 'uneStr', 'uneCh', 'exStr', and 'exCh' for easy ways to
-- create error messages.

posErr :: Int -> String -> [Message] -> Either ParseError a
posErr pos s = Left . foldr addErrorMessage (newErrorUnknown errPos)
    where errPos = updatePosString (initialPos "") (take pos s)

-- | @suneStr s@ returns message created with 'SysUnExpect' constructor that
-- tells the system that string @s@ is unexpected.

suneStr :: String -> Message
suneStr s = SysUnExpect $ showToken s

-- | @suneCh s@ returns message created with 'SysUnExpect' constructor that
-- tells the system that char @s@ is unexpected.

suneCh :: Char -> Message
suneCh s = SysUnExpect $ showToken s

-- | @uneStr s@ returns message created with 'UnExpect' constructor that
-- tells the system that string @s@ is unexpected.

uneStr :: String -> Message
uneStr s = UnExpect $ showToken s

-- | @uneCh s@ returns message created with 'UnExpect' constructor that
-- tells the system that char @s@ is unexpected.

uneCh :: Char -> Message
uneCh s = UnExpect $ showToken s

-- | @exStr s@ returns message created with 'Expect' constructor that tells
-- the system that string @s@ is expected. This can be used to expect end of
-- input, if @s@ is empty.

exStr :: String -> Message
exStr s = Expect $ if null s then "end of input" else showToken s

-- | @exCh s@ returns message created with 'Expect' constructor that tells
-- the system that character @s@ is expected.

exCh :: Char -> Message
exCh s = Expect $ showToken s

-- | @exSpec s@ returns message created with 'Expect' constructor that tells
-- the system that string @s@ is expected.

exSpec :: String -> Message
exSpec = Expect
