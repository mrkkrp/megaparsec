-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's character parsers.
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

module Char (tests) where

import Data.Char (isSpace)
import Data.List (findIndex)

import Test.Framework
import Test.QuickCheck
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Text.Megaparsec.Char
import Text.Megaparsec.Combinator
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

import Util

tests :: Test
tests = testGroup "Character parsers"
        [ testProperty "oneOf" prop_oneOf
        , testProperty "noneOf" prop_noneOf
        , testProperty "spaces" prop_spaces ]

prop_oneOf :: String -> String -> Property
prop_oneOf s a = checkParser (oneOf a) s r
    where h = head s
          r | null s = posErr 0 s [suneStr ""]
            | length s == 1 && h `elem` a = Right h
            | h `notElem` a = posErr 0 s [suneCh h]
            | otherwise = posErr 1 s [uneCh (s !! 1), exStr ""]

prop_noneOf :: String -> String -> Property
prop_noneOf s a = checkParser (noneOf a) s r
    where h = head s
          r | null s = posErr 0 s [suneStr ""]
            | length s == 1 && h `notElem` a = Right h
            | h `elem` a = posErr 0 s [suneCh h]
            | otherwise = posErr 1 s [uneCh (s !! 1), exStr ""]

prop_spaces :: String -> Property
prop_spaces s = checkParser spaces s r
    where r = case findIndex (not . isSpace) s of
                Just x  ->
                    let ch = s !! x
                    in posErr x s
                           [ suneCh ch
                           , uneCh  ch
                           , exSpec "white space"
                           , exStr "" ]
                Nothing -> Right ()
