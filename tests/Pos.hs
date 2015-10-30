-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's textual source positions.
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
-- This software is provided by the copyright holders “as is” and any
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

{-# OPTIONS -fno-warn-orphans #-}

module Pos (tests) where

import Data.Char (isAlphaNum)
import Data.List (intercalate, isInfixOf, elemIndices)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif

tests :: Test
tests = testGroup "Textual source positions"
        [ testProperty "components"                         prop_components
        , testProperty "positive coordinates"               prop_positive
        , testProperty "show file name in source positions" prop_showFileName
        , testProperty "show line in source positions"      prop_showLine
        , testProperty "show column in source positions"    prop_showColumn
        , testProperty "initial position"                   prop_initialPos
        , testProperty "increment source line"              prop_incSourceLine
        , testProperty "increment source column"            prop_incSourceColumn
        , testProperty "set source name"                    prop_setSourceName
        , testProperty "set source line"                    prop_setSourceLine
        , testProperty "set source column"                  prop_setSourceColumn
        , testProperty "position updating"                  prop_updating ]

instance Arbitrary SourcePos where
  arbitrary = newPos <$> fileName <*> choose (-10, 1000) <*> choose (-10, 100)

fileName :: Gen String
fileName = do
  delimiter <- pure <$> elements "/\\"
  dirs      <- listOf1 simpleName
  extension <- simpleName
  frequency [ (1, return [])
            , (7, return $ intercalate delimiter dirs ++ "." ++ extension)]
  where simpleName = listOf1 (arbitrary `suchThat` isAlphaNum)

prop_components :: SourcePos -> Bool
prop_components pos = pos == copy
  where copy = newPos (sourceName pos) (sourceLine pos) (sourceColumn pos)

prop_positive :: SourcePos -> Bool
prop_positive pos = sourceLine pos > 0 && sourceColumn pos > 0

prop_showFileName :: SourcePos -> Bool
prop_showFileName pos = sourceName pos `isInfixOf` show pos

prop_showLine :: SourcePos -> Bool
prop_showLine pos = show (sourceLine pos) `isInfixOf` show pos

prop_showColumn :: SourcePos -> Bool
prop_showColumn pos = show (sourceColumn pos) `isInfixOf` show pos

prop_initialPos :: String -> Bool
prop_initialPos n =
  sourceName   ipos == n &&
  sourceLine   ipos == 1 &&
  sourceColumn ipos == 1
  where ipos = initialPos n

prop_incSourceLine :: SourcePos -> Int -> Bool
prop_incSourceLine pos l =
  d sourceName   id pos incp &&
  d sourceLine   f  pos incp &&
  d sourceColumn id pos incp
  where f    = max 1 . (+ l)
        incp = incSourceLine pos l

prop_incSourceColumn :: SourcePos -> Int -> Bool
prop_incSourceColumn pos c =
  d sourceName   id pos incp &&
  d sourceLine   id pos incp &&
  d sourceColumn f  pos incp
  where f    = max 1 . (+ c)
        incp = incSourceColumn pos c

prop_setSourceName :: SourcePos -> String -> Bool
prop_setSourceName pos n =
  d sourceName   (const n) pos setp &&
  d sourceLine   id        pos setp &&
  d sourceColumn id        pos setp
  where setp = setSourceName pos n

prop_setSourceLine :: SourcePos -> Int -> Bool
prop_setSourceLine pos l =
  d sourceName   id         pos setp &&
  d sourceLine   (const l') pos setp &&
  d sourceColumn id         pos setp
  where l'   = max 1 l
        setp = setSourceLine pos l

prop_setSourceColumn :: SourcePos -> Int -> Bool
prop_setSourceColumn pos c =
  d sourceName   id         pos setp &&
  d sourceLine   id         pos setp &&
  d sourceColumn (const c') pos setp
  where c'   = max 1 c
        setp = setSourceColumn pos c

prop_updating :: Int -> SourcePos -> String -> Bool
prop_updating w pos "" = updatePosString w pos "" == pos
prop_updating w' pos s =
  d sourceName id           pos updated &&
  d sourceLine (+ inclines) pos updated &&
  cols >= mincols && ((last s /= '\t') || ((cols - 1) `rem` w == 0))
  where w        = if w' < 1 then defaultTabWidth else w'
        updated  = updatePosString w' pos s
        cols     = sourceColumn updated
        newlines = elemIndices '\n' s
        inclines = length newlines
        total    = length s
        mincols  = if null newlines
                   then total + sourceColumn pos
                   else total - maximum newlines

d :: Eq b => (a -> b) -> (b -> b) -> a -> a -> Bool
d f g x y = g (f x) == f y
