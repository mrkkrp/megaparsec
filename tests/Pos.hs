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

{-# OPTIONS -fno-warn-orphans #-}

module Pos (tests) where

import Data.Char (isAlphaNum)
import Data.List (intercalate, isInfixOf, elemIndices)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Pos

tests :: Test
tests = testGroup "Textual source positions"
        [ testProperty "components" prop_components
        , testProperty "show file name in source positions" prop_showFileName
        , testProperty "show line in source positions" prop_showLine
        , testProperty "show column in source positions" prop_showColumn
        , testProperty "initial position" prop_initialPos
        , testProperty "increment source line" prop_incSourceLine
        , testProperty "increment source column" prop_incSourceColumn
        , testProperty "set source name" prop_setSourceName
        , testProperty "set source line" prop_setSourceLine
        , testProperty "set source column" prop_setSourceColumn
        , testProperty "position updating" prop_updating ]

instance Arbitrary SourcePos where
  arbitrary = newPos <$> fileName <*> choose (1, 1000) <*> choose (0, 100)

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

prop_showFileName :: SourcePos -> Bool
prop_showFileName pos =
  if null name
  then '"'`notElem` shown
  else ("\"" ++ name ++ "\"") `isInfixOf` shown
  where name  = sourceName pos
        shown = show pos

prop_showLine :: SourcePos -> Bool
prop_showLine pos = ("line " ++ line) `isInfixOf` show pos
  where line = show $ sourceLine pos

prop_showColumn :: SourcePos -> Bool
prop_showColumn pos = ("column " ++ column) `isInfixOf` show pos
  where column = show $ sourceColumn pos

prop_initialPos :: String -> Bool
prop_initialPos n =
  sourceName   ipos == n &&
  sourceLine   ipos == 1 &&
  sourceColumn ipos == 1
  where ipos = initialPos n

prop_incSourceLine :: SourcePos -> NonNegative Int -> Bool
prop_incSourceLine pos l =
  d sourceName   id     pos incp &&
  d sourceLine   (+ l') pos incp &&
  d sourceColumn id     pos incp
  where l'   = getNonNegative l
        incp = incSourceLine pos l'

prop_incSourceColumn :: SourcePos -> NonNegative Int -> Bool
prop_incSourceColumn pos c =
  d sourceName   id     pos incp &&
  d sourceLine   id     pos incp &&
  d sourceColumn (+ c') pos incp
  where c'   = getNonNegative c
        incp = incSourceColumn pos c'

prop_setSourceName :: SourcePos -> String -> Bool
prop_setSourceName pos n =
  d sourceName   (const n) pos setp &&
  d sourceLine   id        pos setp &&
  d sourceColumn id        pos setp
  where setp = setSourceName pos n

prop_setSourceLine :: SourcePos -> Positive Int -> Bool
prop_setSourceLine pos l =
  d sourceName   id         pos setp &&
  d sourceLine   (const l') pos setp &&
  d sourceColumn id         pos setp
  where l'   = getPositive l
        setp = setSourceLine pos l'

prop_setSourceColumn :: SourcePos -> NonNegative Int -> Bool
prop_setSourceColumn pos c =
  d sourceName   id         pos setp &&
  d sourceLine   id         pos setp &&
  d sourceColumn (const c') pos setp
  where c'   = getNonNegative c
        setp = setSourceColumn pos c'

prop_updating :: Int -> SourcePos -> String -> Bool
prop_updating w pos "" = updatePosString w pos "" == pos
prop_updating w' pos s =
  d sourceName id           pos updated &&
  d sourceLine (+ inclines) pos updated &&
  cols >= mincols && ((last s /= '\t') || ((cols - 1) `rem` w == 0))
  where w        = if w' < 1 then defaultTabWidth else w
        updated  = updatePosString w' pos s
        cols     = sourceColumn updated
        newlines = elemIndices '\n' s
        creturns = elemIndices '\r' s
        inclines = length newlines
        total    = length s
        allctrls = newlines ++ creturns
        mincols  = if null allctrls
                   then total + sourceColumn pos
                   else total - maximum allctrls

d :: Eq b => (a -> b) -> (b -> b) -> a -> a -> Bool
d f g x y = g (f x) == f y
