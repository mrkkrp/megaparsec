--
-- QuickCheck tests for Megaparsec's textual source positions.
--
-- Copyright © 2015–2016 Megaparsec contributors
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
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE CPP              #-}
{-# OPTIONS -fno-warn-orphans #-}

module Pos (tests) where

import Control.Monad.Catch
import Data.Function (on)
import Data.List (isInfixOf, elemIndices)
import Data.Semigroup ((<>))

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Pos
import Util (updatePosString)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>), pure)
#endif

tests :: Test
tests = testGroup "Textual source positions"
  [ testProperty "creation of Pos (mkPos)"              prop_mkPos
  , testProperty "creation of Pos (unsafePos)"          prop_unsafePos
  , testProperty "consistency of Show/Read for Pos"     prop_showReadPos
  , testProperty "Ord instance of Pos"                  prop_ordPos
  , testProperty "Semigroup instance of Pos"            prop_semigroupPos
  , testProperty "construction of initial position"     prop_initialPos
  , testProperty "consistency of Show/Read for SourcePos" prop_showReadSourcePos
  , testProperty "pretty-printing: visible file path"   prop_ppFilePath
  , testProperty "pretty-printing: visible line"        prop_ppLine
  , testProperty "pretty-printing: visible column"      prop_ppColumn
  , testProperty "default updating of source position"  prop_defaultUpdatePos ]

prop_mkPos :: Word -> Property
prop_mkPos x' = case mkPos x' of
  Left  e -> fromException e === Just InvalidPosException
  Right x -> unPos x === x'

prop_unsafePos :: Positive Word -> Property
prop_unsafePos x' = unPos (unsafePos x) === x
  where x = getPositive x'

prop_showReadPos :: Pos -> Property
prop_showReadPos x = read (show x) === x

prop_ordPos :: Pos -> Pos -> Property
prop_ordPos x y = compare x y === (compare `on` unPos) x y

prop_semigroupPos :: Pos -> Pos -> Property
prop_semigroupPos x y =
  x <> y === unsafePos (unPos x + unPos y) .&&.
  unPos (x <> y) === unPos x + unPos y

prop_initialPos :: String -> Property
prop_initialPos fp =
  sourceName   x === fp          .&&.
  sourceLine   x === unsafePos 1 .&&.
  sourceColumn x === unsafePos 1
  where x = initialPos fp

prop_showReadSourcePos :: SourcePos -> Property
prop_showReadSourcePos x = read (show x) === x

prop_ppFilePath :: SourcePos -> Property
prop_ppFilePath x = property $
  sourceName x `isInfixOf` sourcePosPretty x

prop_ppLine :: SourcePos -> Property
prop_ppLine x = property $
  (show . unPos . sourceLine) x `isInfixOf` sourcePosPretty x

prop_ppColumn :: SourcePos -> Property
prop_ppColumn x = property $
  (show . unPos . sourceColumn) x `isInfixOf` sourcePosPretty x

prop_defaultUpdatePos :: Pos -> SourcePos -> String -> Property
prop_defaultUpdatePos w pos "" = updatePosString w pos "" === pos
prop_defaultUpdatePos w pos s =
  sourceName updated === sourceName pos .&&.
  unPos (sourceLine updated) === unPos (sourceLine pos) + inclines .&&.
  cols >= mincols && ((last s /= '\t') || ((cols - 1) `rem` unPos w == 0))
  where
    updated  = updatePosString w pos s
    cols     = unPos (sourceColumn updated)
    newlines = elemIndices '\n' s
    inclines = fromIntegral (length newlines)
    total    = fromIntegral (length s)
    mincols  =
      if null newlines
        then total + unPos (sourceColumn pos)
        else total - fromIntegral (maximum newlines)
