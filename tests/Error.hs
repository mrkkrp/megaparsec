--
-- QuickCheck tests for Megaparsec's parse errors.
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

module Error (tests) where

import Data.Function (on)
import Data.List (isInfixOf)
import Data.Monoid ((<>))
import Data.Set (Set)
import qualified Data.Set as E

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Text.Megaparsec.Error
import Text.Megaparsec.Pos

import Util ()

#if !MIN_VERSION_base(4,8,0)
import Data.Foldable (Foldable, all)
import Data.Monoid (mempty)
import Prelude hiding (all)
#endif

tests :: Test
tests = testGroup "Parse errors"
  [ testProperty "monoid left identity"               prop_monoid_left_id
  , testProperty "monoid right identity"              prop_monoid_right_id
  , testProperty "monoid associativity"               prop_monoid_assoc
  , testProperty "consistency of Show/Read"           prop_showReadConsistency
  , testProperty "position of merged error"           prop_mergeErrorPos
  , testProperty "unexpected items in merged error"   prop_mergeErrorUnexpected
  , testProperty "expected items in merged error"     prop_mergeErrorExpected
  , testProperty "custom items in merged error"       prop_mergeErrorCustom
  , testProperty "source position in rendered error"  prop_ppSourcePos
  , testProperty "unexpected items in rendered error" prop_ppUnexpected
  , testProperty "expected items in rendered error"   prop_ppExpected
  , testProperty "custom data in rendered error"      prop_ppCustom ]

type PE = ParseError Char Dec

prop_monoid_left_id :: PE -> Property
prop_monoid_left_id x = mempty <> x === x .&&.
  mempty { errorPos = errorPos x } <> x === x

prop_monoid_right_id :: PE -> Property
prop_monoid_right_id x = x <> mempty === x .&&.
  mempty { errorPos = errorPos x } <> x === x

prop_monoid_assoc :: PE -> PE -> PE -> Property
prop_monoid_assoc x y z = (x <> y) <> z === x <> (y <> z)

prop_showReadConsistency :: PE -> Property
prop_showReadConsistency x = read (show x) === x

prop_mergeErrorPos :: PE -> PE -> Property
prop_mergeErrorPos e1 e2 =
  errorPos (e1 <> e2) === max (errorPos e1) (errorPos e2)

prop_mergeErrorUnexpected :: PE -> PE -> Property
prop_mergeErrorUnexpected = checkMergedItems errorUnexpected

prop_mergeErrorExpected :: PE -> PE -> Property
prop_mergeErrorExpected = checkMergedItems errorExpected

prop_mergeErrorCustom :: PE -> PE -> Property
prop_mergeErrorCustom = checkMergedItems errorCustom

checkMergedItems :: (Ord a, Show a) => (PE -> Set a) -> PE -> PE -> Property
checkMergedItems f e1 e2 = f (e1 <> e2) === r
  where r = case (compare `on` errorPos) e1 e2 of
              LT -> f e2
              EQ -> (E.union `on` f) e1 e2
              GT -> f e1

prop_ppSourcePos :: PE -> Property
prop_ppSourcePos = checkPresence errorPos sourcePosPretty

prop_ppUnexpected :: PE -> Property
prop_ppUnexpected = checkPresence errorUnexpected showErrorComponent

prop_ppExpected :: PE -> Property
prop_ppExpected = checkPresence errorExpected showErrorComponent

prop_ppCustom :: PE -> Property
prop_ppCustom = checkPresence errorCustom showErrorComponent

checkPresence :: Foldable t => (PE -> t a) -> (a -> String) -> PE -> Property
checkPresence g r e = property (all f (g e))
  where rendered = parseErrorPretty e
        f x = r x `isInfixOf` rendered
