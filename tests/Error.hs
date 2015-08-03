-- -*- Mode: Haskell; -*-
--
-- QuickCheck tests for Megaparsec's parse errors.
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

module Error (tests) where

import Data.Bool (bool)
import Data.List (nub, sort, isPrefixOf, isInfixOf)

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Pos ()
import Text.Megaparsec.Error
import Text.Megaparsec.Pos

tests :: Test
tests = testGroup "Parse errors"
        [ testProperty "extracting message string" prop_messageString
        , testProperty "creation of new error messages" prop_newErrorMessage
        , testProperty "error messages are always sorted" prop_sortedMessages
        , testProperty "copying of error positions" prop_parseErrorCopy
        , testProperty "setting of error position" prop_setErrorPos
        , testProperty "addition of error message" prop_addErrorMessage
        , testProperty "position of merged error" prop_mergeErrorPos
        , testProperty "messages of merged error" prop_mergeErrorMsgs
        , testProperty "position of error is visible" prop_visiblePos
        , testProperty "message components are visible" prop_visibleMsgs ]

instance Arbitrary Message where
    arbitrary = ($) <$> elements constructors <*> arbitrary
        where constructors = [SysUnExpect, UnExpect, Expect, Message]

instance Arbitrary ParseError where
    arbitrary = do
      ms <- listOf arbitrary
      pe <- oneof [ newErrorUnknown <$> arbitrary
                  , newErrorMessage <$> arbitrary <*> arbitrary ]
      return $ foldr addErrorMessage pe ms

prop_messageString :: Message -> Bool
prop_messageString m@(SysUnExpect s) = s == messageString m
prop_messageString m@(UnExpect    s) = s == messageString m
prop_messageString m@(Expect      s) = s == messageString m
prop_messageString m@(Message     s) = s == messageString m

prop_newErrorMessage :: Message -> SourcePos -> Bool
prop_newErrorMessage msg pos =
    errorMessages new == [msg] && errorPos new == pos
    where new = newErrorMessage msg pos

prop_sortedMessages :: ParseError -> Bool
prop_sortedMessages err = isSorted $ errorMessages err
    where isSorted xs = and $ zipWith (<=) xs (tail xs)

prop_parseErrorCopy :: ParseError -> Bool
prop_parseErrorCopy err =
    foldr addErrorMessage (newErrorUnknown pos) messages == err
    where pos      = errorPos err
          messages = errorMessages err

prop_setErrorPos :: SourcePos -> ParseError -> Bool
prop_setErrorPos pos err =
    errorPos new == pos && errorMessages new == errorMessages err
    where new = setErrorPos pos err

prop_addErrorMessage :: Message -> ParseError -> Bool
prop_addErrorMessage msg err =
    msg `elem` errorMessages new && not (errorIsUnknown new)
    where new = addErrorMessage msg err

prop_mergeErrorPos :: ParseError -> ParseError -> Bool
prop_mergeErrorPos e1 e2 = errorPos (mergeError e1 e2) == min pos1 pos2
    where pos1 = errorPos e1
          pos2 = errorPos e2

prop_mergeErrorMsgs :: ParseError -> ParseError -> Bool
prop_mergeErrorMsgs e1 e2' =
    errorPos e1 /= errorPos e2 || msgsm == sort (msgs1 ++ msgs2)
    where e2 = setErrorPos (errorPos e1) e2'
          msgsm = errorMessages $ mergeError e1 e2
          msgs1 = errorMessages e1
          msgs2 = errorMessages e2

prop_visiblePos :: ParseError -> Bool
prop_visiblePos err = show (errorPos err) `isPrefixOf` show err

prop_visibleMsgs :: ParseError -> Bool
prop_visibleMsgs err = all (`isInfixOf` shown) msgelts
    where shown   = show err
          msgs    = errorMessages err
          msgelts = nub $ msgs >>= f
          unexps  = 1 `elem` (fromEnum <$> msgs)
          f (SysUnExpect "") = bool ["unexpected", "end of input"] [] unexps
          f (SysUnExpect  s) = bool ["unexpected", s] [] unexps
          f (UnExpect    "") = []
          f (UnExpect     s) = ["unexpected", s]
          f (Expect      "") = []
          f (Expect       s) = ["expecting", s]
          f (Message     "") = []
          f (Message      s) = [s]
