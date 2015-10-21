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

#if MIN_VERSION_base(4,7,0)
import Data.Bool (bool)
#endif
import Data.List (isPrefixOf, isInfixOf)
import Data.Monoid ((<>))

import Test.Framework
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck

import Pos ()
import Text.Megaparsec.Error
import Text.Megaparsec.Pos

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>), (<*>))
#endif
#if !MIN_VERSION_base(4,7,0)
bool :: a -> a -> Bool -> a
bool f _ False = f
bool _ t True  = t
#endif

tests :: Test
tests = testGroup "Parse errors"
        [ testProperty "monoid left identity" prop_monoid_left_id
        , testProperty "monoid right identity" prop_monoid_right_id
        , testProperty "monoid associativity" prop_monoid_assoc
        , testProperty "extraction of message string" prop_messageString
        , testProperty "creation of new error messages" prop_newErrorMessage
        , testProperty "messages are always well-formed" prop_wellFormedMessages
        , testProperty "copying of error positions" prop_parseErrorCopy
        , testProperty "setting of error position" prop_setErrorPos
        , testProperty "addition of error message" prop_addErrorMessage
        , testProperty "setting of error message" prop_setErrorMessage
        , testProperty "position of merged error" prop_mergeErrorPos
        , testProperty "messages of merged error" prop_mergeErrorMsgs
        , testProperty "position of error is visible" prop_visiblePos
        , testProperty "message components are visible" prop_visibleMsgs ]

instance Arbitrary Message where
  arbitrary = ($) <$> elements constructors <*> arbitrary
    where constructors = [Unexpected, Expected, Message]

instance Arbitrary ParseError where
  arbitrary = do
    ms  <- listOf arbitrary
    err <- oneof [ newErrorUnknown <$> arbitrary
                 , newErrorMessage <$> arbitrary <*> arbitrary ]
    return $ addErrorMessages ms err

prop_monoid_left_id :: ParseError -> Bool
prop_monoid_left_id x = mempty <> x == x

prop_monoid_right_id :: ParseError -> Bool
prop_monoid_right_id x = x <> mempty == x

prop_monoid_assoc :: ParseError -> ParseError -> ParseError -> Bool
prop_monoid_assoc x y z = (x <> y) <> z == x <> (y <> z)

prop_messageString :: Message -> Bool
prop_messageString m@(Unexpected s) = s == messageString m
prop_messageString m@(Expected   s) = s == messageString m
prop_messageString m@(Message    s) = s == messageString m

prop_newErrorMessage :: Message -> SourcePos -> Bool
prop_newErrorMessage msg pos = added && errorPos new == pos
  where new   = newErrorMessage msg pos
        added = errorMessages new == bool [msg] [] (badMessage msg)

prop_wellFormedMessages :: ParseError -> Bool
prop_wellFormedMessages = wellFormed . errorMessages

prop_parseErrorCopy :: ParseError -> Bool
prop_parseErrorCopy err =
  foldr addErrorMessage (newErrorUnknown pos) msgs == err
  where pos  = errorPos err
        msgs = errorMessages err

prop_setErrorPos :: SourcePos -> ParseError -> Bool
prop_setErrorPos pos err =
  errorPos new == pos && errorMessages new == errorMessages err
  where new = setErrorPos pos err

prop_addErrorMessage :: Message -> ParseError -> Bool
prop_addErrorMessage msg err =
  wellFormed msgs && (badMessage msg || added)
  where new   = addErrorMessage msg err
        msgs  = errorMessages new
        added = msg `elem` msgs && not (errorIsUnknown new)

prop_setErrorMessage :: Message -> ParseError -> Bool
prop_setErrorMessage msg err =
  wellFormed msgs && (badMessage msg || (added && unique))
  where new    = setErrorMessage msg err
        msgs   = errorMessages new
        added  = msg `elem` msgs && not (errorIsUnknown new)
        unique = length (filter (== fromEnum msg) (fromEnum <$> msgs)) == 1

prop_mergeErrorPos :: ParseError -> ParseError -> Bool
prop_mergeErrorPos e1 e2 = errorPos (mergeError e1 e2) == max pos1 pos2
  where pos1 = errorPos e1
        pos2 = errorPos e2

prop_mergeErrorMsgs :: ParseError -> ParseError -> Bool
prop_mergeErrorMsgs e1 e2' = errorPos e1 /= errorPos e2 || wellFormed msgsm
  where e2    = setErrorPos (errorPos e1) e2'
        msgsm = errorMessages $ mergeError e1 e2

prop_visiblePos :: ParseError -> Bool
prop_visiblePos err = show (errorPos err) `isPrefixOf` show err

prop_visibleMsgs :: ParseError -> Bool
prop_visibleMsgs err = if null msgs
                       then "unknown" `isInfixOf` shown
                       else all (`isInfixOf` shown) (msgs >>= f)
  where shown = show err
        msgs  = errorMessages err
        f (Unexpected s) = ["unexpected", s]
        f (Expected   s) = ["expecting", s]
        f (Message    s) = [s]

-- | @wellFormed xs@ checks that list @xs@ is sorted and contains no
-- duplicates and no empty messages.

wellFormed :: [Message] -> Bool
wellFormed xs = and (zipWith (<) xs (tail xs)) && not (any badMessage xs)
