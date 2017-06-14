{-# LANGUAGE CPP #-}
{-# OPTIONS -fno-warn-orphans #-}

module Text.Megaparsec.Types where

import Test.QuickCheck
import Text.Megaparsec.Error
import Text.Megaparsec.Pos
import Text.Megaparsec.Prim

instance (Arbitrary t, Ord t, Arbitrary e, Ord e)
    => Arbitrary (ParseError t e) where
  arbitrary = ParseError
#if MIN_VERSION_QuickCheck(2,9,0)
    <$> arbitrary
#else
    <$> (NE.fromList . getNonEmpty <$> arbitrary)
#endif
#if MIN_VERSION_QuickCheck(2,8,2)
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
#else
    <*> (E.fromList <$> arbitrary)
    <*> (E.fromList <$> arbitrary)
    <*> (E.fromList <$> arbitrary)
#endif

instance Arbitrary t => Arbitrary (ErrorItem t) where
  arbitrary = oneof
    [
#if !MIN_VERSION_QuickCheck(2,9,0)
      Tokens <$> (NE.fromList . getNonEmpty <$> arbitrary)
    , Label  <$> (NE.fromList . getNonEmpty <$> arbitrary)
#else
      Tokens <$> arbitrary
    , Label  <$> arbitrary
#endif
    , return EndOfInput ]

instance Arbitrary Pos where
  arbitrary = unsafePos <$> (getSmall <$> arbitrary `suchThat` (> 0))

instance Arbitrary SourcePos where
  arbitrary = SourcePos
    <$> sized (\n -> do
          k <- choose (0, n `div` 2)
          vectorOf k arbitrary)
    <*> (unsafePos <$> choose (1, 1000))
    <*> (unsafePos <$> choose (1,  100))

instance Arbitrary Dec where
  arbitrary = oneof
    [ sized (\n -> do
        k <- choose (0, n `div` 2)
        DecFail <$> vectorOf k arbitrary)
    , DecIndentation <$> arbitrary <*> arbitrary <*> arbitrary ]

instance Arbitrary a => Arbitrary (State a) where
  arbitrary = State
    <$> arbitrary
    <*>
#if !MIN_VERSION_QuickCheck(2,9,0)
      (NE.fromList . getNonEmpty <$> arbitrary)
#else
      arbitrary
#endif
    <*> choose (1, 10000)
    <*> (unsafePos <$> choose (1, 20))
