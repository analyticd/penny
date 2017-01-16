{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Sequence.NonEmpty (NonEmptySeq(NonEmptySeq))
import Data.Time
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Penny.SeqUtil
import Penny.Copper.Copperize
import Penny.Copper.Decopperize

instance Arbitrary a => Arbitrary (NonEmptySeq a) where
  arbitrary = NonEmptySeq <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Groups a b) where
  arbitrary = Groups <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TimeZone where
  arbitrary = do
    mins <- choose (-2399, 2399)
    return $ TimeZone mins False ""

prop_copperizeTimeZone :: TimeZone -> Bool
prop_copperizeTimeZone z = case cTimeZone z of
  Nothing -> False
  Just copperZone -> timeZoneMinutes (dZone copperZone) == timeZoneMinutes z

prop_concatThenGroup :: Groups Int Int -> Bool
prop_concatThenGroup groups = groupEithers (concatGroups groups) == groups

main = $(defaultMainGenerator)
