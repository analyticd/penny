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
import Penny.NonNegative
import Penny.Positive

instance Arbitrary a => Arbitrary (NonEmptySeq a) where
  arbitrary = NonEmptySeq <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Groups a b) where
  arbitrary = Groups <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary TimeZone where
  arbitrary = do
    mins <- choose (-1439, 1439)
    return $ TimeZone mins False ""

newtype HoursI = HoursI Int deriving Show

instance Arbitrary HoursI where
  arbitrary = HoursI <$> choose (0, 23)

newtype N20'23I = N20'23I Int deriving Show

instance Arbitrary N20'23I where
  arbitrary = N20'23I <$> choose (20, 23)

newtype N0'19I = N0'19I Int deriving Show

instance Arbitrary N0'19I where
  arbitrary = N0'19I <$> choose (0,19)


prop_hours :: HoursI -> Property
prop_hours (HoursI i) = case c'Hours'Int i of
  Nothing -> counterexample "copperization failed" $ property False
  Just copperHrs -> fromIntegral (c'Integer'NonNegative (dHours copperHrs))
    === i

prop_copperizeTimeZone :: TimeZone -> Property
prop_copperizeTimeZone z = case cTimeZone z of
  Nothing -> counterexample "copperization failed" $ property False
  Just copperZone -> let zone = dZone copperZone
    in counterexample (show zone)
        $ timeZoneMinutes (dZone copperZone) === timeZoneMinutes z

prop_N20'23 :: N20'23I -> Property
prop_N20'23 (N20'23I i) = case c'N20'23'Int i of
  Nothing -> counterexample "copperization failed" $ property False
  Just copperN20 -> fromIntegral (c'Integer'Positive (dN20'23 copperN20))
    === i

prop_N0'19 :: N0'19I -> Property
prop_N0'19 (N0'19I i) = case c'N0'19'Int i of
  Nothing -> counterexample "copperization failed" $ property False
  Just copperN20 -> fromIntegral (c'Integer'NonNegative (dN0'19 copperN20))
    === i


prop_concatThenGroup :: Groups Int Int -> Bool
prop_concatThenGroup groups = groupEithers (concatGroups groups) == groups

main = $(defaultMainGenerator)
