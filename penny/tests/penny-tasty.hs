{-# LANGUAGE TemplateHaskell #-}
module Main where

import Data.Sequence.NonEmpty (NonEmptySeq(NonEmptySeq))
import Test.QuickCheck
import Test.Tasty.QuickCheck
import Test.Tasty.TH

import Penny.SeqUtil

instance Arbitrary a => Arbitrary (NonEmptySeq a) where
  arbitrary = NonEmptySeq <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Groups a b) where
  arbitrary = Groups <$> arbitrary <*> arbitrary <*> arbitrary

prop_concatThenGroup :: Groups Int Int -> Bool
prop_concatThenGroup groups = groupEithers (concatGroups groups) == groups

main = $(defaultMainGenerator)
