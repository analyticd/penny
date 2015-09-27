module Main where

import Penny.Decimal
import Penny.Natural
import Penny.Polar

import Control.Applicative (liftA2)
import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , oneof
  , elements
  )
import qualified Test.QuickCheck as Q

-- # Instances

-- ## Numbers

instance Arbitrary Positive where
  arbitrary = do
    Q.Positive p <- arbitrary
    case integerToNatural p of
      Nothing -> error "could not generate Positive"
      Just r -> return r

instance Arbitrary Unsigned where
  arbitrary = do
    Q.NonNegative p <- arbitrary
    case integerToNatural p of
      Nothing -> error "could not generate Unsigned"
      Just r -> return r

-- ## Polar

instance Arbitrary Pole where
  arbitrary = elements [North, South]

instance Arbitrary a => Arbitrary (Polarized a) where
  arbitrary = liftA2 Polarized arbitrary arbitrary

instance (Arbitrary n, Arbitrary o) => Arbitrary (Moderated n o) where
  arbitrary = oneof
    [ fmap Moderate arbitrary
    , fmap Extreme arbitrary
    ]


-- ## Decimal

instance Arbitrary c => Arbitrary (Exponential c) where
  arbitrary = liftA2 Exponential arbitrary arbitrary

main :: IO ()
main = undefined
