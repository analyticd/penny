module Data.Monoid.Properties where

import Data.Monoid
import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck (testProperty)

-- | 'mappend' is associative

mappendAssociative
  :: (Eq a, Monoid a, Show a)
  => Gen a
  -> Property
mappendAssociative g =
  forAll g $ \a ->
  forAll g $ \b ->
  forAll g $ \c ->
  ((a `mappend` b) `mappend` c) === (a `mappend` (b `mappend` c))

-- | 'mempty' is the identity
memptyIdentity
  :: (Eq a, Monoid a, Show a)
  => Gen a
  -> Property
memptyIdentity g =
  forAll g $ \a ->
  (a `mappend` mempty === a) .&&. (mempty `mappend` a === a)

-- | 'mappend' is commutative.  This does not hold for all monoids;
-- however, you can use this where you do have a commutative monoid.

mappendCommutative
  :: (Eq a, Monoid a, Show a)
  => Gen a
  -> Property
mappendCommutative g =
  forAll g $ \a ->
  forAll g $ \b ->
  (a `mappend` b) === (b `mappend` a)

-- | Bundles all tests for all monoids

monoidProperties
  :: (Eq a, Monoid a, Show a)
  => Gen a
  -> TestTree
monoidProperties g = testGroup "monoid"
  [ testProperty "associativity" (mappendAssociative g)
  , testProperty "mempty identity" (memptyIdentity g)
  ]

-- | Bundles all tests for commutative monoids
commutativeMonoidProperties
  :: (Eq a, Monoid a, Show a)
  => Gen a
  -> TestTree
commutativeMonoidProperties g = testGroup "commutative monoid"
  [ testProperty "commutative" (mappendCommutative g)
  , monoidProperties g
  ]
