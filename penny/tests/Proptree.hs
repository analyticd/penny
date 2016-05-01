module Proptree where

import Test.Tasty
import Test.Tasty.QuickCheck

associative
  :: (Eq a, Arbitrary a, Show a)
  => (a -> a -> a)
  -> TestTree
associative f = testProperty "is associative" $
  \a b c -> (a `f` (b `f` c)) === ((a `f` b) `f` c)

commutative
  :: (Eq a, Arbitrary a, Show a)
  => (a -> a -> a)
  -> TestTree
commutative f = testProperty "is commutative" $
  \a b -> (a `f` b) === (b `f` a)

associativeWit
  :: (Eq a, Arbitrary a, Show a)
  => a
  -> (a -> a -> a)
  -> TestTree
associativeWit wit f = testProperty "is associative" $
  \a b c -> (a `f` (b `f` c)) === ((a `f` b) `f` (c `asTypeOf` wit))

commutativeWit
  :: (Eq a, Arbitrary a, Show a)
  => a
  -> (a -> a -> a)
  -> TestTree
commutativeWit wit f = testProperty "is commutative" $
  \a b -> (a `f` b) === (b `f` (a `asTypeOf` wit))

leftIdentity
  :: (Eq a, Arbitrary a, Show a)
  => a
  -> (a -> a -> a)
  -> TestTree
leftIdentity i f = testProperty
  (show i ++ " is left identity") $
  \a -> (i `f` a) === a

rightIdentity
  :: (Eq a, Arbitrary a, Show a)
  => a
  -> (a -> a -> a)
  -> TestTree
rightIdentity i f = testProperty
  (show i ++ " is right identity") $
  \a -> (a `f` i) === a

inverse
  :: (Eq a, Show a, Arbitrary a, Show b)
  => (String, a -> b)
  -- ^ First operation
  -> (String, b -> a)
  -- ^ This operation is inverse of the first
  -> TestTree
inverse (n1, f1) (n2, f2) = testProperty
  (n2 ++ " is inverse of " ++ n1) $
  \a -> let i = f1 a in
        counterexample (show i) $ f2 i === a

numProperties
  :: (Eq a, Arbitrary a, Show a, Num a)
  => a
  -- ^ Zero
  -> a
  -- ^ One
  -> TestTree
numProperties v0 v1 = testGroup "Num properties"
  [ testGroup "addition"
    [ associativeWit v0 (+)
    , commutativeWit v0 (+)
    , leftIdentity v0 (+)
    , rightIdentity v0 (+)
    ]

  , testGroup "multiplication"
    [ associativeWit v0 (*)
    , commutativeWit v0 (*)
    , leftIdentity v1 (*)
    , rightIdentity v1 (*)
    ]

  , testProperty "abs x * signum x == x" $
    \a -> abs (a `asTypeOf` v0) * signum a === a

  ]
