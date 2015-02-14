{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Lincoln.Properties where

import Penny.Lincoln
import Penny.Lincoln.Instances ()
import Test.Tasty.QuickCheck hiding (Positive)
import qualified Data.Sequence as S
import Test.Tasty
import qualified Data.Foldable as F
import Proptree

-- These tests are organized so that modules at the bottom of the
-- import graph are tested first.

tests = testGroup "Penny.Lincoln.Properties"
  [ testGroupNatural
  , testGroupRep
  , testGroupDecimal
  ]

-- Natural

-- | Builds a TestTree to test all the properties of a member of the
-- Natural class.

naturalTestTree
  :: (Arbitrary a, Show a, Natural a, Eq a)
  => a
  -- ^ The value is ignored.  It is used only to determine the type
  -- that is being tested.
  -> TestTree
naturalTestTree whatType = testGroup "Natural class properties"
  [ testProperty "prev is inverse of next" $
    \a -> case prev (next (a `asTypeOf` whatType)) of
      Nothing -> property False
      Just p -> p === a

  , testProperty "naturalToInteger is inverse of integerToNatural" $
    \a -> case integerToNatural a of
      Nothing -> property Discard
      Just n -> naturalToInteger (n `asTypeOf` whatType) === a

  , testProperty "addition is associative" $
    \a b c -> a `add` (b `add` (c `asTypeOf` whatType))
      === (a `add` b) `add` c

  , testProperty "addition is commutative" $
    \a b -> a `add` (b `asTypeOf` whatType) === b `add` a

  , testProperty "multiplication is associative" $
    \a b c -> a `mult` (b `mult` (c `asTypeOf` whatType))
      === (a `mult` b) `mult` c

  , testProperty "multiplication is commutative" $
    \a b -> a `mult` (b `asTypeOf` whatType) === b `mult` a

  ]

testGroupNatural :: TestTree
testGroupNatural = testGroup "Penny.Lincoln.Natural"
  [ naturalTestTree (undefined :: Unsigned)
  , naturalTestTree (undefined :: Positive)

  , testProperty "positiveDigits is inverse of novDecsToPositive" $
    \d1 ds ->
    let pos = novDecsToPositive d1 (S.fromList ds)
        (d1', ds') = positiveDigits pos
    in (d1', ds') === (d1, ds)

  ]

-- End Natural

-- Rep

testGroupRep :: TestTree
testGroupRep = testGroup "Penny.Lincoln.Rep"
  [ testProperty "ungroupBrimGrouped is inverse of groupBrimUngrouped" $
    \bu -> case groupBrimUngrouped () bu of
      Nothing -> property Discard
      Just bg -> ungroupBrimGrouped bg === bu

  , testProperty "ungroupNilGroup does not change number of digits" $ \ng ->
    let ngNumDigits (NilGrouped mayZ _ _ s1 _ _ s2 s3) =
          maybe 0 (const 1) mayZ + 2 + S.length s1 + S.length s2 + F.sum
          (fmap (\(_, _, sq) -> S.length sq + 1) s3)
        nuNumDigits nu = case nu of
          NUZero _ Nothing -> 1
          NUZero _ (Just (_, Nothing)) -> 1
          NUZero _ (Just (_, Just (_, zs))) -> 2 + S.length zs
          NURadix _ _ sq -> 1 + S.length sq
        _types = ng :: NilGrouped ()
        ungrouped = ungroupNilGrouped ng
    in counterexample ("ungrouped: " ++ show ungrouped)
       $ ngNumDigits ng === nuNumDigits ungrouped
  ]

-- End Rep

-- Decimal

data EqualizeExponents = EqualizeExponents
  { ee'x :: Decimal
  , ee'y :: Decimal
  , ee'mx :: Integer
  , ee'ex :: Unsigned
  , ee'my :: Integer
  , ee'ey :: Unsigned
  , ee'x' :: Decimal
  , ee'y' :: Decimal
  , ee'mx' :: Integer
  , ee'ex' :: Unsigned
  , ee'my' :: Integer
  , ee'ey' :: Unsigned
  } deriving (Eq, Ord, Show)

instance Arbitrary EqualizeExponents where
  arbitrary = do
    x@(Decimal mx ex) <- arbitrary
    y@(Decimal my ey) <- arbitrary
    let (x'@(Decimal mx' ex'), y'@(Decimal my' ey')) = equalizeExponents x y
    return $ EqualizeExponents x y mx ex my ey x' y' mx' ex' my' ey'

testGroupDecimal :: TestTree
testGroupDecimal = testGroup "Penny.Lincoln.Decimal"
  [ testGroup "Decimal properties"
    [ numProperties (fromInteger 0 :: Decimal) (fromInteger 1)
    ]

  , testProperty "increaseExponent result" $
    \u d@(Decimal _ e) ->
      let Decimal _ e' = increaseExponent u d
      in e >= u || e' == u

  , testGroup "equalizeExponents"
    [ testProperty "exponents" $
      \x -> ee'ex' x == ee'ey' x

    , testProperty "one original stays same" $
      \x -> ee'x' x == ee'x x || ee'y' x == ee'y x

    , testProperty "ex'" $
      \x -> ee'ex' x >= ee'ex x

    , testProperty "ey'" $
      \x -> ee'ey' x >= ee'ey x

    , testProperty "mx'" $
      \x -> abs (ee'mx' x) >= abs (ee'mx x)

    , testProperty "my'" $
      \x -> abs (ee'my' x) >= abs (ee'my x)

    ]

  , testGroup "representing decimals"

    [ testProperty "inversion of repUngroupedDecimal" $
      \dec ->
        let repped = repUngroupedDecimal (Radix :: Radix ()) dec
            ei = stripDecimalSign dec
            pair = (repped, ei)
        in counterexample (show pair) $ case pair of
            (Center nu, Left z) -> toDecZero nu === z
            (OffCenter bu pm, Right (pos, pm')) ->
              toDecPositive bu === pos .&&. pm === pm'
            _ -> property False

    , testProperty "inversion of repUngroupedDecNonZero" $
      \dec -> let a@(bu, pm) = repUngroupedDecNonZero (Radix :: Radix ()) dec
      in counterexample (show a)
         $ c'DecNonZero'DecPositive pm (toDecPositive bu) === dec

    , testProperty "inversion of repUngroupedDecUnsigned" $
      \dec -> let r = repUngroupedDecUnsigned (Radix :: Radix ()) dec
      in case (r, decomposeDecUnsigned dec) of
          (Center nu, Left z) -> toDecZero nu === z
          (OffCenter bu _, Right p) ->
            toDecPositive bu === p
          _ -> property False

    , testProperty "inversion of repUngroupedDecZero" $ \dec ->
      let nu = repUngroupedDecZero (Radix :: Radix ()) dec
      in toDecZero nu === dec

    , testProperty "inversion of repUngroupedDecPositive" $ \dec ->
      let bg = repUngroupedDecPositive (Radix :: Radix ()) dec
      in toDecPositive bg === dec

    ]

  ]

-- End Decimal

prop_repUngroupedDecimal dec
  = let rep = repUngroupedDecimal (Radix :: Radix ()) dec
        res = toDecimal rep
  in counterexample ("rep: " ++ show rep ++ " res: " ++ show res)
     $ res == dec

prop_repUngroupedDecNonZero dec
  = pos === dec
  where
    pos = c'DecNonZero'DecPositive pm . toDecPositive $ bu
    (bu, pm) = repUngroupedDecNonZero (Radix :: Radix ()) dec

prop_repUngroupedDecUnsigned dec
  = case (decomposeDecUnsigned dec, rep) of
    (Left z, Center nu) -> toDecZero nu === z
    (Right p, OffCenter bu ()) -> toDecPositive bu === p
    _ -> property False
  where
    rep = repUngroupedDecUnsigned (Radix :: Radix ()) dec

seconds :: Testable prop => Int -> prop -> Property
seconds i = within (i * 10 ^ (6 :: Int))

prop_repUngroupedDecZeroSucceeds dz
  = length (show (repUngroupedDecZero (Radix :: Radix ()) dz)) /= 0

prop_repUngroupedDecZero dec
  = seconds 10
  $ toDecZero (repUngroupedDecZero (Radix :: Radix ()) dec)
  === dec

-- Grouping

prop_groupBrimUngrouped'ungroup bu
  = case groupBrimUngrouped () bu of
      Nothing -> property Discard
      Just a -> property $ ungroupBrimGrouped a == bu

