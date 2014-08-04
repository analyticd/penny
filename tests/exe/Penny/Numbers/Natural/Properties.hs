module Penny.Numbers.Natural.Properties where

import Deka.Native.Abstract
import qualified Penny.Numbers.Natural.Generators as G
import qualified Penny.Numbers.Natural.Shrinkers as S
import Penny.Numbers.Natural
import Test.QuickCheck hiding (NonZero)
import Prelude hiding (length)

-- | a + b - b == a

prop_aPlusBMinusBEqualsA :: Property
prop_aPlusBMinusBEqualsA =
  forAllShrink G.pos S.pos $ \a ->
  forAllShrink G.pos S.pos $ \b ->
  case ((a `addPos` b) `subtPos` b) of
    Nothing -> property False
    Just Zero -> property False
    Just (NonZero r) -> r === a

-- | multiplication is repeated addition
prop_multPos :: Property
prop_multPos =
  forAllShrink G.pos S.pos $ \a ->
  forAllShrink G.pos S.pos $ \b ->
  let rMult = multPos a b
      rByAdd = go a b
      go acc n = case n of
        One -> acc
        Succ p -> let acc' = addPos acc a in go acc' p
  in rMult === rByAdd

-- | 2 to the third power is 8
prop_twoToTheThird :: Bool
prop_twoToTheThird =
  expPos (novemToPos D2) (novemToPos D3) == novemToPos D8

-- | Exponentiation is repeated multiplication
prop_expPos :: Property
prop_expPos =
  mapSize (min 4) $
  forAllShrink G.pos S.pos $ \a ->
  forAllShrink G.pos S.pos $ \b ->
  let rExp = expPos a b
      rByMult = go a b
      go acc n = case n of
        One -> acc
        Succ p -> go (multPos acc a) p
  in rExp === rByMult

-- | a + b - b == a

prop_nonNegAplusBminusBequalsA :: Property
prop_nonNegAplusBminusBequalsA =
  forAllShrink G.nonNeg S.nonNeg $ \a ->
  forAllShrink G.pos S.pos $ \b ->
  case ((a `addNonNeg` (NonZero b)) `subtPosFromNonNeg` b) of
    Nothing -> property False
    Just nn -> a === nn

-- | multiplication is repeated addition

prop_multNonNeg :: Property
prop_multNonNeg =
  forAllShrink G.nonNeg S.nonNeg $ \a ->
  forAllShrink G.nonNeg S.nonNeg $ \b ->
  let rMult = multNonNeg a b
      rByAdd = go Zero b
      go acc i = case i of
        Zero -> acc
        NonZero p -> case p of
          One -> addNonNeg acc a
          Succ ps -> go (addNonNeg acc a) (NonZero ps)
  in rByAdd === rMult

-- | multiply quotient by numerator, and add remainder, yields
-- denominator
prop_divNonNegByPos :: Property
prop_divNonNegByPos =
  forAllShrink G.nonNeg S.nonNeg $ \a ->
  forAllShrink G.pos S.pos $ \b ->
  let (q, r) = divNonNegByPos a b
      tgt = (q `multNonNeg` (NonZero b)) `addNonNeg` r
  in tgt === a

-- | exponentiation is repeated multiplication
prop_expNonNeg :: Property
prop_expNonNeg =
  mapSize (min 4) $
  forAllShrink G.nonNeg S.nonNeg $ \a ->
  forAllShrink G.nonNeg S.nonNeg $ \b ->
  let rExp = expNonNeg a b
      rByMult = case b of
        Zero -> NonZero One
        NonZero p -> go a p
      go acc ps = case ps of
        One -> acc
        Succ p -> go (multNonNeg acc a) p
  in rExp === rByMult

prop_lengthOf4 :: Bool
prop_lengthOf4 = length [1 :: Int,1,1,1] == decemToNonNeg (Nonem D4)

prop_lengthOf0 :: Bool
prop_lengthOf0 = length [] == decemToNonNeg D0
