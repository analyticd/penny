{-# LANGUAGE RankNTypes #-}
module Penny.Numbers.Abstract.Unpolar.Generators where

import Test.QuickCheck
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Natural.Generators
import Deka.Native.Abstract.Generators
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.RadGroup.Generators
import Control.Monad
import Data.Sequence.Generators
import Prelude hiding (seq, exponent)
import qualified Prelude.Generators as PG

novDecs :: Gen NovDecs
novDecs = liftM2 NovDecs novem (seq decem)

exponent :: Gen Exponent
exponent = frequency
  [ (1, return ExpZero), (3, fmap ExpNegative novDecs) ]

coefficient :: Gen Coefficient
coefficient = frequency
  [ (1, return CoeZero), (3, fmap CoeNonZero novDecs) ]

zeroesNovDecs :: Gen ZeroesNovDecs
zeroesNovDecs = liftM2 ZeroesNovDecs nonNeg novDecs

decDecs :: Gen DecDecs
decDecs = liftM2 DecDecs decem (seq decem)

hasZeroDigit :: Gen HasZeroDigit
hasZeroDigit = fmap HasZeroDigit arbitrary

zeroDigit :: Gen ZeroDigit
zeroDigit = return ZeroDigit

zeroes :: Gen Zeroes
zeroes = fmap Zeroes pos

uNWhole :: Gen UNWhole
uNWhole = fmap UNWhole novDecs

uNWholeRadix :: Gen (Radix r) -> Gen (UNWholeRadix r)
uNWholeRadix r = liftM3 UNWholeRadix novDecs r (PG.maybe decDecs)

uNRadFrac :: Gen (Radix r) -> Gen (UNRadFrac r)
uNRadFrac r = liftM3 UNRadFrac hasZeroDigit r zeroesNovDecs

uZZeroOnly :: Gen UZZeroOnly
uZZeroOnly = return UZZeroOnly

uZTrailing :: Gen (Radix r) -> Gen (UZTrailing r)
uZTrailing r = liftM3 UZTrailing hasZeroDigit r (PG.maybe zeroes)

gZ
  :: Gen (Radix r)
  -> (forall a. Gen (a -> Group r a))
  -> Gen (GZ r)
gZ r g = liftM5 GZ hasZeroDigit r zeroes (group zeroes g)
  (seq (group zeroes g))

masunoGroupedLeft
  :: (forall a. Gen (a -> Group r a))
  -> Gen (MasunoGroupedLeft r)
masunoGroupedLeft g =
  liftM3 MasunoGroupedLeft novDecs (group decDecs g)
    (seq (group decDecs g))

masunoGroupedLeftRad
  :: Gen (Radix r)
  -> (forall a. Gen (a -> Group r a))
  -> Gen (MasunoGroupedLeftRad r)
masunoGroupedLeftRad r g = liftM3 MasunoGroupedLeftRad
  (masunoGroupedLeft g) r
  (PG.maybe (liftM2 (,) decDecs (seq (group decDecs g))))

masunoGroupedRight
  :: Gen (Radix r)
  -> (forall a. Gen (a -> Group r a))
  -> Gen (MasunoGroupedRight r)
masunoGroupedRight r g = liftM5 MasunoGroupedRight
  novDecs r decDecs (group decDecs g) (seq (group decDecs g))

fracunoFirstGroupZ
  :: Gen (Radix r)
  -> (forall a. Gen (a -> Group r a))
  -> Gen (FracunoFirstGroupZ r)
fracunoFirstGroupZ r g
  = FracunoFirstGroupZ
  `liftM` hasZeroDigit
  `ap` r
  `ap` zeroes
  `ap` seq (group zeroes g)
  `ap` group zeroesNovDecs g
  `ap` seq (group decDecs g)

fracunoFirstGroupNZ
  :: Gen (Radix r)
  -> (forall a. Gen (a -> Group r a))
  -> Gen (FracunoFirstGroupNZ r)
fracunoFirstGroupNZ r g = liftM5 FracunoFirstGroupNZ
  hasZeroDigit r zeroesNovDecs (group decDecs g)
  (seq (group decDecs g))
