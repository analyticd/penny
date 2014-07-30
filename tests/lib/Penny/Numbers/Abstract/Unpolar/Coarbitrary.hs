module Penny.Numbers.Abstract.Unpolar.Coarbitrary where

import Penny.Numbers.Natural.Coarbitrary
import Penny.Numbers.Abstract.RadGroup.Coarbitrary
import Deka.Native.Abstract.Coarbitrary
import Data.Sequence.Coarbitrary
import Prelude hiding (seq, exponent, maybe)
import Prelude.Coarbitrary
import Penny.Numbers.Abstract.Unpolar
import Test.QuickCheck
import Barecheck.Util

novDecs :: NovDecs -> Gen b -> Gen b
novDecs (NovDecs nv ds) = novem nv . seq decem ds

exponent :: Exponent -> Gen b -> Gen b
exponent e = case e of
  ExpZero -> varInt 0
  ExpNegative nd -> varInt 1 . novDecs nd

coefficient :: Coefficient -> Gen b -> Gen b
coefficient c = case c of
  CoeZero -> varInt 0
  CoeNonZero nd -> varInt 1 . novDecs nd

zeroesNovDecs :: ZeroesNovDecs -> Gen b -> Gen b
zeroesNovDecs (ZeroesNovDecs nn nd)
  = nonNeg nn . novDecs nd

decDecs :: DecDecs -> Gen b -> Gen b
decDecs (DecDecs d ds) = decem d . seq decem ds

hasZeroDigit :: HasZeroDigit -> Gen b -> Gen b
hasZeroDigit (HasZeroDigit d) = coarbitrary d

zeroDigit :: ZeroDigit -> Gen b -> Gen b
zeroDigit _ = varInt 0

zeroes :: Zeroes -> Gen b -> Gen b
zeroes (Zeroes z) = pos z

uNWhole :: UNWhole -> Gen b -> Gen b
uNWhole (UNWhole nd) = novDecs nd

uNWholeRadix :: UNWholeRadix r -> Gen b -> Gen b
uNWholeRadix (UNWholeRadix nd rdx mdd)
  = novDecs nd . radix rdx . maybe decDecs mdd

uNRadFrac :: UNRadFrac r -> Gen b -> Gen b
uNRadFrac (UNRadFrac nzd rdx znd)
  = hasZeroDigit nzd . radix rdx . zeroesNovDecs znd

uZZeroOnly :: UZZeroOnly -> Gen b -> Gen b
uZZeroOnly _ = varInt 0

uZTrailing :: UZTrailing r -> Gen b -> Gen b
uZTrailing (UZTrailing nzd rdx mz)
  = hasZeroDigit nzd . radix rdx . maybe zeroes mz

gZ :: GZ r -> Gen b -> Gen b
gZ (GZ hzd rdx z gz sz)
  = hasZeroDigit hzd . radix rdx . zeroes z
  . group zeroes gz
  . seq (group zeroes) sz

masunoGroupedLeft :: MasunoGroupedLeft r -> Gen b -> Gen b
masunoGroupedLeft (MasunoGroupedLeft nv g1 gs)
  = novDecs nv . group decDecs g1 . seq (group decDecs) gs

masunoGroupedLeftRad :: MasunoGroupedLeftRad r -> Gen b -> Gen b
masunoGroupedLeftRad (MasunoGroupedLeftRad mgl rdx may)
  = masunoGroupedLeft mgl . radix rdx
  . maybe (tuple2 decDecs (seq (group decDecs))) may

masunoGroupedRight :: MasunoGroupedRight r -> Gen b -> Gen b
masunoGroupedRight (MasunoGroupedRight nd rdx dd g1 gs)
  = novDecs nd . radix rdx . decDecs dd
  . group decDecs g1 . seq (group decDecs) gs

fracunoFirstGroupZ :: FracunoFirstGroupZ r -> Gen b -> Gen b
fracunoFirstGroupZ (FracunoFirstGroupZ hzd rdx z s1 g1 s2)
  = hasZeroDigit hzd . radix rdx . zeroes z . seq (group zeroes) s1
  . group zeroesNovDecs g1 . seq (group decDecs) s2

fracunoFirstGroupNZ :: FracunoFirstGroupNZ r -> Gen b -> Gen b
fracunoFirstGroupNZ (FracunoFirstGroupNZ hzd rdx znd g1 s1)
  = hasZeroDigit hzd . radix rdx . zeroesNovDecs znd
  . group decDecs g1 . seq (group decDecs) s1
