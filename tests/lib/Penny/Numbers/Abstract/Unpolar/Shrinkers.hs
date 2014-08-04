module Penny.Numbers.Abstract.Unpolar.Shrinkers where

import Test.QuickCheck
import Penny.Numbers.Abstract.Unpolar
import qualified Deka.Native.Abstract.Shrinkers as S
import qualified Data.Sequence.Shrinkers as S
import qualified Prelude.Shrinkers as S
import Prelude hiding (exponent)
import qualified Penny.Numbers.Natural.Shrinkers as S
import qualified Penny.Numbers.Abstract.RadGroup.Shrinkers as S

novDecs :: NovDecs -> [NovDecs]
novDecs (NovDecs n ds) =
  [ NovDecs n' ds' | (n', ds') <-
    S.tuple2 S.novem (S.seq S.decem) (n, ds) ]

exponent :: Exponent -> [Exponent]
exponent e = case e of
  ExpZero -> []
  ExpNegative n ->
    ExpZero
    : [ ExpNegative n' | n' <- novDecs n ]

coefficient :: Coefficient -> [Coefficient]
coefficient c = case c of
  CoeZero -> []
  CoeNonZero n ->
    CoeZero
    : map CoeNonZero (novDecs n)

zeroesNovDecs :: ZeroesNovDecs -> [ZeroesNovDecs]
zeroesNovDecs (ZeroesNovDecs nn nd) =
  [ ZeroesNovDecs nn' nd' | (nn', nd') <-
      S.tuple2 S.nonNeg novDecs (nn, nd) ]

decDecs :: DecDecs -> [DecDecs]
decDecs (DecDecs d ds) =
  [ DecDecs d' ds' | (d', ds') <-
      S.tuple2 S.decem (S.seq S.decem) (d, ds) ]

hasZeroDigit :: HasZeroDigit -> [HasZeroDigit]
hasZeroDigit (HasZeroDigit b) = map HasZeroDigit . shrink $ b

zeroDigit :: ZeroDigit -> [ZeroDigit]
zeroDigit _ = []

zeroes :: Zeroes -> [Zeroes]
zeroes = map Zeroes . S.pos . unZeroes

uNWhole :: UNWhole -> [UNWhole]
uNWhole = map UNWhole . novDecs . unUNWhole

uNWholeRadix :: UNWholeRadix r -> [UNWholeRadix r]
uNWholeRadix (UNWholeRadix nd rdx md) =
  [ UNWholeRadix nd' rdx' md' | (nd', rdx', md') <-
      S.tuple3 novDecs S.radix (S.maybe decDecs) (nd, rdx, md) ]

uNRadFrac :: UNRadFrac r -> [UNRadFrac r]
uNRadFrac (UNRadFrac hzd rdx znd) =
  [UNRadFrac hzd' rdx' znd' | (hzd', rdx', znd') <-
    S.tuple3 hasZeroDigit S.radix zeroesNovDecs (hzd, rdx, znd) ]

uZZeroOnly :: UZZeroOnly -> [UZZeroOnly]
uZZeroOnly _ = []

uZTrailing :: UZTrailing r -> [UZTrailing r]
uZTrailing (UZTrailing hzd rdx mz) =
  [ UZTrailing hzd' rdx' mz' | (hzd', rdx', mz') <-
      S.tuple3 hasZeroDigit S.radix (S.maybe zeroes) (hzd, rdx, mz) ]

gZ :: GZ r -> [GZ r]
gZ (GZ hzd rdx zz g1 gs) =
  [ GZ hzd' rdx' zz' g1' gs' | (hzd', rdx', zz', g1', gs') <-
      S.tuple5 hasZeroDigit S.radix zeroes (S.group zeroes)
               (S.seq (S.group zeroes)) (hzd, rdx, zz, g1, gs) ]

masunoGroupedLeft
  :: MasunoGroupedLeft r
  -> [MasunoGroupedLeft r]
masunoGroupedLeft (MasunoGroupedLeft nd g1 gs) =
  [ MasunoGroupedLeft nd' g1' gs' | (nd', g1', gs') <-
      S.tuple3 novDecs (S.group decDecs) (S.seq (S.group decDecs))
               (nd, g1, gs) ]

masunoGroupedLeftRad
  :: MasunoGroupedLeftRad r
  -> [MasunoGroupedLeftRad r]
masunoGroupedLeftRad (MasunoGroupedLeftRad mgl rdx may) =
  [ MasunoGroupedLeftRad mgl' rdx' may' | (mgl', rdx', may') <-
      S.tuple3 (masunoGroupedLeft) S.radix
               (S.maybe (S.tuple2 decDecs (S.seq (S.group decDecs))))
               (mgl, rdx, may) ]

masunoGroupedRight
  :: MasunoGroupedRight r
  -> [MasunoGroupedRight r]
masunoGroupedRight (MasunoGroupedRight nd rdx dd g1 gs) =
  [ MasunoGroupedRight nd' rdx' dd' g1' gs' | (nd', rdx', dd', g1', gs') <-
      S.tuple5 novDecs S.radix decDecs (S.group decDecs)
               (S.seq (S.group decDecs))
               (nd, rdx, dd, g1, gs) ]

fracunoFirstGroupZ
  :: FracunoFirstGroupZ r
  -> [FracunoFirstGroupZ r]
fracunoFirstGroupZ (FracunoFirstGroupZ hzd rdx zz sz g1 ss) =
  [ FracunoFirstGroupZ hzd' rdx' zz' sz' g1' ss'
    | (hzd', rdx', zz', sz', (g1', ss')) <-
    S.tuple5 hasZeroDigit S.radix zeroes (S.seq (S.group zeroes))
             (S.tuple2 (S.group zeroesNovDecs)
                       (S.seq (S.group decDecs)))
             (hzd, rdx, zz, sz, (g1, ss)) ]

fracunoFirstGroupNZ
  :: FracunoFirstGroupNZ r
  -> [FracunoFirstGroupNZ r]
fracunoFirstGroupNZ (FracunoFirstGroupNZ hzd rdx znd g1 gs) =
  [ FracunoFirstGroupNZ hzd' rdx' znd' g1' gs'
    | (hzd', rdx', znd', g1', gs') <-
      S.tuple5 hasZeroDigit S.radix zeroesNovDecs
               (S.group decDecs)
               (S.seq (S.group decDecs))
               (hzd, rdx, znd, g1, gs) ]
