-- | Conversion between Concrete to Abstract types.

module Penny.Numbers.Babel where

import Data.Monoid
import Data.Sequence
import Deka.Native.Abstract (Novem, Decem)
import qualified Data.Sequence as S
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Concrete
import Penny.Numbers.Natural
import qualified Penny.Numbers.Natural as N
import Data.Sums
import Deka.Dec (Sign(..))

fromConcrete
  :: (Sign -> p)
  -- ^ How to obtain the polarity.
  -> Radix r
  -> Concrete
  -> UngroupedPolar r p
fromConcrete getP rdx conc = UngroupedPolar plrty
  where
    plrty = case coe of
      CoeZero -> Center uz
      CoeNonZero dc -> OffCenter unz plr
        where
          unz = ungroupedNonZero rdx dc ex
      where
        (Params sgn coe ex) = params conc
        plr = getP sgn
        uz = exponentToUngroupedZero rdx ex


ungroupedNonZero
  :: Radix r
  -> NovDecs
  -> Exponent
  -> UngroupedNonZero r
ungroupedNonZero rdx coe expt = UngroupedNonZero $ case expt of
  ExpZero -> S3a $ UNWhole coe
  ExpNegative nvdcs ->
    finishUngroupedNonZero nv rdx
    $ goUngroupedNonZero S.empty (novDecsToPos nvdcs) coeDcs
    where
      NovDecs nv coeDcs = coe


goUngroupedNonZero
  :: Seq Decem
  -> Pos
  -> Seq Decem
  -> Either (Pos, Seq Decem) (Seq Decem, Seq Decem)
goUngroupedNonZero dcsSoFar plcs co = case S.viewr co of
  EmptyR -> Left (plcs, dcsSoFar)
  rest :> dig -> case prevPos plcs of
    Nothing -> Right (dig <| dcsSoFar, rest)
    Just p -> goUngroupedNonZero (dig <| dcsSoFar) p rest


finishUngroupedNonZero
  :: Novem
  -> Radix r
  -> Either (Pos, Seq Decem) (Seq Decem, Seq Decem)
  -> S3 a (UNWholeRadix r) (UNRadFrac r)
finishUngroupedNonZero nv rdx ei = case ei of
  Left (plcs, dcsSoFar) ->
    S3c . UNRadFrac (HasZeroDigit True) rdx . ZeroesNovDecs z $
      NovDecs nv dcsSoFar
    where
      z = case prevPos plcs of
        Nothing -> zeroNonNeg
        Just p -> posToNonNeg p

  Right (dcsSoFar, rest) -> S3b $
    UNWholeRadix (NovDecs nv rest) rdx (Just dd)
    where
      dd = case S.viewl dcsSoFar of
        EmptyL -> error "ungroupedNonZero: error"
        d :< rst -> DecDecs d rst



exponentToUngroupedZero
  :: Radix r
  -> Exponent
  -> UngroupedZero r
exponentToUngroupedZero rdx expnt = UngroupedZero $ case expnt of
  ExpZero -> S2a UZZeroOnly
  ExpNegative nd ->
    S2b . UZTrailing (HasZeroDigit True) rdx
    . Just . Zeroes . novDecsToPos $ nd

toConcrete
  :: (p -> Sign)
  -> UngroupedPolar r p
  -> Concrete
toConcrete getSign (UngroupedPolar plrty) =
  concrete $ Params sgn coe expt
  where
    (sgn, coe, expt) = case plrty of
      Center uz -> (Sign0, c, e)
        where
          (c, e) = ungroupedZeroCoeExp uz
      OffCenter unz sd -> (getSign sd, c, e)
        where
          (c, e) = ungroupedNonZeroCoeExp unz

ungroupedZeroCoeExp
  :: UngroupedZero r
  -> (Coefficient, Exponent)
ungroupedZeroCoeExp (UngroupedZero sm) = case sm of
  S2a UZZeroOnly -> (CoeZero, ExpZero)
  S2b (UZTrailing _ _ mz) -> (CoeZero, expt)
    where
      expt = case mz of
        Nothing -> ExpZero
        Just (Zeroes p) -> ExpNegative . posToNovDecs $ p

ungroupedNonZeroCoeExp
  :: UngroupedNonZero r
  -> (Coefficient, Exponent)
ungroupedNonZeroCoeExp (UngroupedNonZero s3) = case s3 of

  S3a (UNWhole nd) -> (CoeNonZero nd, ExpZero)

  S3b (UNWholeRadix nd _ mayDD) -> (CoeNonZero nd', expt)
    where
      NovDecs nv dd = nd
      dr = maybe S.empty flattenDecDecs mayDD
      dd' = dd <> dr
      nd' = NovDecs nv dd'
      expt = case nonNegToPos $ N.length dr of
        Nothing -> ExpZero
        Just p -> ExpNegative . posToNovDecs $ p

  S3c (UNRadFrac _ _ (ZeroesNovDecs zz nd)) -> (CoeNonZero nd, expt)
      where
        NovDecs _ ds = nd
        lenNovDecs = case nonNegToPos $ N.length ds of
          Nothing -> onePos
          Just p -> nextPos p
        expt = ExpNegative . posToNovDecs $ case nonNegToPos zz of
          Nothing -> lenNovDecs
          Just x -> addPos x lenNovDecs
