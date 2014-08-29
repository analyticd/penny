-- | Conversion between Concrete to Abstract types.

module Penny.Numbers.Babel where

import Data.Monoid
import Data.Sequence
import Deka.Native.Abstract (Novem, Decem)
import qualified Data.Sequence as S
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Polar
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Concrete
import Penny.Numbers.Natural
import qualified Penny.Numbers.Natural as N
import Deka.Dec (Sign(..))

-- | Convert a 'Params' to an 'Ungrouped'.  If @fSign@ is a bijection,
-- then the function resulting from @paramsToUngrouped fSign rdx@ is
-- also a bijection whose inverse is 'ungroupedToParams'.
paramsToUngrouped
  :: (Sign -> p)
  -> Radix r
  -> Params
  -> Ungrouped r p
paramsToUngrouped getP rdx (Params coe ex) = Ungrouped $ case coe of
  CoeZero -> Center $ nilUngrouped rdx ex
  CoeNonZero nd s -> OffCenter (brimUngrouped rdx nd ex) (getP s)

nilUngrouped
  :: Radix r
  -> Exponent
  -> NilUngrouped r
nilUngrouped rdx ex = NULeadingZero Zero nu1
  where
    nu1 = case ex of
      ExpZero -> NU1End
      ExpNegative nd -> NU1Radix rdx . NU2Zeroes
        . Zeroes . novDecsToPos $ nd

brimUngrouped
  :: Radix r
  -> NE Novem Decem
  -> Exponent
  -> BrimUngrouped r
brimUngrouped rdx ne expnt = go S.empty ne (toMaybe expnt)
  where
    toMaybe e = case e of
      ExpZero -> Nothing
      ExpNegative nd -> Just . novDecsToPos $ nd

    go onRight (NE nv ds) ex = case ex of
      Nothing -> BUMasuno (NE nv ds) bu1
        where
          bu1 | S.null onRight = BU1End
              | otherwise = BU1Radix rdx onRight

      Just posi -> case S.viewr ds of
        ds' :> d1 -> go (d1 <| onRight) (NE nv ds') (prevPos posi)
        EmptyR -> BUFracuno . BU2LeadingZero Zero rdx $
          case prevPos posi of
            Nothing -> BU3NoZeroes (NE nv onRight)
            Just ps -> BU3Zeroes (Zeroes ps) (NE nv onRight)

-- | Convert an 'Ungrouped' to a 'Params'.  If @fSign@ is a bijection,
-- then the function resulting from @ungroupedToParams fSign@ is also
-- a bijection whose inverse is 'paramsToUngrouped'.

ungroupedToParams
  :: (p -> Sign)
  -> Ungrouped r p
  -> Params
ungroupedToParams getP (Ungrouped plrty) = case plrty of
  Center nu -> case nu of
    NULeadingZero _ nu1 -> Params CoeZero ex
      where
        ex = case nu1 of
          NU1End -> ExpZero
          NU1Radix _ nu2 -> case nu2 of
            NU2End -> ExpZero
            NU2Zeroes (Zeroes ps) -> ExpNegative (posToNovDecs ps)
    NUNoLeadingZero _ zs -> Params CoeZero . ExpNegative
      . posToNovDecs . unZeroes $ zs

  OffCenter bu p -> Params (CoeNonZero nd (getP p)) ex
    where
      (nd, ex) = case bu of
        BUMasuno (NE nv ds) bu1 -> case bu1 of
          BU1End -> (NE nv ds, ExpZero)
          BU1Radix _ after -> (NE nv (ds <> after), expnt)
            where
              expnt = case nonNegToPos . N.length $ after of
                Nothing -> ExpZero
                Just ps -> ExpNegative . posToNovDecs $ ps
        BUFracuno bu2 -> procBU3 $ case bu2 of
          BU2LeadingZero _ _ x -> x
          BU2NoLeadingZero _ x -> x
          where
            procBU3 bu3 = case bu3 of
              BU3Zeroes (Zeroes zs) novdecs -> (novdecs, expt)
                where
                  expt = ExpNegative . posToNovDecs
                    $ addPos zs (lengthNE nd)
              BU3NoZeroes novdecs -> (novdecs, expt)
                where
                  expt = ExpNegative . posToNovDecs . lengthNE $ nd



{-
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
-}
