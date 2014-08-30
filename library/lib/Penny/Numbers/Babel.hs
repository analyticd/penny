-- | Conversion between Concrete to Abstract types.

module Penny.Numbers.Babel where

import Data.Monoid
import Data.Sequence
import Deka.Native.Abstract (Novem, Decem)
import qualified Data.Sequence as S
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Signed
import Penny.Numbers.Abstract.Unsigned
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
  -> SignedUngrouped r p
paramsToUngrouped getP rdx (Params coe ex) = SignedUngrouped $ case coe of
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

-- | Convert an 'SignedUngrouped' to a 'Params'.  If @fSign@ is a
-- bijection, then the function resulting from @ungroupedToParams
-- fSign@ is also a bijection whose inverse is 'paramsToUngrouped'.

ungroupedToParams
  :: (p -> Sign)
  -> SignedUngrouped r p
  -> Params
ungroupedToParams getP (SignedUngrouped plrty) = case plrty of
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



