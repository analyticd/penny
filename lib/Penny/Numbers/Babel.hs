-- | Conversion between Concrete to Abstract types.

module Penny.Numbers.Babel where

import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Abstract.Aggregates
import Penny.Numbers.Abstract.Unpolar
import Penny.Numbers.Concrete
import Data.Sums
import Deka.Dec (Sign)

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
      CoeNonZero dc -> OffCenter plr unz
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
ungroupedNonZero rdx dcple expnt = UngroupedNonZero $ case expnt of
  ExpZero -> S3a $ UNWhole dcple
  _ -> undefined

exponentToUngroupedZero
  :: Radix r
  -> Exponent
  -> UngroupedZero r
exponentToUngroupedZero rdx expnt = UngroupedZero $ case expnt of
  ExpZero -> S2a (UZBare ZeroDigit)
  ExpNegative nd ->
    S2b . UZTrailing ZeroDigit rdx . Just . Zeroes . novDecsToPos $ nd

toConcrete
  :: (p -> Sign)
  -> UngroupedPolar r p
  -> Concrete
toConcrete = undefined
