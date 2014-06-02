module Penny.Lincoln.Decimal.Represent.Grouped where

import Penny.Lincoln.Decimal.Native
import Penny.Lincoln.Decimal.Rep
import Deka.Native hiding (Exponent, unExponent)
import Deka.Native.Abstract hiding (Exponent, unExponent)
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Lane
import Prelude hiding (exponent)
import Data.Maybe
import qualified Penny.Lincoln.Decimal.Represent.Ungrouped as U
import Data.List.Split (chunksOf)

-- | Represents a number, with digit grouping.  Rules for digit
-- grouping:
--
-- * Digits to the left of the radix are grouped only if there are
-- at least five digits.
--
-- * Digits to the right of the radix are never grouped.
--
-- The unit type is used as the groupng character; use 'fmap' to
-- insert your preferred grouping character.

grouped
  :: (HasExponent a, Laned a)
  => a
  -> Rep ()
grouped a = case lane a of
  Center -> RZero $ U.ungroupedZero (exponent a)
  NonCenter (s, d) -> RQuant $ groupedNonZero (exponent a) s d

-- | Groups digits for non-zero numbers.  If all digits will appear
-- to the right of the radix point, then no grouping will occur and
-- therefore 'U.punctaRungrouped' is applied.
groupedNonZero
  :: Exponent
  -> Side
  -> Decuple
  -> Quant ()
groupedNonZero e s d = Quant nz s
  where
    nz | dcplLen > iExp = NZLeft $ punctaLgrouped iExp d
       | otherwise = NZRight $ U.punctaRungrouped iExp d
    dcplLen = width d
    iExp = fromMaybe 0 . fmap decupleToInt . unExponent $ e

-- | Use this function only where the length of the 'Decuple' is
-- greater than the size of the exponent; that is, when at least one
-- significant digit will appear to the left of the radix point.
punctaLgrouped
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> PunctaL ()
punctaLgrouped e d = PunctaL cltch mayFl
  where
    cltch = Clatch (ChainsL []) lot chnsR
    lot = Lot [] nv decems1
    (nv, decems1, chnsR, mayFl) = punctaLgroupedNovemDecems e d

-- | Used by 'punctaLgrouped' to compute various groups of digits.
punctaLgroupedNovemDecems
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> (Novem, [Decem], ChainsR (), Maybe (Flock ()))
  -- ^ MSD, Decem in first group, remaining groups, right of radix
punctaLgroupedNovemDecems expnt dcple =
  let Decuple nv decemsAll = dcple
      nOnLeft = width dcple - expnt
      (decemOnLeft, decemOnRight) = splitAt (nOnLeft - 1) decemsAll
      mayFl = case decemOnRight of
        [] -> Nothing
        x:xs -> Just (Flock vl (ChainsR []))
          where
            vl = Voll x xs
      (decemGroup1, grps)
        | length decemOnLeft < 4 = (decemOnLeft, ChainsR [])
        | otherwise =
            let (g1, gr) = case groupsOf3 decemOnLeft of
                  [] -> error "punctaLgroupedNovemDecems: error 1"
                  x:xs -> (x, xs)
                mkChain ls = case ls of
                  [] -> error "punctaLgroupedNovemDecems: error 2"
                  x:xs -> ChainR () (Voll x xs)
                chains = map mkChain gr
            in (g1, ChainsR chains)
  in (nv, decemGroup1, grps, mayFl)


-- | Splits a list into groups of 3.  If it doesn't divide evenly,
-- parts at the front will be shorter.

groupsOf3 :: [a] -> [[a]]
groupsOf3 = map reverse . reverse . chunksOf 3 . reverse

