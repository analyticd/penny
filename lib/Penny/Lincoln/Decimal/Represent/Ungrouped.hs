module Penny.Lincoln.Decimal.Represent.Ungrouped where

import Penny.Lincoln.Decimal.Native
import Penny.Lincoln.Decimal.Rep
import Deka.Native hiding (Exponent, unExponent)
import Deka.Native.Abstract hiding (Exponent, unExponent)
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Decimal.Lane
import Penny.Lincoln.Nats hiding (length)
import Prelude hiding (exponent)
import Data.Maybe

-- | Represents a number, without any digit grouping.

ungrouped
  :: (HasExponent a, Laned a)
  => a
  -> Rep b
ungrouped a = case lane a of
  Center -> RZero $ ungroupedZero (exponent a)
  NonCenter (s, d) -> RQuant $ ungroupedNonZero (exponent a) s d

ungroupedZero :: Exponent -> Zero a
ungroupedZero (Exponent e) = case e of
  Nothing -> ZBeak $ Beak (Coop (Eggs One) (Baskets []))
  Just dc -> case positive . decupleToInt $ dc of
    Nothing -> error "ungroupedZero: impossible exponent"
    Just p -> ZWingR $ WingR (Just (Coop (Eggs One) (Baskets [])))
      (Coop (Eggs p) (Baskets []))

ungroupedNonZero
  :: Exponent
  -> Side
  -> Decuple
  -> Quant a
ungroupedNonZero e s d = Quant nz s
  where
    nz | dcplLen > iExp = NZLeft $ punctaLungrouped iExp d
       | otherwise = NZRight $ punctaRungrouped iExp d
    dcplLen = width d
    iExp = fromMaybe 0 . fmap decupleToInt . unExponent $ e

-- | Use this function only where the length of the 'Decuple' is
-- greater than the size of the exponent; that is, when at least one
-- significant digit will appear to the left of the radix point.
punctaLungrouped
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> PunctaL a
punctaLungrouped e d = PunctaL cltch mayFl
  where
    cltch = Clatch (ChainsL []) lot (ChainsR [])
    lot = Lot [] nv decems
    ((nv, decems), mayFl) = punctaLungroupedNovemDecems e d

punctaLungroupedNovemDecems
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> ((Novem, [Decem]), Maybe (Flock a))
punctaLungroupedNovemDecems eInt dc =
  let nOnLeft = width dc - eInt
      Decuple nv decemAll = dc
      (decemOnLeft, decemOnRight) = splitAt (nOnLeft - 1) decemAll
      mayFl = case decemOnRight of
        [] -> Nothing
        x:xs -> Just (Flock vl (ChainsR []))
          where
            vl = Voll x xs
  in ((nv, decemOnLeft), mayFl)

-- | Use this function only where the length of the 'Decuple' is
-- less than or equal to the size of the exponent; that is, when all
-- significant digits will appear to the right of the radix point.
punctaRungrouped
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> PunctaR a
punctaRungrouped expnt dcple = PunctaR mayFl cltch
  where
    mayFl = Just (Flock (Voll D0 []) (ChainsR []))
    cltch = Clatch (ChainsL []) lot (ChainsR [])
    lot = Lot decemsL novem decemsR
    (decemsL, novem, decemsR) = punctaRungroupedNovemsDecems expnt dcple

-- | Used by 'punctaRungrouped' to compute digits.
punctaRungroupedNovemsDecems
  :: Int
  -- ^ Exponent value
  -> Decuple
  -> ([Decem], Novem, [Decem])
  -- ^ Zeroes between the radix and the MSD, the MSD, and digits
  -- after the MSD
punctaRungroupedNovemsDecems eInt dcple = (zeros, nvm, dcms)
  where
    Decuple nvm dcms = dcple
    nZeroes = eInt - width dcple
    zeros = replicate nZeroes D0
