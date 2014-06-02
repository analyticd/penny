module Penny.Lincoln.Decimal.Represent where

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
    nz | dcplLen > iExp = NZLeft $ punctaLungrouped e d
       | otherwise = NZRight $ punctaRungrouped e d
    dcplLen = width d
    iExp = fromMaybe 0 . fmap decupleToInt . unExponent $ e

punctaLungrouped :: Exponent -> Decuple -> PunctaL a
punctaLungrouped e d = PunctaL cltch mayFl
  where
    cltch = Clatch (ChainsL []) lot (ChainsR [])
    lot = Lot [] nv decems
    ((nv, decems), mayFl) = punctaLungroupedNovemDecems e d

punctaLungroupedNovemDecems
  :: Exponent
  -> Decuple
  -> ((Novem, [Decem]), Maybe (Flock a))
punctaLungroupedNovemDecems expnt dc =
  let eInt = fromMaybe 0 . fmap decupleToInt . unExponent $ expnt
      nOnLeft = width dc - eInt
      Decuple nv decemAll = dc
      (decemOnLeft, decemOnRight) = splitAt (nOnLeft - 1) decemAll
      mayFl = case decemOnRight of
        [] -> Nothing
        x:xs -> Just (Flock vl (ChainsR []))
          where
            vl = Voll x xs
  in ((nv, decemOnLeft), mayFl)

punctaRungrouped :: Exponent -> Decuple -> PunctaR a
punctaRungrouped expnt dcple = PunctaR mayFl cltch
  where
    mayFl = Just (Flock (Voll D0 []) (ChainsR []))
    cltch = Clatch (ChainsL []) lot (ChainsR [])
    lot = Lot decemsL novem decemsR
    (decemsL, novem, decemsR) = punctaRungroupedNovemsDecems expnt dcple

punctaRungroupedNovemsDecems
  :: Exponent
  -> Decuple
  -> ([Decem], Novem, [Decem])
punctaRungroupedNovemsDecems expnt dcple = (zeros, nvm, dcms)
  where
    Decuple nvm dcms = dcple
    eInt = fromMaybe 0 . fmap decupleToInt . unExponent $ expnt
    nZeroes = eInt - width dcple
    zeros = replicate nZeroes D0



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
  Center -> RZero $ ungroupedZero (exponent a)
  NonCenter (s, d) -> RQuant $ groupedNonZero (exponent a) s d

groupedNonZero
  :: Exponent
  -> Side
  -> Decuple
  -> Quant ()
groupedNonZero = undefined
