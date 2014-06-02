module Penny.Lincoln.Decimal.Native where

import qualified Penny.Lincoln.Decimal.Rep as A
import qualified Deka.Native.Abstract as D
import Prelude hiding (exponent)
import Control.Monad (join)
import Penny.Lincoln.Nats (countPositive)
import Data.Maybe (fromMaybe)

lotToDecuple :: A.Lot -> D.Decuple
lotToDecuple (A.Lot l n r) = case D.decemListToDecuple l of
  Nothing -> D.Decuple n r
  Just (D.Decuple nv ds) ->
    D.Decuple nv (ds ++ D.Nonem n : r)

addDecuples :: D.Decuple -> D.Decuple -> D.Decuple
addDecuples (D.Decuple n1 d1) (D.Decuple n2 d2) =
  D.Decuple n1 (d1 ++ D.Nonem n2 : d2)

addMaybeLDecuple :: Maybe D.Decuple -> D.Decuple -> D.Decuple
addMaybeLDecuple mayDc dc = case mayDc of
  Nothing -> dc
  Just d -> addDecuples d dc

addMaybeRDecuple :: D.Decuple -> Maybe D.Decuple -> D.Decuple
addMaybeRDecuple dc mayDc = case mayDc of
  Nothing -> dc
  Just d -> addDecuples dc d

addMaybeDecuples
  :: Maybe D.Decuple -> Maybe D.Decuple -> Maybe D.Decuple
addMaybeDecuples mayl mayr = case (mayl, mayr) of
  (Nothing, Nothing) -> Nothing
  (Just l, Nothing) -> Just l
  (Nothing, Just r) -> Just r
  (Just l, Just r) -> Just $ addDecuples l r

vollToDecuple :: A.Voll -> Maybe D.Decuple
vollToDecuple (A.Voll d1 ds) = case D.decemToNovem d1 of
  Nothing -> D.decemListToDecuple ds
  Just nv -> Just $ D.Decuple nv ds

chainsLtoDecuple :: A.ChainsL a -> Maybe D.Decuple
chainsLtoDecuple = foldl f Nothing . A.unChainsL
  where
    f acc (A.ChainL v _) = case vollToDecuple v of
      Nothing -> acc
      Just d -> case acc of
        Nothing -> Just d
        Just lft -> Just $ addDecuples lft d

chainsRtoDecuple :: A.ChainsR a -> Maybe D.Decuple
chainsRtoDecuple = foldl f Nothing . A.unChainsR
  where
    f acc (A.ChainR _ v) = case vollToDecuple v of
      Nothing -> acc
      Just d -> case acc of
        Nothing -> Just d
        Just lft -> Just $ addDecuples lft d

clatchToDecuple :: A.Clatch a -> D.Decuple
clatchToDecuple (A.Clatch lft ctr rgt) =
  (l `addMaybeLDecuple` c) `addMaybeRDecuple` r
  where
    l = chainsLtoDecuple lft
    c = lotToDecuple ctr
    r = chainsRtoDecuple rgt

flockToDecuple :: A.Flock a -> Maybe D.Decuple
flockToDecuple (A.Flock v cs) =
  addMaybeDecuples (vollToDecuple v) (chainsRtoDecuple cs)

punctaLtoDecuple :: A.PunctaL a -> D.Decuple
punctaLtoDecuple (A.PunctaL cl mayFl) = case mayFl of
  Nothing -> lft
  Just fl -> case flockToDecuple fl of
    Nothing -> lft
    Just rt -> addDecuples lft rt
  where
    lft = clatchToDecuple cl

punctaRtoDecuple :: A.PunctaR a -> D.Decuple
punctaRtoDecuple (A.PunctaR mayFl cl) = case mayFl of
  Nothing -> rt
  Just fl -> case flockToDecuple fl of
    Nothing -> rt
    Just lft -> addDecuples lft rt
  where
    rt = clatchToDecuple cl

-- | An exponent.  Penny only recognizes negative or zero exponents.

newtype Exponent = Exponent { unExponent :: Maybe D.Decuple }
  deriving (Eq, Ord, Show)

class HasExponent a where
  exponent :: a -> Exponent

instance HasExponent (A.Whole a) where
  exponent _ = Exponent Nothing

class HasWidth a where
  width :: a -> Int

instance HasWidth D.Decuple where
  width (D.Decuple _ ds) = length ds + 1

instance HasWidth A.Voll where
  width (A.Voll _ ds) = length ds + 1

instance HasWidth (A.ChainL a) where
  width (A.ChainL v _) = width v

instance HasWidth (A.ChainsL a) where
  width = sum . map width . A.unChainsL

instance HasWidth (A.ChainR a) where
  width (A.ChainR _ v) = width v

instance HasWidth (A.ChainsR a) where
  width = sum . map width . A.unChainsR

instance HasWidth A.Lot where
  width (A.Lot l _ r) = length l + 1 + length r

instance HasWidth (A.Clatch a) where
  width (A.Clatch l c r) = width l + width c + width r

instance HasWidth (A.Whole a) where
  width = width . A.unWhole

instance HasWidth (A.Flock a) where
  width (A.Flock f r) = width f + width r

instance HasWidth (A.PunctaL a) where
  width (A.PunctaL c f) = width c + maybe 0 width f

instance HasWidth (A.PunctaR a) where
  width (A.PunctaR f c) = width c + maybe 0 width f

instance HasWidth (A.NonZero a) where
  width a = case a of
    A.WholeOnly w -> width w
    A.NZLeft w -> width w
    A.NZRight w -> width w

instance HasWidth A.Eggs where
  width = countPositive 1 succ . A.unEggs

instance HasWidth (A.Basket a) where
  width = width . A.bkEggs

instance HasWidth (A.Baskets a) where
  width = sum . map width . A.unBaskets

instance HasWidth (A.Coop a) where
  width a = width (A.cpEggs a) + width (A.cpBaskets a)

instance HasWidth (A.Beak a) where
  width = width . A.unBeak

instance HasWidth (A.WingL a) where
  width a = width (A.wlLeft a) +
    (fromMaybe 0 . fmap width . A.wlRight $ a)

instance HasWidth (A.WingR a) where
  width a = width (A.wrRight a) +
    (fromMaybe 0 . fmap width . A.wrLeft $ a)

widthToExp :: Int -> Exponent
widthToExp = Exponent . fmap snd . D.intToDecuple

instance HasExponent (A.PunctaL a) where
  exponent = widthToExp . maybe 0 width . A.plRight

instance HasExponent (A.PunctaR a) where
  exponent = widthToExp . width . A.prRight

class HasDecuple a where
  decuple :: a -> D.Decuple

instance HasDecuple (A.Clatch a) where
  decuple = clatchToDecuple

instance HasDecuple (A.Whole a) where
  decuple = clatchToDecuple . A.unWhole

instance HasDecuple (A.PunctaL a) where
  decuple (A.PunctaL c f) = addMaybeRDecuple l r
    where
      l = clatchToDecuple c
      r = join . fmap flockToDecuple $ f

instance HasDecuple (A.PunctaR a) where
  decuple (A.PunctaR f c) = addMaybeLDecuple l r
    where
      l = join . fmap flockToDecuple $ f
      r = clatchToDecuple c

instance HasDecuple (A.NonZero a) where
  decuple r = case r of
    A.WholeOnly a -> decuple a
    A.NZLeft a -> decuple a
    A.NZRight a -> decuple a

instance HasDecuple (A.Quant a) where
  decuple = decuple . A.qNonZero

class HasCoefficient a where
  coefficient :: a -> D.Coefficient

instance HasCoefficient (A.Whole a) where
  coefficient = D.Coefficient . D.Plenus . decuple

instance HasCoefficient (A.PunctaL a) where
  coefficient = D.Coefficient . D.Plenus . decuple

instance HasCoefficient (A.PunctaR a) where
  coefficient = D.Coefficient . D.Plenus . decuple

instance HasCoefficient (A.NonZero a) where
  coefficient r = case r of
    A.WholeOnly a -> coefficient a
    A.NZLeft a -> coefficient a
    A.NZRight a -> coefficient a

instance HasExponent (A.NonZero a) where
  exponent r = case r of
    A.WholeOnly a -> exponent a
    A.NZLeft a -> exponent a
    A.NZRight a -> exponent a

-- # Zeroes

instance HasCoefficient (A.Beak a) where
  coefficient _ = D.Coefficient D.Nil

instance HasCoefficient (A.WingL a) where
  coefficient _ = D.Coefficient D.Nil

instance HasCoefficient (A.WingR a) where
  coefficient _ = D.Coefficient D.Nil

instance HasCoefficient (A.Zero a) where
  coefficient _ = D.Coefficient D.Nil

instance HasCoefficient (A.Quant a) where
  coefficient = coefficient . A.qNonZero

instance HasCoefficient (A.Rep a) where
  coefficient r = case r of
    A.RQuant a -> coefficient a
    A.RZero a -> coefficient a

instance HasExponent (A.Beak a) where
  exponent _ = widthToExp 0

instance HasExponent (A.WingL a) where
  exponent = widthToExp . fromMaybe 0 . fmap width . A.wlRight

instance HasExponent (A.WingR a) where
  exponent = widthToExp . width . A.wrRight

instance HasExponent (A.Zero a) where
  exponent r = case r of
    A.ZBeak a -> exponent a
    A.ZWingL a -> exponent a
    A.ZWingR a -> exponent a

instance HasExponent (A.Quant a) where
  exponent = exponent . A.qNonZero

instance HasExponent (A.Rep a) where
  exponent r = case r of
    A.RQuant a -> exponent a
    A.RZero a -> exponent a
