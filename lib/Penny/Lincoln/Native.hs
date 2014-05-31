module Penny.Lincoln.Native where

import qualified Penny.Lincoln.Rep as A
import qualified Deka.Native.Abstract as D
import Prelude hiding (exponent)

data CoeffExp = CoeffExp
  { coeff :: D.Coefficient
  , exponent :: D.Exponent
  }

class Number a where
  number :: a -> CoeffExp

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

wholeToCoeff :: A.Whole a -> D.Coefficient
wholeToCoeff = D.Coefficient . D.Plenus .
  clatchToDecuple . A.unWhole

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

