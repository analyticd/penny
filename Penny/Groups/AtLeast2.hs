module Penny.Groups.AtLeast2 where

import qualified Penny.Groups.AtLeast1 as A1
import Penny.Groups.FamilyMember ( FamilyMember ( FamilyMember ) )
import qualified Penny.Groups.FamilyMember as F
import Data.List

data AtLeast2 a = AtLeast2 { first :: a
                           , second :: a
                           , rest :: [a] }
                  deriving Show

family :: AtLeast2 a -> [FamilyMember a]
family a = f : s : rs where
  f = FamilyMember (first a) (A1.AtLeast1 (second a) (rest a))
  s = FamilyMember (second a) (A1.AtLeast1 (first a) (rest a))
  rs = map toMember (others . rest $ a)
  toMember (m, as) =
    FamilyMember m (A1.AtLeast1 (first a) ((second a):as))

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))
