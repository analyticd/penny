-- | A Transaction consists of a TopLine (information common to all
-- postings, such as the DateTime) and of at least two Postings. This
-- data relationship is so important and useful that it is expressed
-- in these modules. This module has its own functions and re-exports
-- functions and types from other modules in this hierarchy. There are
-- handy accessor functions for the records within each of the family
-- types, but these are not exported due to name conflicts; if you
-- want these simply import the necessary module (maybe qualified).
module Penny.Lincoln.Family (
  -- * Family types
  F.Family(Family),
  C.Child(Child),
  S.Siblings(Siblings),
  
  -- * Functions to manipulate families
  children,
  orphans,
  adopt,
  marryWith,
  marry,
  divorceWith,
  divorce,
  S.collapse ) where

import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Child as C
import qualified Penny.Lincoln.Family.Siblings as S

-- | Gets a family's children. The Child type contains information on
-- the parent, and each Child contains information on the other
-- Siblings.
children :: F.Family p c -> S.Siblings (C.Child p c)
children (F.Family p c1 c2 cRest) = S.Siblings fc sc rc where
  fc = C.Child c1 c2 cRest p
  sc = C.Child c2 c1 cRest p
  rc = map toChild rest
  rest = others cRest
  toChild (c, cs) = C.Child c c1 (c2:cs) p

-- | Separates the children from their parent.
orphans :: F.Family p c -> S.Siblings c
orphans (F.Family _ c1 c2 cs) = S.Siblings c1 c2 cs

-- | Unites a parent and some siblings into one family; the dual of
-- orphans.
adopt :: p -> S.Siblings c -> F.Family p c
adopt p (S.Siblings c1 c2 cs) = F.Family p c1 c2 cs

-- | Marries two families into one. This function is rather cruel: if
-- one family has more children than the other family, then the extra
-- children are discarded. That is, all children must pair one-by-one.
marryWith :: (p1 -> p2 -> p3)
             -> (c1 -> c2 -> c3)
             -> F.Family p1 c1
             -> F.Family p2 c2
             -> F.Family p3 c3
marryWith fp fc (F.Family lp lc1 lc2 lcs) (F.Family rp rc1 rc2 rcs) =
  F.Family (fp lp rp) (fc lc1 rc1) (fc lc2 rc2)
  (zipWith fc lcs rcs)

-- | marryWith a tupling function.
marry :: F.Family p1 c1
         -> F.Family p2 c2
         -> F.Family (p1, p2) (c1, c2)
marry = marryWith (,) (,)
  
-- | Splits up a family.
divorceWith :: (p1 -> (p2, p3))
             -> (c1 -> (c2, c3))
             -> F.Family p1 c1
             -> (F.Family p2 c2, F.Family p3 c3)
divorceWith fp fc (F.Family p c1 c2 cs) = (f2, f3) where
  f2 = F.Family p2 c21 c22 c2s
  f3 = F.Family p3 c31 c32 c3s
  (p2, p3) = fp p
  (c21, c31) = fc c1
  (c22, c32) = fc c2
  cps = map fc cs
  (c2s, c3s) = (map fst cps, map snd cps)

-- | divorceWith an untupling function.
divorce :: F.Family (p1, p2) (c1, c2)
         -> (F.Family p1 c1, F.Family p2 c2)
divorce = divorceWith id id

others :: [a] -> [(a, [a])]
others = map yank . allIndexes

allIndexes :: [a] -> [(Int, [a])]
allIndexes as = zip [0..] (replicate (length as) as)

yank :: (Int, [a]) -> (a, [a])
yank (i, as) = let
  (ys, zs) = splitAt i as
  in (head zs, ys ++ (tail zs))

