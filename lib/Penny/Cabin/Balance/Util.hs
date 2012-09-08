-- | Grab bag of utility functions.

module Penny.Cabin.Balance.Util where

import qualified Penny.Cabin.Options as CO
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.NestedMap as NM
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import Data.List (sortBy)
import Data.Monoid (mconcat, Monoid)
import Data.Maybe (mapMaybe)
import qualified Data.Tree as T
import qualified Penny.Lincoln.Queries as Q

-- | Constructs a forest sorted into tiers based on lists of keys that
-- are extracted from the elements.
tieredForest ::
  Ord k
  => (a -> [k])
  -- ^ Extracts a key from the elements we are putting in the tree. If
  -- this function returns an empty list for any element, the element
  -- will not appear in the tiered forest.
  -> [a]
  -> T.Forest (k, [a])
tieredForest getKeys ls = fmap (fmap revSnd) . NM.toForest $ nm
  where
    revSnd (a, xs) = (a, reverse xs)
    nm = foldr f NM.empty ls
    f a m = NM.relabel m ps
      where
        ps = case getKeys a of
          [] -> []
          ks ->
            let mkInitPair k = (k, maybe [] id)
                mkLastPair k = (k, maybe [] (a:))
            in (map mkInitPair . init $ ks)
               ++ [(mkLastPair (last ks))]

-- | Takes a list of postings and puts them into a Forest. Each level
-- of each of the trees corresponds to a sub account. The label of the
-- node tells you the sub account name and gives you a list of the
-- postings at that level.
tieredPostings :: [L.Box a] -> T.Forest (L.SubAccountName, [L.Box a])
tieredPostings = tieredForest e
  where
    e = Fdbl.toList . L.unAccount . Q.account . L.boxPostFam

-- | Keeps only Trees that match a given condition. First examines
-- child trees to determine whether they should be retained. If a
-- child tree is retained, does not delete the parent tree.
filterForest :: (a -> Bool) -> T.Forest a -> T.Forest a
filterForest f = mapMaybe pruneTree
  where
    pruneTree (T.Node a fs) =
      case filterForest f fs of
        [] -> if not (f a) then Nothing else Just (T.Node a [])
        cs -> Just (T.Node a cs)


-- | Puts all Boxes into a Tree and sums the balances. Removes
-- accounts that have empty balances if requested. Does NOT sum
-- balances from the bottom up.
balances ::
  CO.ShowZeroBalances
  -> [L.Box a]
  -> T.Forest (L.SubAccountName, L.Balance)
balances (CO.ShowZeroBalances szb) =
  remover
  . map (fmap (mapSnd boxesBalance))
  . tieredPostings
  where
    remover =
      if szb
      then id
      else filterForest (M.null . L.unBalance . snd)
           . map (fmap (mapSnd L.removeZeroCommodities))


-- | Sums a forest from the bottom up. Returns a pair, where the first
-- element is the forest, but with the second element of each node
-- replaced with the sum of that node and all its children. The second
-- element is the sum of all the second elements in the forest.
sumForest ::
  s
  -- ^ Zero

  -> (s -> s -> s)
  -- ^ Combiner

  -> T.Forest (a, s)
  -> (T.Forest (a, s), s)
sumForest z f ts = (ts', s)
  where
    ts' = map (sumTree z f) ts
    s = foldr f z . map (snd . T.rootLabel) $ ts'

-- | Sums a tree from the bottom up.
sumTree ::
  s
  -- ^ Zero
  
  -> (s -> s -> s)
  -- ^ Combiner

  ->  T.Tree (a, s)
  -> T.Tree (a, s)
sumTree z f (T.Node (a, s) cs) = T.Node (a, f s cSum) cs'
  where
    (cs', cSum) = sumForest z f cs
  

boxesBalance :: [L.Box a] -> L.Balance
boxesBalance = mconcat . map L.entryToBalance . map Q.entry
               . map L.boxPostFam

mapSnd :: (a -> b) -> (f, a) -> (f, b)
mapSnd f (x, a) = (x, f a)

-- | Label each level of a Tree with an integer indicating how deep it
-- is. The top node of the tree is level 0.
labelLevels :: T.Tree a -> T.Tree (Int, a)
labelLevels = go 0
  where
    go l (T.Node x xs) = T.Node (l, x) (map (go (l + 1)) xs)

-- | Sorts each level of a Forest.
sortForest ::
  (a -> a -> Ordering)
  -> T.Forest a
  -> T.Forest a
sortForest o f = sortBy o' (map (sortTree o) f)
  where
    o' x y = o (T.rootLabel x) (T.rootLabel y)

-- | Sorts each level of a Tree.
sortTree ::
  (a -> a -> Ordering)
  -> T.Tree a
  -> T.Tree a
sortTree o (T.Node l f) = T.Node l (sortForest o f)
