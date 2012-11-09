-- | Grab bag of utility functions.

module Penny.Cabin.Balance.Util
  ( tieredForest
  , tieredPostings
  , filterForest
  , balances
  , flatten
  , treeWithParents
  , forestWithParents
  , sumForest
  , sumTree
  , boxesBalance
  , labelLevels
  , sortForest
  , sortTree
  , lastMode
  , lastModeBy
  , modes
  , modesBy
  , longestLists
  ) where

import qualified Penny.Cabin.Options as CO
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.NestedMap as NM
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy, groupBy)
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
                mkLastPair k = (k, maybe [a] (a:))
            in (map mkInitPair . init $ ks)
               ++ [(mkLastPair (last ks))]

-- | Takes a list of postings and puts them into a Forest. Each level
-- of each of the trees corresponds to a sub account. The label of the
-- node tells you the sub account name and gives you a list of the
-- postings at that level.
tieredPostings :: [L.Box a] -> T.Forest (L.SubAccount, [L.Box a])
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
  -> T.Forest (L.SubAccount, L.Balance)
balances (CO.ShowZeroBalances szb) =
  remover
  . map (fmap (mapSnd boxesBalance))
  . tieredPostings
  where
    remover =
      if szb
      then id
      else filterForest (not . M.null . L.unBalance . snd)
           . map (fmap (mapSnd L.removeZeroCommodities))


-- | Takes a tree of Balances (like what is produced by the 'balances'
-- function) and produces a flat list of accounts with the balance of
-- each account.
flatten
  :: T.Forest (L.SubAccount, L.Balance)
  -> [(L.Account, L.Balance)]
flatten =
  concatMap T.flatten
  . map (fmap toPair) . forestWithParents
  where
    toPair ((s, b), ls) =
      case reverse . map fst $ ls of
        [] -> (L.Account [s], b)
        s1:sr -> (L.Account (s1 : (sr ++ [s])), b)

-- | Takes a Tree and returns a Tree where each node has information
-- about its parent Nodes. The list of parent nodes has the most
-- immediate parent first and the most distant parent last.
treeWithParents :: T.Tree a -> T.Tree (a, [a])
treeWithParents = treeWithParentsR []

-- | Given a list of the parents seen so far, return a Tree where each
-- node contains information about its parents.
treeWithParentsR :: [a] -> T.Tree a -> T.Tree (a, [a])
treeWithParentsR ls (T.Node n cs) = T.Node (n, ls) cs'
  where
    cs' = map (treeWithParentsR (n:ls)) cs

-- | Takes a Forest and returns a Forest where each node has
-- information about its parent Nodes.
forestWithParents :: T.Forest a -> T.Forest (a, [a])
forestWithParents = map (treeWithParentsR [])

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

-- | Like lastModeBy but using Ord.
lastMode :: Ord a => [a] -> Maybe a
lastMode = lastModeBy compare

-- | Finds the mode of a list. Takes the mode that is located last in
-- the list. Returns Nothing if there is no mode (that is, if the list
-- is empty).
lastModeBy ::
  (a -> a -> Ordering)
  -> [a]
  -> Maybe a
lastModeBy o ls =
  case modesBy o' ls' of
    [] -> Nothing
    ms -> Just . fst . maximumBy fx $ ms
    where
      fx = comparing snd
      ls' = zip ls ([0..] :: [Int])
      o' x y = o (fst x) (fst y)

-- | Like modesBy but using Ord.
modes :: Ord a => [a] -> [a]
modes = modesBy compare

-- | Finds the modes of a list.
modesBy :: (a -> a -> Ordering) -> [a] -> [a]
modesBy o =
  concat
  . longestLists
  . groupBy (\x y -> o x y == EQ)
  . sortBy o


-- | Returns the longest lists.
longestLists :: [[a]] -> [[a]]
longestLists as =
  let lengths = map (\ls -> (ls, length ls)) as
      maxLen = maximum . map snd $ lengths
  in map fst . filter (\(_, len) -> len == maxLen) $ lengths
