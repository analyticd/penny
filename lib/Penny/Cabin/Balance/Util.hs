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
  ) where

import Control.Arrow (second, first)
import qualified Penny.Cabin.Options as CO
import qualified Penny.Lincoln as L
import Data.Tuple (swap)
import Data.Either (partitionEithers)
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy, maximumBy, groupBy)
import Data.Monoid (mconcat, Monoid)
import Data.Maybe (mapMaybe)
import qualified Data.Tree as T
import qualified Penny.Lincoln.Queries as Q

-- | Takes a list of postings and puts them into a Forest. Each level
-- of each of the trees corresponds to a sub account. The label of the
-- node tells you the sub account name and gives you a list of the
-- postings at that level.
tieredPostings
  :: [(a, L.Posting)]
  -> ([(a, L.Posting)], T.Forest (L.SubAccount, [(a, L.Posting)]))
tieredPostings = second (map (fmap swap)) . tieredForest e
  where
    e = L.unAccount . Q.account . snd

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
balances
  :: CO.ShowZeroBalances
  -> [(a, L.Posting)]
  -> (L.Balance, T.Forest (L.SubAccount, L.Balance))
balances (CO.ShowZeroBalances szb)
  = first boxesBalance
  . second remover
  . second (map (fmap (second boxesBalance)))
  . tieredPostings
  where
    remover =
      if szb
      then id
      else filterForest (not . M.null . L.unBalance . snd)
           . map (fmap (second L.removeZeroCommodities))


-- | Takes a tree of Balances (like what is produced by the 'balances'
-- function) and produces a flat list of accounts with the balance of
-- each account. Also adds in the first balance, which is for Accounts
-- that have no sub-accounts.
flatten
  :: (L.Balance, T.Forest (L.SubAccount, L.Balance))
  -> [(L.Account, L.Balance)]
flatten (top, frst) = (L.Account [], top) : rest
  where
    rest
      = concatMap T.flatten
      . map (fmap toPair)
      . forestWithParents
      $ frst
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


boxesBalance :: [(a, L.Posting)] -> L.Balance
boxesBalance
  = mconcat
  . map (either L.entryToBalance L.entryToBalance)
  . map Q.entry
  . map snd

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

-- | Finds the modes of a list.
modesBy :: (a -> a -> Ordering) -> [a] -> [a]
modesBy o =
  concat
  . longestLists
  . groupBy (\x y -> o x y == EQ)
  . sortBy o


-- | Returns the longest lists. This function is partial. It is bottom
-- if the argument list is empty. Therefore, do not export this
-- function.
longestLists :: [[a]] -> [[a]]
longestLists as =
  let lengths = map (\ls -> (ls, length ls)) as
      maxLen = maximum . map snd $ lengths
  in map fst . filter (\(_, len) -> len == maxLen) $ lengths

--
-- # Tiered forest
--

-- | Places items into a tiered forest.
tieredForest
  :: Ord b
  => (a -> [b])
  -- ^ Function that, when applied to an item, returns a list.  The
  -- items will be placed into a tiered forest according to each list.

  -> [a]
  -- ^ List of items to put into the forest

  -> ([a], T.Forest ([a], b))
  -- ^ fst is the list of items for which the function returned an
  -- empty list. The forest includes all other items.
tieredForest f
  = second forest
  . groupByHead
  . sortBy (comparing snd)
  . map (\a -> (a, f a))

tree
  :: Eq b
  => b
  -> ([a], [(b, [(a, [b])])])
  -> T.Tree ([a], b)
tree lbl (as, rest) = T.Node (as, lbl) (forest rest)

forest
  :: Eq b
  => [(b, [(a, [b])])]
  -> T.Forest ([a], b)
forest = map (uncurry tree . second groupByHead)

groupByHead
  :: Eq b
  => [(a, [b])]
  -> ([a], [(b, [(a, [b])])])
groupByHead
  = second groupPairs
  . partitionEithers
  . map pluckHead

pluckHead
  :: (a, [b])
  -> Either a (b, (a, [b]))
pluckHead (a, []) = Left a
pluckHead (a, b:bs) = Right (b, (a, bs))

groupPairs
  :: Eq a
  => [(a, b)]
  -> [(a, [b])]
groupPairs
  = map (\ls -> (fst . head $ ls, map snd ls))
  . groupBy (\x y -> fst x == fst y)

