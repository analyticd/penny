{-# LANGUAGE TypeFamilies, RecursiveDo #-}

module Penny.Lincoln.Ledger where

import Prednote
import Control.Applicative
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Lincoln.Commodity
import Penny.Lincoln.Prices
import Penny.Lincoln.Exch
import Control.Monad.Fix

class Ledger l where
  type PriceL (l :: * -> *) :: *
  type TransactionL (l :: * -> *) :: *
  type TreeL (l :: * -> *) :: *
  type PostingL (l :: * -> *) :: *

  ledgerItems :: l [Either (PriceL l) (TransactionL l)]

  priceDate :: PriceL l -> l DateTime
  priceFromTo :: PriceL l -> l FromTo
  priceExch :: PriceL l -> l Exch

  transactionMeta :: TransactionL l -> l [TreeL l]
  scalar :: TreeL l -> l Scalar
  realm :: TreeL l -> l Realm
  children :: TreeL l -> l [TreeL l]
  postings :: TransactionL l -> l [PostingL l]
  postingTrees :: PostingL l -> l [TreeL l]
  postingTrio :: PostingL l -> l Trio
  postingQty :: PostingL l -> l Qty
  postingCommodity :: PostingL l -> l Commodity

-- # Clatches

data Clatch l
  = Clatch (TransactionL l) [PostingL l] (PostingL l) [PostingL l]
  -- ^ @Clatch t l c r@, where
  --
  -- @t@ is the transaction giving rise to this 'Clatch',
  --
  -- @l@ are sibling postings on the left.  Closer siblings are at the
  -- head of the list.
  --
  -- @c@ is the current posting of this 'Clatch', and
  --
  -- @r@ are sibling postings on the right.  Closer siblings are at
  -- the head of the list.

nextClatch :: Clatch l -> Maybe (Clatch l)
nextClatch (Clatch t l c r) = case r of
  [] -> Nothing
  x:xs -> Just $ Clatch t (c : l) x xs

prevClatch :: Clatch l -> Maybe (Clatch l)
prevClatch (Clatch t l c r) = case l of
  [] -> Nothing
  x:xs -> Just $ Clatch t xs x (c : r)

clatches :: (Functor l, Ledger l) => TransactionL l -> l [Clatch l]
clatches txn = fmap (go []) $ postings txn
  where
    go soFar curr = case curr of
      [] -> []
      x:xs ->
        let this = Clatch txn soFar x xs
        in this : go (x : soFar) xs

-- # Tree search

newtype FoundTree l
  = FoundTree (Either (Labeled Passed, TreeL l) (Labeled Failed, FoundForest l))

data NotFoundTree = NotFoundTree (Labeled Failed) NotFoundForest

data FoundForest l = FoundForest [NotFoundTree] (FoundTree l)

data NotFoundForest = NotFoundForest [NotFoundTree]

-- | Selects a single 'Tree' when given a 'Pred'.  First examines the
-- given 'Tree'; if tht 'Tree' does not match, examines the forest of
-- children using 'selectForest'.
selectTree
  :: (Monad l, Ledger l)
  => PredM l (TreeL l)
  -> TreeL l
  -> l (Either NotFoundTree (FoundTree l))
selectTree pd tr = do
  res <- runPredM pd tr
  case splitResult res of
    Left bad -> do
      kids <- children tr
      ei <- selectForest pd kids
      return $ case ei of
        Left notFound -> Left $ NotFoundTree bad notFound
        Right found -> Right $ FoundTree (Right (bad, found))
    Right good -> return . Right . FoundTree . Left $ (good, tr)


-- | Selects a single 'Tree' when given a 'Pred' and a list of 'Tree'.
-- Examines each 'Tree' in the list, in order, using 'selectTree'.
selectForest
  :: (Monad l, Ledger l)
  => PredM l (TreeL l)
  -> [TreeL l]
  -> l (Either NotFoundForest (FoundForest l))
selectForest pd = go []
  where
    go acc [] = return . Left . NotFoundForest . reverse $ acc
    go acc (x:xs) = do
      treeRes <- selectTree pd x
      case treeRes of
        Left notFound -> go (notFound : acc) xs
        Right found -> return . Right $ FoundForest (reverse acc) found

-- # contramapM

contramapM
  :: Monad m
  => (a -> m b)
  -> PredM m b
  -> PredM m a
contramapM conv (PredM f) = PredM $ \a -> conv a >>= f

-- # Predicates on trees

data TreeZip l
  = TreeZip (Maybe (TreeZip l)) [TreeZip l] [TreeZip l]

{-
-- | A zipper to view 'TreeL'.
data TreeZip l
  = TreeZip (Maybe (ForestZip l)) (TreeL l) (ForestZip l)
  -- ^ @TreeZip m t f@, where
  --
  -- @m@ is this tree's parent forest (if any)
  --
  -- @t@ is this tree
  --
  -- @z@ is the child forest of this tree.

-- | A zipper to view forests of 'TreeL'.
data ForestZip l
  = ForestZip (Maybe (TreeZip l)) [TreeZip l] [TreeZip l]
  -- ^ @ForestZip m l r@, where
  --
  -- @m@ is the parent tree of this forest (if any)
  --
  -- @l@ is trees to the left, and
  --
  -- @r@ is trees to the right.

treeZip
  :: (Monad l, Applicative l, Ledger l, MonadFix l)
  => Maybe (ForestZip l)
  -> TreeL l
  -> l (TreeZip l)
treeZip p t = mdo
  cs <- children t
  fs <- forestZip (Just this) cs
  let this = TreeZip p t fs
  return this

forestZip
  :: (Monad l, Applicative l, Ledger l, MonadFix l)
  => Maybe (TreeZip l)
  -> [TreeL l]
  -> l (ForestZip l)
forestZip p ts = mdo
  let this = ForestZip p [] cs
  cs <- mapM (treeZip (Just this)) ts
  return this

nextTree :: ForestZip l -> Maybe (ForestZip l)
nextTree (ForestZip p l r) = case r of
  [] -> Nothing
  x:xs -> Just (ForestZip p (x : l) xs)

prevTree :: ForestZip l -> Maybe (ForestZip l)
prevTree (ForestZip p l r) = case l of
  [] -> Nothing
  x:xs -> Just (ForestZip p xs (x : r))

parentTree :: ForestZip l -> Maybe (TreeZip l)
parentTree (ForestZip p _ _) = p

childForest
  :: (Monad l, Applicative l, Ledger l, MonadFix l)
  => TreeZip l
  -> l (ForestZip l)
childForest tz@(TreeZip _ t _) = do
  cs <- children t
  forestZip (Just tz) cs

-- | True if any 'TreeZip' in the Forest is True.
anyTree
  :: (Monad l, Applicative l, Ledger l, MonadFix l)
  => PredM l (TreeZip l)
  -> PredM l (ForestZip l)
anyTree = undefined
-}
