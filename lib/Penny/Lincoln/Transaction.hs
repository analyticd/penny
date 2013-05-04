{-# LANGUAGE DeriveGeneric, DeriveFunctor #-}

module Penny.Lincoln.Transaction
  ( Inferred(..)
  , Posting
  , pEntry
  , pInferred
  , pMeta
  , Transaction
  , unTransaction
  , mapPostings
  , transaction
  , rTransaction
  , View
  , unView
  , views
  ) where

import Data.Binary (Binary)
import GHC.Generics (Generic)
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Balance as Bal
import Control.Monad (guard)
import qualified Data.Map as M
import Data.Monoid (mconcat)
import Data.List (foldl')
import Data.Maybe (isNothing, catMaybes)

data Inferred = Inferred | NotInferred
  deriving (Eq, Ord, Show, Generic)

instance Binary Inferred

data Posting m = Posting
  { pEntry :: B.Entry
  , pInferred :: Inferred
  , pMeta :: m
  } deriving (Eq, Ord, Show, Generic)

instance Functor Posting where
  fmap f (Posting e i m) = Posting e i (f m)

instance Binary m => Binary (Posting m)

newtype Transaction m = Transaction { unTransaction :: [Posting m] }
  deriving (Eq, Ord, Show, Generic, Functor)

instance Binary m => Binary (Transaction m)

newtype View m = View { unView :: [Posting m] }
  deriving (Eq, Ord, Show, Functor)

views :: Transaction m -> [View m]
views = map View . orderedPermute . unTransaction


-- | Returns a list of lists where each element in the original list
-- is in the front of a new list once.
--
-- > orderedPermute [1,2,3] == [[1,2,3], [2,3,1], [3,1,2]]
orderedPermute :: [a] -> [[a]]
orderedPermute ls = take (length ls) (iterate toTheBack ls)
  where
    toTheBack [] = []
    toTheBack (a:as) = as ++ [a]

mapPostings
  :: (Posting a -> Posting b)
  -> Transaction a
  -> Transaction b
mapPostings f = Transaction . map f . unTransaction

transaction
  :: [(Maybe B.Entry, m)]
  -> Maybe (Transaction m)
transaction ls = fmap Transaction $ mapM makePstg ls
  where
    makePstg = makePosting (inferredVal . map fst $ ls)

rTransaction
  :: B.Commodity
  -- ^ Commodity for all postings
  -> B.DrCr
  -- ^ DrCr for all non-inferred postings
  -> (B.Qty, m)
  -- ^ Non-inferred posting 1
  -> [(B.Qty, m)]
  -- ^ Remaining non-inferred postings
  -> m
  -- ^ Metadata for inferred posting
  -> Transaction m
rTransaction com dc (q1, m1) nonInfs lastMeta =
  let tot = foldl' B.add q1 . map fst $ nonInfs
      p1 = makePstg (q1, m1)
      ps = map makePstg nonInfs
      makePstg (q, m) = Posting (B.Entry dc (B.Amount q com))
                                NotInferred m
      lastPstg = Posting (B.Entry (B.opposite dc) (B.Amount tot com))
                         Inferred lastMeta
  in Transaction $ p1:ps ++ [lastPstg]


-- | Changes Maybe Entries into Postings. Uses the inferred value if
-- the Maybe Entry is Nothing. If there is no inferred value, returns
-- Nothing.
makePosting
  :: Maybe B.Entry
  -- ^ Inferred value
  -> (Maybe B.Entry, m)
  -> Maybe (Posting m)
makePosting mayInf (mayEn, m) = case mayEn of
  Nothing -> case mayInf of
    Nothing -> Nothing
    Just inf -> return $ Posting inf Inferred m
  Just en -> return $ Posting en NotInferred m


-- | Gets a single inferred entry from a balance, if possible.
inferredVal :: [Maybe B.Entry] -> Maybe B.Entry
inferredVal ls = do
  guard ((length . filter id . map isNothing $ ls) == 1)
  let bal = Bal.removeZeroCommodities
            . mconcat
            . map Bal.entryToBalance
            . catMaybes
            $ ls
  (com, bl) <- case M.assocs . Bal.unBalance $ bal of
    [] -> Nothing
    x:[] -> return x
    _ -> Nothing
  (drCr, qty) <- case bl of
    Bal.Zero -> Nothing
    Bal.NonZero col -> return (B.opposite . Bal.drCr $ col
                              , Bal.qty col)
  return $ B.Entry drCr (B.Amount qty com)
