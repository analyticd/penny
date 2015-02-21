{-# LANGUAGE TypeFamilies #-}

module Penny.Lincoln.Ledger where

import Prednote
import qualified Prednote as P
import Control.Applicative
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Lincoln.Commodity
import Penny.Lincoln.Prices
import Penny.Lincoln.Exch
import Data.Sequence (Seq, ViewL(..), ViewR(..), (<|), (|>), viewl, viewr)
import qualified Data.Sequence as S
import Penny.Lincoln.Transaction
import Data.Foldable (toList)
import qualified Data.Foldable as F
import Penny.Lincoln.NonEmpty
import qualified Data.Map.Strict as M
import qualified Data.Traversable as T
import Penny.Lincoln.Rep
import Control.Monad

class (Applicative l, Monad l) => Ledger l where
  type PriceL (l :: * -> *) :: *
  type TransactionL (l :: * -> *) :: *
  type TreeL (l :: * -> *) :: *
  type PostingL (l :: * -> *) :: *

  ledgerItems :: l (Seq (Seq (Either (PriceL l) (TransactionL l))))

  priceDate :: PriceL l -> l DateTime
  priceFromTo :: PriceL l -> l FromTo
  priceExch :: PriceL l -> l Exch

  transactionMeta :: TransactionL l -> l (Seq (TreeL l))
  topLineSerial :: TransactionL l -> l TopLineSer
  scalar :: TreeL l -> l Scalar
  realm :: TreeL l -> l Realm
  children :: TreeL l -> l (Seq (TreeL l))
  postings :: TransactionL l -> l (Seq (PostingL l))
  postingTrees :: PostingL l -> l (Seq (TreeL l))
  postingTrio :: PostingL l -> l Trio
  postingQty :: PostingL l -> l Qty
  postingCommodity :: PostingL l -> l Commodity
  postingSerial :: PostingL l -> l PostingSer

-- # Tree search

newtype FoundTree l
  = FoundTree (Either (Labeled Passed, TreeL l) (Labeled Failed, FoundForest l))

data NotFoundTree = NotFoundTree (Labeled Failed) NotFoundForest

data FoundForest l = FoundForest (Seq NotFoundTree) (FoundTree l)

data NotFoundForest = NotFoundForest (Seq NotFoundTree)

-- | Selects a single 'Tree' when given a 'Pred'.  First examines the
-- given 'Tree'; if tht 'Tree' does not match, examines the forest of
-- children using 'selectForest'.
selectTree
  :: Ledger l
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
  :: Ledger l
  => PredM l (TreeL l)
  -> Seq (TreeL l)
  -> l (Either NotFoundForest (FoundForest l))
selectForest pd = go S.empty
  where
    go acc ls = case viewl ls of
      EmptyL -> return . Left . NotFoundForest $ acc
      x :< xs -> do
        treeRes <- selectTree pd x
        case treeRes of
          Left notFound -> go (acc |> notFound) xs
          Right found -> return . Right $ FoundForest acc found

-- # contramapM

contramapM
  :: Monad m
  => (a -> m b)
  -> PredM m b
  -> PredM m a
contramapM conv (PredM f) = PredM $ \a -> conv a >>= f

-- # Predicates on trees

anyTree
  :: Ledger m
  => PredM m (TreeL m)
  -> PredM m (TreeL m)
anyTree p = p ||| anyChildNode
  where
    anyChildNode = contramapM (fmap (fmap toList) children) (P.any p)

allTrees
  :: Ledger m
  => PredM m (TreeL m)
  -> PredM m (TreeL m)
allTrees p = p &&& allChildNodes
  where
    allChildNodes = contramapM (fmap (fmap toList) children) (P.all p)

