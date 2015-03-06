{-# LANGUAGE TypeFamilies #-}

module Penny.Lincoln.Ledger where

import Prednote (PredM(..), (&&&), (|||) )
import qualified Prednote as P
import Control.Applicative
import Penny.Lincoln.Field
import Penny.Lincoln.Trio
import Penny.Lincoln.Qty
import Penny.Lincoln.DateTime
import Penny.Lincoln.Commodity
import Penny.Lincoln.Prices
import Penny.Lincoln.Exch
import Data.Sequence (Seq, ViewL(..), viewl)
import qualified Data.Sequence as S
import Penny.Lincoln.Transaction
import Data.Foldable (toList)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as X

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
  scalar :: TreeL l -> l (Maybe Scalar)
  realm :: TreeL l -> l Realm
  children :: TreeL l -> l (Seq (TreeL l))
  postings :: TransactionL l -> l (Seq (PostingL l))
  postingTrees :: PostingL l -> l (Seq (TreeL l))
  postingTrio :: PostingL l -> l Trio
  postingQty :: PostingL l -> l Qty
  postingCommodity :: PostingL l -> l Commodity
  postingSerial :: PostingL l -> l PostingSer

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

-- # Searching for trees

findTree
  :: Ledger l
  => (TreeL l -> l Bool)
  -> TreeL l
  -> l (Maybe (TreeL l))
findTree pd tr = pd tr >>= go
  where
    go found
      | found = return (Just tr)
      | otherwise = do
          fs <- children tr
          findTreeInForest pd fs

findTreeInForest
  :: Ledger l
  => (TreeL l -> l Bool)
  -> Seq (TreeL l)
  -> l (Maybe (TreeL l))
findTreeInForest pd sq = case viewl sq of
  EmptyL -> return Nothing
  x :< xs -> do
    r <- findTree pd x
    case r of
      Nothing -> findTreeInForest pd xs
      Just a -> return (Just a)

pdNoScalarUser :: Ledger l => TreeL l -> l Bool
pdNoScalarUser tr = realm tr >>= go
  where
    go rlm
      | rlm == User = do
          mayScl <- scalar tr
          return $ case mayScl of
            Nothing -> True
            _ -> False
      | otherwise = return False
      

pdScalarUser
  :: Ledger l
  => (Scalar -> Bool)
  -> TreeL l
  -> l Bool
pdScalarUser pd tr = realm tr >>= go
  where
    go rlm
      | rlm == User = do
          mayScl <- scalar tr
          return $ case mayScl of
            Nothing -> False
            Just scl -> pd scl
      | otherwise = return False

-- # Displaying trees

-- To deal with special Unicode characters in Emacs, use C-x 8 to
-- insert them.  C-x = will give brief information about the character
-- at point; M-x describe-char gives more detailed information.

-- | Displays a tree.  It's impractical to display any of the children
-- of the child trees, so any child tree that has children is suffixed
-- with a down arrow (which is U+2193, or ↓) to let the user know
-- something is down there.

displayTree
  :: Ledger l
  => TreeL l
  -> l Text
displayTree t = f <$> scalar t <*> children t
  where
    f sc cs = maybe X.empty displayScalar sc <>
      if S.null cs then mempty else X.singleton '↓'

-- | Displays a forest of trees, with each separated by a bullet
-- (which is U+2022, or •).
displayForest
  :: Ledger l
  => Seq (TreeL l)
  -> l Text
displayForest sq = case viewl sq of
  EmptyL -> return X.empty
  x1 :< xs1 -> do
    t1 <- displayTree x1
    let dispNext t = fmap (X.cons '•') $ displayTree t
    fmap (F.foldl' mappend t1) $ T.traverse dispNext xs1
