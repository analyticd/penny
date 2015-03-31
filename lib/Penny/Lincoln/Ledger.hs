{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | A 'Ledger' is a store of transactions and prices.  'Ledger'
-- specifies an interface for such stores.
module Penny.Lincoln.Ledger where

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
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Text (Text)
import Data.Monoid
import qualified Data.Text as X
import Control.Monad.Reader

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
  namespace :: TreeL l -> l Realm
  children :: TreeL l -> l (Seq (TreeL l))
  postings :: TransactionL l -> l (Seq (PostingL l))
  postingTrees :: PostingL l -> l (Seq (TreeL l))
  postingTrio :: PostingL l -> l Trio
  postingQty :: PostingL l -> l Qty
  postingCommodity :: PostingL l -> l Commodity
  postingSerial :: PostingL l -> l PostingSer

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
