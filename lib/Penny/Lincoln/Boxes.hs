-- | Lincoln separates transactions and postings from metadata about
-- the transactions and postings. This is because not all postings
-- necessarily have metadata. However, when you do have metadata, you
-- will want to handle it together with the posting or the
-- transaction; this information goes into a Box.
module Penny.Lincoln.Boxes (
  -- * Boxes
  TransactionBox ( transaction, transactionMeta ),
  PostingBox ( postingBundle, metaBundle ),
  PriceBox (PriceBox, price, priceMeta),
  
  -- * Functions to manipulate boxes
  -- ** Making boxes
  transactionBox,
  postingBoxes,

  -- ** Deconstructing posting boxes
  posting,
  postingMeta,
  topLineMeta,
  ) where

import Data.Foldable (toList)

import Penny.Lincoln.Meta
  (TransactionMeta, unTransactionMeta, TopLineMeta, PostingMeta,
   PriceMeta)
import Penny.Lincoln.Bits (PricePoint)
import Penny.Lincoln.Transaction (
  Transaction, TopLine, Posting, unTransaction)
import Penny.Lincoln.Family (children)
import Penny.Lincoln.Family.Child (Child, child, parent)
import qualified Penny.Lincoln.Family.Family as F

data TransactionBox =
  TransactionBox { transaction :: Transaction
                 , transactionMeta :: Maybe TransactionMeta }
  deriving (Eq, Show)

-- | A PostingBox holds not only information about a particular
-- posting, but also information about the associated TopLine and also
-- information on all related Postings. There is also information
-- about metadata for all these things, if there is any. The functions
-- in 'Penny.Lincoln.Queries' make use of all this information to
-- provide the \"best\" information about a posting--for instance, if
-- a posting has no Number, but its associated TopLine does, the
-- number function will provide the number from the TopLine.
data PostingBox =
  PostingBox { postingBundle :: Child TopLine Posting
             , metaBundle :: Maybe (Child TopLineMeta PostingMeta) }
  deriving Show

data PriceBox =
  PriceBox { price :: PricePoint
           , priceMeta :: Maybe PriceMeta }
  deriving Show

-- | Makes a TransactionBox. Unites each element of a family of
-- Postings with the correct element of a family of metadata. WARNING
-- /This function is partial/. It will apply 'error' if there is not
-- exactly the same number of postings as there are children in the
-- metadata family. This always indicates some sort of programmer
-- error so it is pointless to anything other than apply 'error'.
transactionBox :: Transaction
                  -> Maybe TransactionMeta
                  -> TransactionBox
transactionBox t mm = case mm of
  Nothing -> TransactionBox t Nothing
  (Just m) -> let
    (ft, fm) = (unTransaction t, unTransactionMeta m)
    len = length . F.children
    in if len ft /= len fm
       then error "Boxes.hs: error: metadata and transaction mismatch"
       else TransactionBox t (Just m)

-- | Changes a list of TransactionBoxes into a list of
-- PostingBoxes. This will always result in a longer list, because
-- each Transaction has at least two Postings.
postingBoxes :: [TransactionBox] -> [PostingBox]
postingBoxes = concatMap toBox where
  toBox (TransactionBox cTrans cMeta) = let
    cs = toList . children . unTransaction $ cTrans
    metas = case cMeta of
      Nothing -> repeat Nothing
      (Just mb) -> map Just . toList
                   . children . unTransactionMeta $ mb
    in zipWith PostingBox cs metas

posting :: PostingBox -> Posting
posting = child . postingBundle

postingMeta :: PostingBox -> Maybe PostingMeta
postingMeta b = metaBundle b >>= return . child

topLineMeta :: PostingBox -> Maybe TopLineMeta
topLineMeta b = metaBundle b >>= return . parent
