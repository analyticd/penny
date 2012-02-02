module Penny.Lincoln.Boxes (
  TransactionBox ( transaction, transactionMeta ),
  PostingBox ( postingBundle, metaBundle ),
  posting,
  postingMeta,
  transactionBox,
  topLineMeta,
  PriceBox(PriceBox, price, priceMeta)) where

import Penny.Lincoln.Bits ( PricePoint )
import Penny.Lincoln.Meta (TransactionMeta, unTransactionMeta)
import Penny.Lincoln.Transaction (
  Transaction, TopLine, Posting, unTransaction)
import Penny.Lincoln.Family (children)
import Penny.Lincoln.Family.Child (Child, child, parent)
import Penny.Lincoln.Family.Siblings (flatten)
import Penny.Lincoln.Family.Family ( Family )
import qualified Penny.Lincoln.Family.Family as F

data TransactionBox t p =
  TransactionBox { transaction :: Transaction
                 , transactionMeta :: (Maybe (TransactionMeta t p)) }
  deriving Show

data PostingBox t p =
  PostingBox { postingBundle :: (Child TopLine Posting)
             , metaBundle :: (Maybe (Child t p)) }
  deriving Show

data PriceBox m =
  PriceBox { price :: PricePoint
           , priceMeta :: Maybe m }
  deriving Show

transactionBox :: Transaction
                  -> Maybe (TransactionMeta t p)
                  -> TransactionBox t p
transactionBox t mm = case mm of
  Nothing -> TransactionBox t Nothing
  (Just m) -> let
    (ft, fm) = (unTransaction t, unTransactionMeta m)
    len = length . F.children
    in if len ft /= len fm
       then error "Bundles.hs: error: metadata and transaction mismatch"
       else TransactionBox t (Just m)

postingBoxes :: [TransactionBox t p] -> [PostingBox t p]
postingBoxes = concatMap toBox where
  toBox (TransactionBox cTrans cMeta) = let
    cs = flatten . children . unTransaction $ cTrans
    metas = case cMeta of
      Nothing -> repeat Nothing
      (Just mb) -> map Just . flatten
                   . children . unTransactionMeta $ mb
    in zipWith PostingBox cs metas

posting :: PostingBox t p -> Posting
posting = child . postingBundle

postingMeta :: PostingBox t p -> Maybe p
postingMeta b = metaBundle b >>= return . child

topLineMeta :: PostingBox t p -> Maybe t
topLineMeta b = metaBundle b >>= return . parent
