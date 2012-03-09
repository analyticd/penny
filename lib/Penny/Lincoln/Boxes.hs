module Penny.Lincoln.Boxes (
  TransactionBox ( transaction, transactionMeta ),
  PostingBox ( postingBundle, metaBundle ),
  PriceBox (PriceBox, price, priceMeta),
  posting,
  postingMeta,
  transactionBox,
  topLineMeta,
  postingBoxes) where

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
  deriving Show

data PostingBox =
  PostingBox { postingBundle :: Child TopLine Posting
             , metaBundle :: Maybe (Child TopLineMeta PostingMeta) }
  deriving Show

data PriceBox =
  PriceBox { price :: PricePoint
           , priceMeta :: Maybe PriceMeta }
  deriving Show

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
