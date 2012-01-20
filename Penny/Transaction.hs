module Penny.Transaction (
  Transaction,
  Error(..),
  posting1,
  posting2,
  morePostings ) where

import Penny.Posting
import Penny.Qty
import Control.Monad.Exception.Synchronous

-- | All the Postings in a Transaction:
--
-- * May have different Commodities for the Amount in the Entry.
--
-- * Must have the same Commodity for the Amount in the Value.
--
-- * Must produce a Total whose debits and credits are equal.
data Transaction = Transaction { posting1 :: Posting
                               , posting2 :: Posting
                               , morePostings :: [Posting] }

data Error = DifferentDatesError
           | UnbalancedError
           | DifferentCommodityError Total Value

postingsToTransaction :: Posting
                         -> Posting
                         -> [Posting]
                         -> Exceptional Error Transaction
postingsToTransaction = undefined

-- | The value of a posting conceptually is the entry multiplied by
-- the price or, if there is no price, simply the entry.
data Value = Value { valueDrCr :: DrCr
                   , valueAmount :: Amount }

-- | A Total is the sum of multiple Values or Entries that have the
-- same Commodity.
data Total = Total { totCommodity :: Commodity
                   , debits :: Qty
                   , credits :: Qty }

toValue :: Posting -> Value
toValue p = Value dc a where
  dc = drCr . entry $ p
  a = case price p of
    Nothing -> amount . entry $ p
    (Just pr) -> case priceDesc pr of
      TotalPrice -> priceAmt pr
      UnitPrice -> let
        q = (qty . amount . entry $ p)
            `mult` (qty . priceAmt $ pr)
        c = commodity . priceAmt $ pr
        in Amount q c

addValue :: Exceptional Error Total
            -> Value
            -> Exceptional Error Total
addValue e v = case e of
  (Exception err) -> Exception err
  (Success t) ->
    if totCommodity t /= (commodity . valueAmount $ v)
    then Exception $ DifferentCommodityError t v
    else let c = totCommodity t
             q = qty . valueAmount $ v
             (dr, cr) = case valueDrCr v of
               Debit -> ((debits t) `add` q, credits t)
               Credit -> (debits t, (credits t) `add` q)
         in Success $ Total c dr cr

total :: Posting -> Posting -> [Posting] -> Total
total = undefined

toTotal :: Posting -> Total
toTotal p = Total c dr cr where
  a = qty . valueAmount . toValue $ p
  c = case price p of
    Nothing -> commodity . amount . entry $ p
    (Just pr) -> commodity . priceAmt $ pr
  (dr, cr) = case drCr . entry $ p of
    Debit -> (a, zero)
    Credit -> (zero, a)
