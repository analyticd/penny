module Penny.Transaction (
  Transaction,
  Error(..), 
  transaction, 
  siblings ) where

import Penny.Posting
import Penny.Qty
import Control.Monad.Exception.Synchronous
import Penny.Groups.AtLeast2
import Penny.Groups.FamilyMember

-- | All the Postings in a Transaction:
--
-- * May have different Commodities for the Amount in the Entry.
--
-- * Must have the same Commodity for the Amount in the Value.
--
-- * Must produce a Total whose debits and credits are equal.
newtype Transaction = Transaction (AtLeast2 Posting)

data Error = DifferentDatesError
           | UnbalancedError Total
           | DifferentCommodityError Total Value

-- | Get the Postings from a Transaction, with information on the
-- sibling Postings.
postingFamily :: Transaction -> [FamilyMember Posting]
postingFamily (Transaction ps) = family ps

transaction :: AtLeast2 Posting -> Exceptional Error Transaction
transaction a@(AtLeast2 p1 p2 ps) = do
  t <- totalPostings p1 p2 ps
  if isBalanced t
    then return (Transaction a)
    else throw $ UnbalancedError t

-- | The value of a posting conceptually is the entry multiplied by
-- the price or, if there is no price, simply the entry.
data Value = Value { valueDrCr :: DrCr
                   , valueAmount :: Amount }

-- | A Total is the sum of multiple Values or Entries that have the
-- same Commodity.
data Total = Total { totCommodity :: Commodity
                   , debits :: Qty
                   , credits :: Qty }

isBalanced :: Total -> Bool
isBalanced t = debits t == credits t

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

totalPostings :: Posting -> Posting -> [Posting] -> Exceptional Error Total
totalPostings p1 p2 ps = foldl addValue i (map toValue ps) where
  i = addValue (return . toTotal $ p1) (toValue p2)

toTotal :: Posting -> Total
toTotal p = Total c dr cr where
  a = qty . valueAmount . toValue $ p
  c = case price p of
    Nothing -> commodity . amount . entry $ p
    (Just pr) -> commodity . priceAmt $ pr
  (dr, cr) = case drCr . entry $ p of
    Debit -> (a, zero)
    Credit -> (zero, a)
