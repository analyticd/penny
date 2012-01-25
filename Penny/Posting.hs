module Penny.Posting where

import qualified Penny.Bits as B
import qualified Penny.Bits.Qty as Q
import Penny.Bits.Qty ( add, mult )
import qualified Penny.Posting.Parent as P
import Penny.Groups.AtLeast2 (
  AtLeast2 ( AtLeast2 ), family )
import Penny.Groups.FamilyMember ( FamilyMember )
import Control.Monad.Exception.Synchronous (
  Exceptional (Exception, Success) , throw )

data Posting =
  Posting { payee :: B.Payee
          , number :: Maybe B.Number
          , cleared :: B.Cleared
          , account :: B.Account
          , entry :: B.Entry
          , price :: Maybe B.Price
          , tags :: B.Tags
          , memo :: Maybe B.Memo
          , parent :: P.Parent }

-- | All the Postings in a Transaction:
--
-- * May have different Commodities for the Amount in the Entry.
--
-- * Must have the same Commodity for the Amount in the Value.
--
-- * Must produce a Total whose debits and credits are equal.
newtype Transaction =
  Transaction { unTransaction :: AtLeast2 Posting }
  
data Error = DifferentDatesError
           | UnbalancedError Total
           | DifferentCommodityError Total Value

-- | A Total is the sum of multiple Values or Entries that have the
-- same Commodity.
data Total = Total { totCommodity :: B.Commodity
                   , debits :: Q.Qty
                   , credits :: Q.Qty }

-- | The value of a posting conceptually is the entry multiplied by
-- the price or, if there is no price, simply the entry.
data Value = Value { valueDrCr :: B.DrCr
                   , valueAmount :: B.Amount }

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

isBalanced :: Total -> Bool
isBalanced t = debits t == credits t

addValue :: Exceptional Error Total
            -> Value
            -> Exceptional Error Total
addValue e v = case e of
  (Exception err) -> Exception err
  (Success t) ->
    if totCommodity t /= (B.commodity . valueAmount $ v)
    then Exception $ DifferentCommodityError t v
    else let c = totCommodity t
             q = B.qty . valueAmount $ v
             (dr, cr) = case valueDrCr v of
               B.Debit -> ((debits t) `add` q, credits t)
               B.Credit -> (debits t, (credits t) `add` q)
         in Success $ Total c dr cr

toValue :: Posting -> Value
toValue p = Value dc a where
  dc = B.drCr . entry $ p
  a = case price p of
    Nothing -> B.amount . entry $ p
    (Just pr) -> case B.priceDesc pr of
      B.TotalPrice -> B.priceAmt pr
      B.UnitPrice -> let
        q = (B.qty . B.amount . entry $ p)
            `mult` (B.qty . B.priceAmt $ pr)
        c = B.commodity . B.priceAmt $ pr
        in B.Amount q c

totalPostings :: Posting -> Posting -> [Posting] -> Exceptional Error Total
totalPostings p1 p2 ps = foldl addValue i (map toValue ps) where
  i = addValue (return . toTotal $ p1) (toValue p2)

toTotal :: Posting -> Total
toTotal p = Total c dr cr where
  a = B.qty . valueAmount . toValue $ p
  c = case price p of
    Nothing -> B.commodity . B.amount . entry $ p
    (Just pr) -> B.commodity . B.priceAmt $ pr
  (dr, cr) = case B.drCr . entry $ p of
    B.Debit -> (a, Q.zero)
    B.Credit -> (Q.zero, a)
