module Penny.Posting where

import qualified Penny.Bits as B
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Price as Price
import qualified Penny.Bits.Commodity as Commodity
import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Entry as E

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
          , entry :: E.Entry
          , price :: Maybe Price.Price
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
data Total = Total { totCommodity :: Commodity.Commodity
                   , debits :: Q.Qty
                   , credits :: Q.Qty }

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
    if totCommodity t /= (A.commodity . valueAmount $ v)
    then Exception $ DifferentCommodityError t v
    else let c = totCommodity t
             q = A.qty . valueAmount $ v
             (dr, cr) = case valueDrCr v of
               E.Debit -> ((debits t) `add` q, credits t)
               E.Credit -> (debits t, (credits t) `add` q)
         in Success $ Total c dr cr

toValue :: Posting -> Value
toValue p = Value dc a where
  dc = E.drCr . entry $ p
  a = case price p of
    Nothing -> B.amount . entry $ p
    (Just pr) -> case Price.priceDesc pr of
      (Price.TotalPrice _) -> Price.priceAmt pr
      Price.UnitPrice -> let
        q = (A.qty . B.amount . entry $ p)
            `mult` (A.qty . Price.priceAmt $ pr)
        c = A.commodity . Price.priceAmt $ pr
        in A.Amount q c

totalPostings :: Posting -> Posting -> [Posting] -> Exceptional Error Total
totalPostings p1 p2 ps = foldl addValue i (map toValue ps) where
  i = addValue (return . toTotal $ p1) (toValue p2)

toTotal :: Posting -> Total
toTotal p = Total c dr cr where
  a = A.qty . valueAmount . toValue $ p
  c = case price p of
    Nothing -> A.commodity . B.amount . entry $ p
    (Just pr) -> A.commodity . Price.priceAmt $ pr
  (dr, cr) = case B.drCr . entry $ p of
    B.Debit -> (a, Q.zero)
    B.Credit -> (Q.zero, a)
