module Main where

import Control.Monad (guard)
import qualified Control.Monad.Trans.State as St
import qualified Penny.Copper as Cop
import Penny.Copper.DateTime (dateTime)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.List (find)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import qualified Text.Parsec as Parsec


-- | A selloff pertains only to commodities that are in a particular
-- group, where the group is indicated in the account name. The group
-- name is the penultimate sub-account name in the Proceeds
-- account. The group name is often the same as the commodity name,
-- but this need not be true.
newtype GroupName = GroupName { unGroupName :: L.SubAccountName }
  deriving (Eq, Show)

-- | A selloff has a label applying to that particular selloff. This
-- is indicated in the last sub-account name in the Proceeds
-- account. The selloff label is often the date of the selloff, but
-- this need not be true.
newtype SelloffLabel =
  SelloffLabel { unSelloffLabel :: L.SubAccountName }
  deriving (Eq, Show)

-- | The stock being sold off.
newtype Selloff = Selloff { unSelloff :: L.Amount }
  deriving (Eq, Show)

-- | The commodity obtained in exchange for the sold-off commodity.
newtype Currency = Currency { unCurrency :: L.Amount }
  deriving (Eq, Show)

-- | The Proceeds account parsed in from the command line.
newtype ProceedsAcct = ProceedsAcct { unProceedsAcct :: L.Account }
  deriving (Eq, Show)

-- | Gets a ProceedsAcct from an account parsed in from the command
-- line. Fails if the account does not have exactly three
-- sub-accounts.
parseAccount
  :: L.Account
  -> Maybe (ProceedsAcct, GroupName, SelloffLabel)
parseAccount a@(L.Account (_ :| (g : s : []))) =
  Just (ProceedsAcct a, GroupName g, SelloffLabel s)
parseAccount _ = Nothing

-- | Determines the selloff commodity and the currency. The
-- ProceedsAcct must exist, and it must have a balance consisting of
-- exactly two entries, with one debit entry and one credit entry. If
-- any of these conditions fail, this function fails.
cmdtyCurrency
  :: ProceedsAcct
  -> [(L.Account, L.Balance)]
  -> Maybe (Selloff, Currency)
cmdtyCurrency (ProceedsAcct a) ls = do
  (_, bal) <- find (\p -> fst p == a) ls
  let lsBal = mapMaybe toColumn . M.toList . L.unBalance $ bal
      isDebit (_, (L.Column dc _)) = case dc of
        L.Debit -> True
        _ -> False
      toColumn (cty, bl) = case bl of
        L.Zero -> Nothing
        L.NonZero c -> Just (cty, c)
  guard (length lsBal == 2)
  (sellCty, L.Column _ sellQty) <- find isDebit lsBal
  (currCty, L.Column _ currQty) <- find (not . isDebit) lsBal
  return (Selloff (L.Amount sellQty sellCty),
          Currency (L.Amount currQty currCty))


-- | Change a BottomLine to a Column. Fails if the BottomLine is zero.
bottomLineToColumn :: L.BottomLine -> Maybe L.Column
bottomLineToColumn bl = case bl of
  L.Zero -> Nothing
  L.NonZero c -> Just c

basisSub :: L.SubAccountName
basisSub = L.SubAccountName (L.TextNonEmpty 'B' (pack "asis"))

-- | Examines an account to determine whether it is a Basis
-- account and has the selloff label we're interested in.
isBasisAccount :: SelloffLabel -> L.Account -> Bool
isBasisAccount (SelloffLabel l) (L.Account (s1 :| sr))
  | s1 == basisSub = case sr of
      [] -> False
      s:_ -> l == s
  | otherwise = False

newtype PurchaseDate = PurchaseDate { unPurchaseDate :: L.DateTime }
  deriving Show

-- | Examines an Account to see if it has a valid DateTime as the
-- third sub-account, and to ensure that there are no additional
-- sub-accounts. If it is valid, returns the DateTime; fails
-- otherwise.
basisDateTime
  :: Cop.DefaultTimeZone -> L.Account -> Maybe PurchaseDate
basisDateTime dtz (L.Account (_ :| (_ : d : []))) =
  case Parsec.parse (dateTime dtz) "" (L.text d) of
    Left _ -> Nothing
    Right dt -> Just . PurchaseDate $ dt
basisDateTime _ _ = Nothing

-- | The currency quantity for a purchase.
newtype CurrencyQty = CurrencyQty { unCurrencyQty :: L.Qty }
  deriving (Show, Eq)

-- | The stock quantity for a purchase.
newtype StockQty = StockQty { unStockQty :: L.Qty }
  deriving (Show, Eq)

data PurchaseInfo = PurchaseInfo
  { pDateTime :: PurchaseDate
  , pCurrQty :: CurrencyQty
  , pStockQty :: StockQty
  } deriving Show

-- | Gets a quantity from a balance that matches a particular
-- commodity, but only if the commodity's balance has the given DrCr.
quantityFromBal
  :: L.Commodity
  -> M.Map L.Commodity L.BottomLine
  -- ^ Balance map, with zero commodities already removed
  -> L.DrCr
  -> Maybe L.Qty
quantityFromBal cy balMap tgtDc = do
  bl <- M.lookup cy balMap
  (L.Column dc q) <- bottomLineToColumn bl
  guard (dc == tgtDc)
  return q



-- | Gets the Basis information pertaining to a single purchase. Fails
-- if there is not exactly one credit with the selloff commodity and
-- one debit with the currency commodity, or if there are more than
-- two commodities in the balance.
purchaseBasis
  :: Currency
  -> Selloff
  -> L.Balance
  -> Maybe (CurrencyQty, StockQty)
purchaseBasis (Currency cu) (Selloff so) bal = do
  let balMap = L.unBalance . L.removeZeroCommodities $ bal
  cq <- fmap CurrencyQty $ quantityFromBal
        (L.commodity cu) balMap L.Debit
  sq <- fmap StockQty $ quantityFromBal
        (L.commodity so) balMap L.Credit
  guard (M.size balMap == 2)
  return (cq, sq)

-- | How many shares still need to have their basis adjusted? Nothing
-- if no shares remain.
newtype SelloffRemaining
  = SelloffRemaining { unSelloffRemaining :: Maybe L.Qty }
  deriving Show

-- | Information on the adjustment of the basis, to be used in the
-- result transaction. Each BasisAdj represents the postings that will
-- result from adjusting a single purchase transaction.
data BasisAdj = BasisAdj
  { adjSharesSold :: Q.Qty
    -- ^ Number of shares sold (this will be a debit). Typically will
    -- be the number of shares purchased in the corresponding
    -- transaction, but it might be less if nearly all the sold shares
    -- have already been adjusted.

  ,

-- | A stateful computation that computes the selloff information for
-- a single purchase. Stores how many shares remain to be sold off. If
-- there are no shares remaining to be sold off, returns an empty
-- list.
selloffPurchase
  :: GroupName
  -> SelloffLabel
  -> PurchaseInfo
  -> St.State SelloffRemaining (Maybe BasisAdj)
selloffPurchase = undefined

main :: IO ()
main = undefined


