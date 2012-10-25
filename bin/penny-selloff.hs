module Main where

import Control.Monad (guard)
import qualified Penny.Copper as Cop
import Penny.Copper.DateTime (dateTime)
import qualified Penny.Lincoln as L
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

-- | Examines an Account to see if it has a valid DateTime as the
-- third sub-account, and to ensure that there are no additional
-- sub-accounts. If it is valid, returns the DateTime; fails
-- otherwise.
basisDateTime :: Cop.DefaultTimeZone -> L.Account -> Maybe L.DateTime
basisDateTime dtz (L.Account (_ :| (_ : d : []))) =
  case Parsec.parse (dateTime dtz) "" (L.text d) of
    Left _ -> Nothing
    Right dt -> Just dt
basisDateTime _ _ = Nothing

-- | Information on a single Basis corresponding to an asset purchase.
data Basis = Basis
  { basisCurrencyQty :: L.Qty
  , basisStockQty :: L.Qty
  } deriving Show

-- | Gets the Basis information pertaining to a single

main :: IO ()
main = undefined


