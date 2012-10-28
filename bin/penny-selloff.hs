-- | penny-selloff

-- Steps

-- * In IO monad: read values given on command line.

-- * Parse command line. Fails if command line fails to parse.

-- * If help is requested, show help and exit successfully.

-- * In IO monad: read text of files given on command line. Fails if
-- there is an IO failure.

-- * Parse files given on command line. Fails if files fail to parse.

-- * Calculate balances of all accounts. Remove zero balances. This
-- step never fails.

-- * Find Proceeds account specified on command line. Split it into a
-- group name (the second sub-account) and a selloff label (the third
-- sub-account). Obtain the SelloffStockAmt, which is the debit
-- balance, and the SelloffCurrencyAmt, which is the credit
-- balance. Fails if the Proceeds account does not have exactly three
-- sub-accounts, or if the account does not have a balance with
-- exactly one debit balance and exactly one credit balance. Returns a
-- record with the group name, the selloff label, the selloff currency
-- amount, and the selloff stock amount. (Remember, an amount is a Qty
-- and a Commodity.)

-- * Filter account balances to find all Basis accounts with a
-- matching group name. Only accounts that have Basis as the first
-- sub-account AND a matching group name as the second sub-account are
-- further analyzed; all other accounts are discarded. Returns a list
-- of matching accounts, but only with a list of remaining
-- sub-accounts after the first and second sub-accounts, and the
-- balances of these accounts. This computation does not fail.

-- * For each basis account, parse out the purchase information. This
-- consists of a DateTime, which is the time of the purchase; the Qty
-- of stock purchased; and the Qty of currency paid for the
-- stock. Returns this information in a record. Fails if the basis
-- account does not have exactly two commodities in its balance, or if
-- there is not a credit balance matching the stock commodity, or if
-- there is not a debit balance matching the currency commodity, or if
-- the basis account has more than one remaining sub-account; or if
-- the DateTime cannot be parsed.

-- * For each basis account, compute the basis realization
-- information. First sort the basis accounts with the earliest
-- accounts coming first. Then for each account calculate how many
-- shares to debit and how much currency to credit. Do this in a
-- stateful transforming function that will transform the purchase
-- information into a pair with basis realization information and
-- purchase information. The state contains the number of shares
-- remaining that need to have their basis realized, and the total
-- cost of realized shares.

-- To calculate each basis realization, compare the number of shares
-- purchased with the number still remaining to be realized. If the
-- number purchased is less than or equal to the number remaining to
-- be realized, return a basis realization that realizes all the
-- shares purchased. If the number of shares purchased is more than
-- the number still remaining to be realized, then realize all the
-- shares that still need to be realized, and credit the cost
-- proportionally. If there are no shares remaining to be realized,
-- return no realization information at all.

-- Returns a list of pairs of basis realizations and purchase
-- information; purchases that have no basis realizations are
-- discarded. Also returns the total cost of shares sold. Fails if
-- there are still shares that have not had their basis realized
-- (i.e. the number of shares in the Proceeds account is greater than
-- the number of shares in the selloff group.)

-- * Compute the capital gain or loss. Take the difference between the
-- selloff currency quantity and the cost of shares sold. If the
-- selloff currency quantity is greater, there is a capital
-- gain. Record credits to an Income:Capital Gain account. If the cost
-- of shares sold is greater, there is a capital loss. Record debits
-- to an Expenses:Capital Loss account. To calculate the gain or loss
-- per purchase transaction, use the allocate function in the Qty
-- module. The target total is the total capital gain or loss, and
-- each allocation is the number of shares purchased in each
-- account. Returns a list of quantities (one for each capital gain or
-- loss, which corresponds to each purchase account) and an indication
-- of whether there was a capital gain or loss (this need only be
-- reported once.) Fails if the allocation fails.

-- * Create output transaction. This never fails (if it does, it is a
-- programmer error; just apply error.)

-- * In IO monad: Print output transaction.

module Main where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Lincoln as L

type Err = Ex.Exception Error

data Error
  = ParseFail String
  deriving (Show, Eq)

data ProceedsAcct = ProceedsAcct { unProceedsAcct :: L.Account }
  deriving Show

data ParseResult
  = NeedsHelp
  | ParseResult ProceedsAcct [InputFilename]

parseCommandLine :: [String] -> Err ParseResult
parseCommandLine = undefined
