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

module Main (main) where

import Control.Arrow (first)
import Control.Applicative ((<$>), (<*>), pure)
import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Monad (when)
import qualified Control.Monad.Trans.State as St
import Control.Monad.Trans.Class (lift)
import Data.List (find)
import Data.Maybe (isJust, mapMaybe, catMaybes, fromMaybe)
import Data.Text (pack)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Steel.Sums as S
import qualified Penny.Cabin.Balance.Util as BU
import Penny.Cabin.Options (ShowZeroBalances(..))
import qualified Penny.Copper as Cop
import qualified Penny.Copper.Parsec as CP
import qualified Penny.Copper.Render as CR
import qualified Penny.Liberty as Ly
import qualified Penny.Lincoln as L
import qualified Data.Map as M
import qualified System.Console.MultiArg as MA
import qualified Text.Parsec as Parsec
import qualified Paths_penny_bin as PPB

groupingSpec :: CR.GroupSpecs
groupingSpec = CR.GroupSpecs CR.NoGrouping CR.NoGrouping

type Err = Ex.Exceptional Error

data Error
  = ParseFail MA.Error
  | NoInputArgs
  | ProceedsParseFailed Parsec.ParseError
  | NoSelloffAccount
  | NotThreeSelloffSubAccounts
  | BadSelloffBalance
  | BadPurchaseBalance
  | BadPurchaseDate Parsec.ParseError
  | NotThreePurchaseSubAccounts [L.SubAccount]
  | BasisAllocationFailed
  | ZeroCostSharesSold
  | InsufficientSharePurchases
  | NoPurchaseInformation
  | SaleDateParseFailed Parsec.ParseError
  deriving Show

data ProceedsAcct = ProceedsAcct { _unProceedsAcct :: L.Account }
  deriving Show

newtype InputFilename = InputFilename { _unInputFilename :: String }
  deriving (Eq, Show)

data ParseResult = ParseResult ProceedsAcct [Cop.LedgerItem]

parseCommandLine :: IO ParseResult
parseCommandLine = do
  as <- MA.simpleHelpVersion help (Ly.version PPB.version)
        [] MA.Intersperse return
  x:xs <- case as of
    [] -> fail (show NoInputArgs)
    r -> return r
  a <- Ex.switch (fail . show . ProceedsParseFailed) return
       . Ex.fromEither
       $ Parsec.parse CP.lvl1Acct "" (pack x)
  l <- Cop.open xs
  return $ ParseResult (ProceedsAcct a) l


help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " PROCEEDS_ACCOUNT FILE..."
  , "calculate capital gains and losses from commodity sales."
  , "Options:"
  , "  -h, --help - show this help and exit."
  , "  --version  - show version and exit."
  ]

calcBalances :: [Cop.LedgerItem] -> [(L.Account, L.Balance)]
calcBalances =
  BU.flatten
  . BU.balances (ShowZeroBalances False)
  . map (\p -> ((), p))
  . concatMap L.transactionToPostings
  . mapMaybe (S.caseS4 Just (const Nothing) (const Nothing)
              (const Nothing))

newtype Group = Group { unGroup :: L.SubAccount }
  deriving (Show, Eq)

newtype SaleDate = SaleDate { unSaleDate :: L.DateTime }
  deriving (Show, Eq)

newtype SelloffStock = SelloffStock { unSelloffStock :: L.Amount }
  deriving (Show, Eq)

newtype SelloffCurrency
  = SelloffCurrency { unSelloffCurrency :: L.Amount }
  deriving (Show, Eq)

data SelloffInfo = SelloffInfo
  { siGroup    :: Group
  , siSaleDate    :: SaleDate
  , siStock    :: SelloffStock
  , siCurrency :: SelloffCurrency
  } deriving Show

selloffInfo
  :: ProceedsAcct -> [(L.Account, L.Balance)] -> Err SelloffInfo
selloffInfo (ProceedsAcct pa) bals = do
  bal <- fmap snd
          . Ex.fromMaybe NoSelloffAccount
          . find ((== pa) . fst)
          $ bals
  (g, d) <- case L.unAccount pa of
    _ : s2 : s3 : [] -> return (s2, s3)
    _ -> Ex.throw NotThreeSelloffSubAccounts
  (sStock, sCurr) <- selloffStockCurr bal
  date <- fmap SaleDate
          . Ex.mapException SaleDateParseFailed
          . Ex.fromEither
          . Parsec.parse CP.dateTime ""
          $ (L.text d)
  return $ SelloffInfo (Group g) date sStock sCurr

selloffStockCurr :: L.Balance -> Err (SelloffStock, SelloffCurrency)
selloffStockCurr bal = do
  let m = L.unBalance bal
  when (M.size m /= 2) $ Ex.throw BadSelloffBalance
  let toPair (cy, bl) = case bl of
        Bal.Zero -> Nothing
        Bal.NonZero col -> Just (cy, col)
      ps = mapMaybe toPair . M.toList $ m
      findBal dc = Ex.fromMaybe BadSelloffBalance
                    . find ((== dc) . Bal.colDrCr . snd)
                    $ ps
  (cyStock, (Bal.Column _ qtyStock)) <- findBal L.Debit
  (cyCurr, (Bal.Column _ qtyCurr)) <- findBal L.Credit
  let sellStock = SelloffStock
        (L.Amount qtyStock cyStock)
      sellCurr = SelloffCurrency
        (L.Amount qtyCurr cyCurr)
  return (sellStock, sellCurr)


basis :: L.SubAccount
basis = L.SubAccount . pack $ "Basis"

findBasisAccounts
  :: Group
  -> [(L.Account, L.Balance)]
  -> [([L.SubAccount], L.Balance)]
findBasisAccounts (Group g) = mapMaybe f
  where
    f ((L.Account a), b) = case a of
      s0 : s1 : s2 : ss -> if (s0 == basis) && (s1 == g)
                           then Just (s2:ss, b) else Nothing
      _ -> Nothing


data PurchaseDate = PurchaseDate { unPurchaseDate :: L.DateTime }
  deriving Show

data PurchaseStockQty
  = PurchaseStockQty { unPurchaseStockQty :: L.Qty }
  deriving (Eq, Show)

data PurchaseCurrencyQty
  = PurchaseCurrencyQty { unPurchaseCurrencyQty :: L.Qty }
  deriving (Eq, Show)

data PurchaseInfo = PurchaseInfo
  { piDate :: PurchaseDate
  , piStockQty :: PurchaseStockQty
  , piCurrencyQty :: PurchaseCurrencyQty
  } deriving Show

purchaseInfo
  :: SelloffStock
  -> SelloffCurrency
  -> ([L.SubAccount], L.Balance)
  -> Err PurchaseInfo
purchaseInfo sStock sCurr (ss, bal) = do
  dateSub <- case ss of
    s1:[] -> return s1
    _ -> Ex.throw $ NotThreePurchaseSubAccounts ss
  date <- Ex.mapException BadPurchaseDate
          . Ex.fromEither
          . Parsec.parse CP.dateTime ""
          . L.text
          $ dateSub
  (stockQty, currQty) <- purchaseQtys sStock sCurr bal
  return $ PurchaseInfo (PurchaseDate date) stockQty currQty

purchaseQtys
  :: SelloffStock
  -> SelloffCurrency
  -> L.Balance
  -> Err (PurchaseStockQty, PurchaseCurrencyQty)
purchaseQtys (SelloffStock sStock) (SelloffCurrency sCurr) bal = do
  let m = L.unBalance bal
  when (M.size m /= 2) $ Ex.throw BadPurchaseBalance
  let toPair (cy, bl) = case bl of
        Bal.Zero -> Nothing
        Bal.NonZero col -> Just (cy, col)
      ps = mapMaybe toPair . M.toList $ m
      findBal dc = Ex.fromMaybe BadPurchaseBalance
                    . find ((== dc) . Bal.colDrCr . snd)
                    $ ps
  (cyStock, (Bal.Column _ qtyStock)) <- findBal L.Credit
  (cyCurr, (Bal.Column _ qtyCurr)) <- findBal L.Debit
  when (cyStock /= L.commodity sStock) $ Ex.throw BadPurchaseBalance
  when (cyCurr /= L.commodity sCurr) $ Ex.throw BadPurchaseBalance
  return (PurchaseStockQty qtyStock, PurchaseCurrencyQty qtyCurr)


newtype RealizedStockQty
  = RealizedStockQty { unRealizedStockQty :: L.Qty }
  deriving (Eq, Show)

newtype RealizedCurrencyQty
  = RealizedCurrencyQty { unRealizedCurrencyQty :: L.Qty }
  deriving (Eq, Show)

newtype CostSharesSold
  = CostSharesSold { unCostSharesSold :: L.Qty }
  deriving (Eq, Show)

newtype StillToRealize
  = StillToRealize { _unStillToRealize :: L.Qty }
  deriving (Eq, Show)

data BasisRealiztn = BasisRealiztn
  { brStockQty :: RealizedStockQty
  , brCurrencyQty :: RealizedCurrencyQty
  } deriving Show

-- | Realize an individual purchase account's basis. Fails if the
-- basis cannot be allocated.
stRealizeBasis
  :: PurchaseInfo
  -> Ex.ExceptionalT Error
     (St.State (Maybe CostSharesSold, Maybe StillToRealize))
     (Maybe (PurchaseInfo, BasisRealiztn))
stRealizeBasis p = do
  mayTr <- lift $ St.gets snd
  case mayTr of
    Nothing -> return Nothing
    Just (StillToRealize tr) -> do
      let sq = unPurchaseStockQty . piStockQty $ p
          pcq = unPurchaseCurrencyQty . piCurrencyQty $ p
      mayCss <- lift $ St.gets fst
      case L.difference tr sq of

        L.LeftBiggerBy tr' -> do
          let br = BasisRealiztn (RealizedStockQty sq)
                   (RealizedCurrencyQty pcq)
              css' = case mayCss of
                Nothing -> CostSharesSold pcq
                Just (CostSharesSold css) ->
                  CostSharesSold (L.add pcq css)
          lift $ St.put (Just css', Just (StillToRealize tr'))
          return (Just (p, br))

        L.RightBiggerBy unsoldStockQty -> do
          let alloced = L.allocate pcq (sq, [unsoldStockQty])
              basisSold = case alloced of
                (x, (_ : [])) -> x
                _ -> error "stRealizeBasis: error"
          let css' = case mayCss of
                Nothing -> CostSharesSold basisSold
                Just (CostSharesSold css) ->
                  CostSharesSold (L.add basisSold css)
              br = BasisRealiztn (RealizedStockQty tr)
                 (RealizedCurrencyQty basisSold)
          lift $ St.put (Just css', Nothing)
          return (Just (p, br))

        L.Equal -> do
          let br = BasisRealiztn (RealizedStockQty sq)
                (RealizedCurrencyQty pcq)
              css' = case mayCss of
                Nothing -> CostSharesSold pcq
                Just (CostSharesSold css) ->
                  CostSharesSold (L.add css pcq)
          lift $ St.put (Just css', Nothing)
          return (Just (p, br))

realizeBases
  :: SelloffStock
  -> [PurchaseInfo]
  -> Err ([(PurchaseInfo, BasisRealiztn)], CostSharesSold)
realizeBases sellStck ps = do
  let stReal = Just . StillToRealize . L.qty
               . unSelloffStock $ sellStck
      (exRs, (mayCss, mayTr)) = St.runState
        (Ex.runExceptionalT (mapM stRealizeBasis ps))
        (Nothing, stReal)
  rs <- exRs
  when (isJust mayTr) $ Ex.throw InsufficientSharePurchases
  css <- Ex.fromMaybe ZeroCostSharesSold mayCss
  return (catMaybes rs, css)

newtype CapitalChange = CapitalChange { unCapitalChange :: L.Qty }
  deriving Show

data WithCapitalChanges
  = WithCapitalChanges [(PurchaseInfo, BasisRealiztn, CapitalChange)]
      GainOrLoss
  | NoChange [(PurchaseInfo, BasisRealiztn)]
  deriving Show

data GainOrLoss = Gain | Loss deriving (Eq, Show)

capitalChange
  :: CostSharesSold
  -> SelloffCurrency
  -> [(PurchaseInfo, BasisRealiztn)]
  -> Err WithCapitalChanges
capitalChange css sc ls =
  let sellCurrQty = L.qty . unSelloffCurrency $ sc
      costQty = unCostSharesSold css
      mayGainLoss =
        case L.difference sellCurrQty costQty of
          L.LeftBiggerBy q -> Just (q, Gain)
          L.RightBiggerBy q -> Just (q, Loss)
          L.Equal -> Nothing
  in case mayGainLoss of
    Nothing -> return . NoChange $ ls
    Just (qt, gl) -> do
      nePurchs <- Ex.fromMaybe NoPurchaseInformation
                  . uncons $ ls
      let qtys = mapNE (unPurchaseCurrencyQty . piCurrencyQty . fst)
                 nePurchs
          alloced = L.allocate qt qtys
      let mkCapChange (p, br) q = (p, br, CapitalChange q)
          r = flattenNE $ zipNE mkCapChange nePurchs alloced
      return $ WithCapitalChanges r gl

mapNE :: (a -> b) -> (a, [a]) -> (b, [b])
mapNE f (a, as) = (f a, map f as)

flattenNE :: (a, [a]) -> [a]
flattenNE (a, as) = a:as

uncons :: [a] -> Maybe (a, [a])
uncons as = case as of
  [] -> Nothing
  x:xs -> Just (x, xs)

zipNE :: (a -> b -> c) -> (a, [a]) -> (b, [b]) -> (c, [c])
zipNE f (a, as) (b, bs) = (f a b, zipWith f as bs)

memo :: SaleDate -> L.Memo
memo (SaleDate sd) =
  let dTxt = CR.dateTime sd
      txt = pack "transaction created by penny-selloff for sale on "
            `X.append` dTxt
  in L.Memo [txt]

payee :: L.Payee
payee = L.Payee . pack $ "Realize gain or loss"

topLine :: SaleDate -> L.TopLineData
topLine sd =
  let core = (L.emptyTopLineCore (unSaleDate sd))
             { L.tPayee = Just payee
             , L.tMemo = Just . memo $ sd
             }
  in L.TopLineData { L.tlCore = core
                   , L.tlFileMeta = Nothing
                   , L.tlGlobal = Nothing }

basisOffsets
  :: SelloffInfo
  -> PurchaseDate
  -> BasisRealiztn
  -> ((L.Entry, L.PostingData), (L.Entry, L.PostingData))
basisOffsets s pd p = (po enDr, po enCr)
  where
    ac = L.Account [basis, grp, dt]
    grp = unGroup . siGroup $ s
    dt = dateToSubAcct . unPurchaseDate $ pd
    enDr = L.Entry L.Debit
           (L.Amount (unRealizedStockQty . brStockQty $ p)
              (L.commodity . unSelloffStock . siStock $ s))
    enCr = L.Entry L.Credit
           (L.Amount (unRealizedCurrencyQty . brCurrencyQty $ p)
              (L.commodity . unSelloffCurrency . siCurrency $ s))
    po en = (en, emptyPostingData ac)

emptyPostingData :: L.Account -> L.PostingData
emptyPostingData a =
  let core = (L.emptyPostingCore a)
        { L.pSide = Just L.CommodityOnLeft
        , L.pSpaceBetween = Just L.SpaceBetween
        }
  in L.PostingData { L.pdCore = core
                   , L.pdFileMeta = Nothing
                   , L.pdGlobal = Nothing
                   }

dateToSubAcct :: L.DateTime -> L.SubAccount
dateToSubAcct = L.SubAccount . CR.dateTime

income :: L.SubAccount
income = L.SubAccount .  pack $ "Income"

capGain :: L.SubAccount
capGain = L.SubAccount . pack $ "Capital Gain"

expense :: L.SubAccount
expense = L.SubAccount . pack $ "Expenses"

capLoss :: L.SubAccount
capLoss = L.SubAccount . pack $ "Capital Loss"

capChangeAcct
  :: GainOrLoss
  -> SelloffInfo
  -> PurchaseInfo
  -> L.Account
capChangeAcct gl si p = L.Account $ case gl of
  Gain -> [income, capGain, grp, sd, pd]
  Loss -> [expense, capLoss, grp, sd, pd]
  where
    grp = unGroup . siGroup $ si
    sd = dateToSubAcct . unSaleDate . siSaleDate $ si
    pd = dateToSubAcct . unPurchaseDate . piDate $ p

capChangeEntry
  :: GainOrLoss
  -> SelloffCurrency
  -> CapitalChange
  -> L.Entry
capChangeEntry gl sc cc = L.Entry dc (L.Amount qt cy)
  where
    dc = case gl of
      Gain -> L.Credit
      Loss -> L.Debit
    cy = L.commodity . unSelloffCurrency $ sc
    qt = unCapitalChange cc

capChangePstg
  :: SelloffInfo
  -> GainOrLoss
  -> CapitalChange
  -> PurchaseInfo
  -> (L.Entry, L.PostingData)
capChangePstg si gl cc p = (en, emptyPostingData ac)
  where
    ac = capChangeAcct gl si p
    en = capChangeEntry gl (siCurrency si) cc

proceeds :: L.SubAccount
proceeds = L.SubAccount . pack $ "Proceeds"

proceedsPstgs
  :: SelloffInfo
  -> ((L.Entry, L.PostingData), (L.Entry, L.PostingData))
proceedsPstgs si = (po dr, po cr)
  where
    po en = (en, emptyPostingData ac)
    ac = L.Account [proceeds, gr, dt]
    gr = unGroup . siGroup $ si
    dt = dateToSubAcct . unSaleDate . siSaleDate $ si
    dr = L.Entry L.Debit (unSelloffCurrency . siCurrency $ si)
    cr = L.Entry L.Credit (unSelloffStock . siStock $ si)


mkTxn
  :: SelloffInfo
  -> WithCapitalChanges
  -> L.Transaction
mkTxn si wcc = fromMaybe err exTxn
  where
    err = error "mkTxn: making transaction failed"
    exTxn = (\topl es -> L.Transaction (topl, es))
            <$> pure tl <*> L.ents entInputs
    tl = topLine . siSaleDate $ si
    (p1, p2) = proceedsPstgs si
    ps = case wcc of
      NoChange infoRlzns -> concatMap f infoRlzns
        where
          f (p, br) =
            let (b1, b2) = basisOffsets si (piDate p) br
            in [b1, b2]
      WithCapitalChanges trips gl -> concatMap f trips
        where
          f (p, br, cc) = [b1, b2, c]
            where
              (b1, b2) = basisOffsets si (piDate p) br
              c = capChangePstg si gl cc p
    entInputs = map (first Just) (p1:p2:ps)

makeOutput
  :: ProceedsAcct
  -> [Cop.LedgerItem]
  -> Err X.Text
makeOutput pa ldgr = do
  let bals = calcBalances ldgr
  si <- selloffInfo pa bals
  let basisAccts = findBasisAccounts (siGroup si) bals
  purchInfos <- mapM (purchaseInfo (siStock si) (siCurrency si))
                basisAccts
  (purchBases, css) <- realizeBases (siStock si) purchInfos
  wcc <- capitalChange css (siCurrency si) purchBases
  return
    . (`X.snoc` '\n')
    . fromMaybe (error "makeOutput: transaction did not render")
    . CR.transaction groupingSpec
    . (\t -> let (tl, es) = L.unTransaction t
             in (L.tlCore tl, fmap L.pdCore es))
    . mkTxn si
    $ wcc


main :: IO ()
main = parseCommandLine >>= handleParseResult


handleParseResult :: ParseResult -> IO ()
handleParseResult (ParseResult pa ldgr) =
  Ex.switch (error . show) TIO.putStr . makeOutput pa $ ldgr
