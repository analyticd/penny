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
import Control.Monad (when)
import qualified Control.Monad.Trans.State as St
import Control.Monad.Trans.Class (lift)
import Data.Foldable (toList)
import Data.List (find)
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (isJust, mapMaybe, catMaybes, fromMaybe)
import Data.Text (pack)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Cabin.Balance.Util as BU
import Penny.Cabin.Options (ShowZeroBalances(..))
import qualified Penny.Copper as Cop
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Account as CA
import qualified Penny.Copper.Transaction as CT
import qualified Penny.Lincoln as L
import qualified Data.Map as M
import qualified System.Console.MultiArg as MA
import Text.Parsec as Parsec

defaultTimeZone :: Cop.DefaultTimeZone
defaultTimeZone = Cop.utcDefault

radGroup :: Cop.RadGroup
radGroup = Cop.periodComma

groupingSpec :: (Cop.GroupingSpec, Cop.GroupingSpec)
groupingSpec = (Cop.GroupAll, Cop.NoGrouping)

type Err = Ex.Exceptional Error

data Error
  = ParseFail MA.Error
  | NoInputArgs
  | ProceedsParseFailed Parsec.ParseError
  | NoInputFiles
  | LedgerParseError Cop.ErrorMsg
  | NoSelloffAccount
  | NotThreeSelloffSubAccounts
  | BadSelloffBalance
  | BadPurchaseBalance
  | BadPurchaseDate Parsec.ParseError
  | NotThreePurchaseSubAccounts [L.SubAccountName]
  | BasisAllocationFailed
  | ZeroCostSharesSold
  | InsufficientSharePurchases
  | NoPurchaseInformation
  | CapitalChangeAllocationFailed
  | SaleDateParseFailed Parsec.ParseError
  deriving Show

data ProceedsAcct = ProceedsAcct { unProceedsAcct :: L.Account }
  deriving Show

newtype InputFilename = InputFilename { unInputFilename :: String }
  deriving (Eq, Show)

loadFile :: InputFilename -> IO (L.Filename, Cop.FileContents)
loadFile (InputFilename fn) = fmap f (TIO.readFile fn)
  where
    f fc = (L.Filename . pack $ fn, Cop.FileContents fc)

data ParseResult
  = NeedsHelp
  | ParseResult ProceedsAcct [InputFilename]

data Flag
  = Help
  | PosArg String
  deriving (Show, Eq)

parseCommandLine :: [String] -> Err ParseResult
parseCommandLine ss =
  let os = [MA.OptSpec ["help"] "h" (MA.NoArg Help)]
  in case MA.parse MA.Intersperse os PosArg ss of
    Ex.Exception e -> Ex.Exception . ParseFail $ e
    Ex.Success g ->
      if isJust . find (== Help) $ g
      then return NeedsHelp
      else do
        let toArg a = case a of
              PosArg s -> Just s
              _ -> Nothing
        x:xs <- case mapMaybe toArg g of
          [] -> Ex.throw NoInputArgs
          r -> return r
        a <- Ex.mapException ProceedsParseFailed
              . Ex.fromEither
              $ Parsec.parse CA.lvl1Account "" (pack x)
        when (null xs) $ Ex.throw NoInputFiles
        return $ ParseResult (ProceedsAcct a) (map InputFilename xs)

help :: String
help = "usage: penny-selloff PROCEEDS_ACCOUNT FILE..."

parseFiles
  :: [(L.Filename, Cop.FileContents)]
  -> Err Cop.Ledger
parseFiles ls = Ex.mapException LedgerParseError
  $ Cop.parse defaultTimeZone radGroup ls

calcBalances :: Cop.Ledger -> [(L.Account, L.Balance)]
calcBalances =
  let toTxn (_, i) = case i of
        Cop.Transaction t -> Just t
        _ -> Nothing
  in BU.flatten
      . BU.balances (ShowZeroBalances False)
      . map (L.Box ())
      . concatMap L.postFam
      . mapMaybe toTxn
      . Cop.unLedger

newtype Group = Group { unGroup :: L.SubAccountName }
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
    _ :| (s2 : s3 : []) -> return (s2, s3)
    _ -> Ex.throw NotThreeSelloffSubAccounts
  (sStock, sCurr) <- selloffStockCurr bal
  date <- fmap SaleDate
          . Ex.mapException SaleDateParseFailed
          . Ex.fromEither
          . Parsec.parse (DT.dateTime defaultTimeZone) ""
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
                    . find ((== dc) . Bal.drCr . snd)
                    $ ps
  (cyStock, (Bal.Column _ qtyStock)) <- findBal L.Debit
  (cyCurr, (Bal.Column _ qtyCurr)) <- findBal L.Credit
  let sellStock = SelloffStock (L.Amount qtyStock cyStock)
      sellCurr = SelloffCurrency (L.Amount qtyCurr cyCurr)
  return (sellStock, sellCurr)


basis :: L.SubAccountName
basis = L.SubAccountName (L.TextNonEmpty 'B' (pack "asis"))

findBasisAccounts
  :: Group
  -> [(L.Account, L.Balance)]
  -> [([L.SubAccountName], L.Balance)]
findBasisAccounts (Group g) = mapMaybe f
  where
    f ((L.Account a), b) = case a of
      s0 :| (s1:s2:ss) -> if (s0 == basis) && (s1 == g)
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
  -> ([L.SubAccountName], L.Balance)
  -> Err PurchaseInfo
purchaseInfo sStock sCurr (ss, bal) = do
  dateSub <- case ss of
    s1:[] -> return s1
    _ -> Ex.throw $ NotThreePurchaseSubAccounts ss
  date <- Ex.mapException BadPurchaseDate
          . Ex.fromEither
          . Parsec.parse (DT.dateTime defaultTimeZone) ""
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
                    . find ((== dc) . Bal.drCr . snd)
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
  = StillToRealize { unStillToRealize :: L.Qty }
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
          let alloced = L.allocate pcq (sq :| [unsoldStockQty])
          basisSold <- case alloced of
            Just (x :| (_ : [])) -> return x
            Nothing -> Ex.throwT BasisAllocationFailed
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
  | NoChange [BasisRealiztn]
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
    Nothing -> return . NoChange . fmap snd $ ls
    Just (qt, gl) -> do
      nePurchs <- Ex.fromMaybe NoPurchaseInformation
                  . NE.nonEmpty $ ls
      let qtys = fmap (unRealizedStockQty . brStockQty . snd)
                 nePurchs
      alloced <- Ex.fromMaybe CapitalChangeAllocationFailed
                 . L.allocate qt $ qtys
      let mkCapChange (p, br) q = (p, br, CapitalChange q)
          r = toList $ NE.zipWith mkCapChange nePurchs alloced
      return $ WithCapitalChanges r gl

memo :: SaleDate -> L.Memo
memo (SaleDate sd) =
  let dTxt = DT.render defaultTimeZone sd
      txt = pack "transaction created by penny-selloff for sale on "
            `X.append` dTxt
  in L.Memo [L.MemoLine (L.TextNonEmpty ' ' txt)]

payee :: L.Payee
payee = L.Payee (L.TextNonEmpty 'R' (pack "ealize gain or loss"))

topLine :: SaleDate -> U.TopLine
topLine sd = U.TopLine (unSaleDate sd) Nothing Nothing (Just payee)
             (memo sd) L.emptyTopLineMeta

basisOffsets
  :: SelloffInfo
  -> BasisRealiztn
  -> (U.Posting, U.Posting)
basisOffsets s p = (po enDr, po enCr)
  where
    ac = L.Account (basis :| [grp, dt])
    grp = unGroup . siGroup $ s
    dt = dateToSubAcct . unSaleDate . siSaleDate $ s
    enDr = L.Entry L.Debit
           (L.Amount (unRealizedStockQty . brStockQty $ p)
              (L.commodity . unSelloffStock . siStock $ s))
    enCr = L.Entry L.Credit
           (L.Amount (unRealizedCurrencyQty . brCurrencyQty $ p)
              (L.commodity . unSelloffCurrency . siCurrency $ s))
    meta = L.PostingMeta Nothing (Just fmt) Nothing Nothing
    fmt = L.Format L.CommodityOnLeft L.SpaceBetween
    po en = U.Posting Nothing Nothing Nothing ac (L.Tags [])
            (Just en) (L.Memo []) meta

dateToSubAcct :: L.DateTime -> L.SubAccountName
dateToSubAcct dt = L.SubAccountName (L.TextNonEmpty f r)
  where
    dtTxt = DT.render defaultTimeZone dt
    (f, r) = case X.uncons dtTxt of
      Just x -> x
      Nothing -> error "dateToSubAcct: date rendered empty"

income :: L.SubAccountName
income = L.SubAccountName (L.TextNonEmpty 'I' (pack "ncome"))

capGain :: L.SubAccountName
capGain = L.SubAccountName (L.TextNonEmpty 'C' (pack "apital Gain"))

expense :: L.SubAccountName
expense = L.SubAccountName (L.TextNonEmpty 'E' (pack "xpenses"))

capLoss :: L.SubAccountName
capLoss = L.SubAccountName (L.TextNonEmpty 'C' (pack "apital Loss"))

capChangeAcct
  :: GainOrLoss
  -> SelloffInfo
  -> PurchaseInfo
  -> L.Account
capChangeAcct gl si p = L.Account $ case gl of
  Gain -> income :| [capGain, grp, sd, pd]
  Loss -> expense :| [capLoss, grp, sd, pd]
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
  -> U.Posting
capChangePstg si gl cc p =
  U.Posting Nothing Nothing Nothing ac (L.Tags []) (Just en)
    (L.Memo []) meta
  where
    meta = L.PostingMeta Nothing (Just fmt) Nothing Nothing
    fmt = L.Format L.CommodityOnLeft L.SpaceBetween
    ac = capChangeAcct gl si p
    en = capChangeEntry gl (siCurrency si) cc

proceeds :: L.SubAccountName
proceeds = L.SubAccountName (L.TextNonEmpty 'P' (pack "roceeds"))

proceedsPstgs
  :: SelloffInfo
  -> (U.Posting, U.Posting)
proceedsPstgs si = (po dr, po cr)
  where
    po en = U.Posting Nothing Nothing Nothing ac (L.Tags [])
      (Just en) (L.Memo []) meta
    meta = L.PostingMeta Nothing (Just fmt) Nothing Nothing
    ac = L.Account (proceeds :| [gr, dt])
    gr = unGroup . siGroup $ si
    dt = dateToSubAcct . unSaleDate . siSaleDate $ si
    dr = L.Entry L.Debit (unSelloffCurrency . siCurrency $ si)
    cr = L.Entry L.Credit (unSelloffStock . siStock $ si)
    fmt = L.Format L.CommodityOnLeft L.SpaceBetween

mkTxn
  :: SelloffInfo
  -> WithCapitalChanges
  -> L.Transaction
mkTxn si wcc = Ex.resolve err exTxn
  where
    err = const $ error "mkTxn: making transaction failed"
    exTxn = L.transaction $ L.Family tl p1 p2 ps
    tl = topLine . siSaleDate $ si
    (p1, p2) = proceedsPstgs si
    ps = case wcc of
      NoChange infoRlzns -> concatMap f infoRlzns
        where
          f br =
            let (b1, b2) = basisOffsets si br
            in [b1, b2]
      WithCapitalChanges trips gl -> concatMap f trips
        where
          f (p, br, cc) = [b1, b2, c]
            where
              (b1, b2) = basisOffsets si br
              c = capChangePstg si gl cc p

makeOutput
  :: ProceedsAcct
  -> [(L.Filename, Cop.FileContents)]
  -> Err X.Text
makeOutput pa ps = do
  bals <- fmap calcBalances $ parseFiles ps
  si <- selloffInfo pa bals
  let basisAccts = findBasisAccounts (siGroup si) bals
  purchInfos <- mapM (purchaseInfo (siStock si) (siCurrency si))
                basisAccts
  (purchBases, css) <- realizeBases (siStock si) purchInfos
  wcc <- capitalChange css (siCurrency si) purchBases
  return
    . (`X.snoc` '\n')
    . fromMaybe (error "makeOutput: transaction did not render")
    . CT.render defaultTimeZone groupingSpec radGroup
    . mkTxn si
    $ wcc


main :: IO ()
main =
  MA.getArgs
  >>= Ex.switch (error . show) handleParseResult . parseCommandLine


handleParseResult :: ParseResult -> IO ()
handleParseResult pr = case pr of
  NeedsHelp -> putStrLn help
  ParseResult pa fs ->
    mapM loadFile fs
    >>= Ex.switch (error . show) TIO.putStr . makeOutput pa
