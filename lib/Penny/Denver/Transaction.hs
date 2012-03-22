module Penny.Denver.Transaction (lincolnize, Raw(Raw)) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (catMaybes)
import qualified Data.Text as X
import qualified Data.Time as T
import Data.Traversable (traverse)
import qualified Data.Foldable as Foldable
import qualified Penny.Denver.Common as C
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Builders as Bd
import qualified Penny.Lincoln.Meta as M
import Penny.Lincoln.Family (orphans, adopt, divorceWith)
import qualified Penny.Lincoln.Family.Family as F
import qualified Penny.Lincoln.Family.Siblings as S
import qualified Penny.Lincoln.TextNonEmpty as TNE
import qualified Penny.Lincoln.Transaction as LT
import qualified Penny.Denver.Posting as P
import qualified Penny.Denver.TopLine as TL
import qualified Penny.Lincoln.Transaction.Unverified as U


-- | Takes a C++ Ledger transaction and transforms it to a Lincoln
-- transaction. All data passed in through the Raw is preserved,
-- either through the actual transaction or through the metadata.
--
-- Postings with no Record result in one Lincolnized Posting. Postings
-- with a Record but without a price result in one Lincolnized
-- Posting. Postings with a Record and with a Price result in three
-- Lincolnized postings and in one or more Prices. For details, see
-- the comments at the bottom of this source file.
--
-- This computation will fail if:
--
-- * Lincoln cannot make a balanced transaction. Unlike C++ Ledger
-- there is no sort of fudge factor for transactions that have prices.
--
-- * A posting has a price with a commodity that is the same as the
-- commodity on the Amount.
lincolnize ::
  Raw
  -> Maybe (Boxes.TransactionBox, [Boxes.PriceBox])
lincolnize r = do
  (lincolnizedTxn, ps) <- lincolnizeTxn r
  let unv = unvWithMeta lincolnizedTxn
  tBox <- transactionBox unv
  let pBoxes = map priceBox ps
  return (tBox, pBoxes)

priceBox :: PriceWithFormat -> Boxes.PriceBox
priceBox (PriceWithFormat p fmt) =
  Boxes.PriceBox p (Just (M.PriceMeta Nothing (Just fmt)))

transactionBox :: UnverifiedWithMeta
                  -> Maybe Boxes.TransactionBox
transactionBox (UnverifiedWithMeta unv meta) = do
  txn <- Ex.toMaybe $ LT.transaction unv
  return $ Boxes.transactionBox txn (Just meta)

data UnverifiedWithMeta =
  UnverifiedWithMeta
  (F.Family U.TopLine U.Posting)
  M.TransactionMeta
  deriving (Show, Eq)

unvWithMeta :: LincolnizedTxn -> UnverifiedWithMeta
unvWithMeta (LincolnizedTxn fam) = let
  (unv, m) = divorceWith splitTopLine splitLincolnized fam
  meta = M.TransactionMeta m
  in UnverifiedWithMeta unv meta


-- | A Lincolnized transaction. Is not yet a true Penny transaction as
-- it still must be processed through
-- Penny.Lincoln.Transaction.transaction.
newtype LincolnizedTxn =
  LincolnizedTxn (F.Family U.TopLine Lincolnized)
  deriving (Eq, Show)

lincolnizeTxn ::
  Raw
  -> Maybe (LincolnizedTxn, [PriceWithFormat])
lincolnizeTxn (Raw fa@(F.Family tl _ _ _)) = let
  tl' = lincolnizeTopLine tl
  ors = orphans fa
  d = TL.day tl
  in do
    lincolnWithMayPrices <- traverse (lincolnizePstg d) ors
    let (sibs, ps) = separatePrices lincolnWithMayPrices
        fam' = adopt tl' sibs
        lincolnized = LincolnizedTxn fam'
    return (lincolnized, ps)

-- | Converts a NonEmpty list of Lincolnized and maybe Prices to a
-- NonEmpty list of Lincolnized and a list of Prices.
separatePrices ::
  S.Siblings (NE.NonEmpty Lincolnized, Maybe PriceWithFormat)
  -> (S.Siblings Lincolnized, [PriceWithFormat])
separatePrices s = (lincs, ps) where
  split = (fmap fst s, fmap snd s)
  lincs = S.collapse (fst split)
  ps = catMaybes . Foldable.toList . snd $ split

-- | Splits an unverified TopLine into an unverified TopLine and an
-- empty TopLineMeta.
splitTopLine :: U.TopLine -> (U.TopLine, M.TopLineMeta)
splitTopLine tl = (tl, M.TopLineMeta Nothing Nothing Nothing)

-- | Splits a Lincolnized into an unverified Posting and a
-- PostingMeta.
splitLincolnized :: Lincolnized -> (U.Posting, M.PostingMeta)
splitLincolnized l = case l of
  WithEntry (PostingWithFormat p fmt) -> let
    meta = M.PostingMeta Nothing (Just fmt)
    in (p, meta)
  NoEntry p -> (p, M.PostingMeta Nothing Nothing)

-- | A transaction as read in from a Ledger file. The parser supplies
-- this.
newtype Raw = Raw (F.Family TL.TopLine P.Posting)
            deriving (Eq, Show)

data PostingWithFormat = PostingWithFormat
                         U.Posting
                         M.Format
                       deriving (Eq, Show)

data Lincolnized =
  WithEntry PostingWithFormat
  | NoEntry U.Posting
  deriving (Show, Eq)

data PriceWithFormat = PriceWithFormat
                       B.PricePoint
                       M.Format
                       deriving (Eq, Show)

lincolnizeTopLine :: TL.TopLine -> U.TopLine
lincolnizeTopLine (TL.TopLine d c n p) =
  U.TopLine dt fl n p (B.Memo []) where
    dt = lincolnizeDay d
    fl = lincolnizeCleared c

lincolnizePstg ::
  T.Day
  -> P.Posting
  -> Maybe (NE.NonEmpty Lincolnized, Maybe PriceWithFormat)
lincolnizePstg d (P.Posting c a r m) = case r of
  Nothing -> let
    l = lincolnizeNoEntry c a m
    in Just ((NoEntry l) :| [], Nothing)
  Just rec -> let e = P.entry rec in
    case P.price rec of
      Nothing -> let
        l = lincolnizeEntryOnly c a e m
        in Just ((WithEntry l) :| [], Nothing)
      Just p -> do
        (Valued p1 p2 p3 vp) <- lincolnizeValued d c a e p m
        let ls = fmap WithEntry (p1 :| [p2, p3])
        return (ls, Just vp)

lincolnizeCleared :: C.Cleared -> Maybe B.Flag
lincolnizeCleared c = case c of
  C.Cleared -> Just (B.Flag (TNE.TextNonEmpty '*' X.empty))
  C.NotCleared -> Nothing

data Valued = Valued {
  _original :: PostingWithFormat
  , _offset :: PostingWithFormat
  , _priced :: PostingWithFormat
  , _vPriceFormat :: PriceWithFormat
  } deriving (Eq, Show)

lincolnizeNoEntry ::
  C.Cleared
  -> B.Account
  -> Maybe B.MemoLine
  -> U.Posting
lincolnizeNoEntry c a mayMemo =
  U.Posting Nothing Nothing fl a (B.Tags []) Nothing memo where
    fl = lincolnizeCleared c
    memo = lincolnizeMemo mayMemo

lincolnizeEntryOnly ::
  C.Cleared
  -> B.Account
  -> P.Entry
  -> Maybe B.MemoLine
  -> PostingWithFormat
lincolnizeEntryOnly c a e mayMemo = PostingWithFormat p fmt where
  fmt = P.entryFormat e
  p = U.Posting Nothing Nothing fl a (B.Tags []) (Just e') memo
  fl = lincolnizeCleared c
  e' = B.Entry (lincolnizeSign (P.sign e)) amt
  amt = lincolnizeAmount (P.amount e)
  memo = lincolnizeMemo mayMemo

lincolnizeValued ::
  T.Day
  -> C.Cleared
  -> B.Account
  -> P.Entry
  -> P.Price
  -> Maybe B.MemoLine
  -> Maybe Valued
lincolnizeValued d c a e p m = let
  orig = lincolnizeOriginal c a e m
  off = lincolnizeOffset e
  pri = lincolnizePriced e p
  in do
    vPri <- makePrice d e p
    return $ Valued orig off pri vPri

lincolnizeOriginal ::
  C.Cleared
  -> B.Account
  -> P.Entry
  -> Maybe B.MemoLine
  -> PostingWithFormat
lincolnizeOriginal c a e mayMemo = PostingWithFormat p fmt where
  fmt = P.entryFormat e
  p = U.Posting Nothing Nothing fl a (B.Tags []) (Just e') memo
  fl = lincolnizeCleared c
  memo = lincolnizeMemo mayMemo
  e' = B.Entry (lincolnizeSign (P.sign e)) amt
  amt = lincolnizeAmount (P.amount e)

lincolnizeOffset :: P.Entry -> PostingWithFormat
lincolnizeOffset e = PostingWithFormat p fmt where
  fmt = P.entryFormat e
  p = U.Posting Nothing Nothing Nothing tradingAcct (B.Tags [])
      (Just e') (B.Memo [])
  e' = B.Entry dc amt
  dc = B.opposite (lincolnizeSign (P.sign e))
  amt = lincolnizeAmount (P.amount e)

lincolnizePriced :: P.Entry -> P.Price -> PostingWithFormat
lincolnizePriced e pr = PostingWithFormat p fmt where
  fmt = P.priceFormat pr
  p = U.Posting Nothing Nothing Nothing tradingAcct (B.Tags [])
      (Just e') (B.Memo [])
  e' = B.Entry dc amt
  dc = lincolnizeSign (P.sign e)
  amt = B.Amount qty (lincolnizeCommodity (P.toCommodity pr))
  qty = B.mult (P.qtyPerUnit pr) (P.qty . P.amount $ e)
  
makePrice :: T.Day -> P.Entry -> P.Price -> Maybe PriceWithFormat
makePrice d e pr = let
  fmt = P.priceFormat pr
  dt = lincolnizeDay d
  fr = B.From (lincolnizeCommodity (P.commodity . P.amount $ e))
  to = B.To (lincolnizeCommodity (P.toCommodity pr))
  cpu = B.CountPerUnit (P.qtyPerUnit pr)
  in do
    pr' <- B.newPrice fr to cpu
    let pp = B.PricePoint dt pr'
    return $ PriceWithFormat pp fmt

lincolnizeDay :: T.Day -> B.DateTime
lincolnizeDay d = B.DateTime lt tz where
  lt = T.LocalTime d tod
  tod = T.midnight
  tz = B.noOffset

tradingAcct :: B.Account
tradingAcct = Bd.crashy . Bd.account $ "Income:Trading"

lincolnizeMemo :: Maybe B.MemoLine -> B.Memo
lincolnizeMemo mayMemo = case mayMemo of
  Nothing -> B.Memo []
  Just ml -> B.Memo [ml]

lincolnizeSign :: P.Sign -> B.DrCr
lincolnizeSign s = case s of
  P.Positive -> B.Debit
  P.Negative -> B.Credit

lincolnizeCommodity :: P.Commodity -> B.Commodity
lincolnizeCommodity (P.Commodity tne) = B.Commodity (s1:|[]) where
  s1 = B.SubCommodity tne

lincolnizeAmount :: P.Amount -> B.Amount
lincolnizeAmount (P.Amount q c) = B.Amount q (lincolnizeCommodity c)

{-
Values

Take this transaction for example:

2011/03/02 Broker
    Assets:Brokerage             100 F @ $5
    Assets:Checking              $-500.00

The value of the first posting is $500.

This translates to Penny like so:

@ 2011/03/02 F $5

2011/03/02 Broker
    Assets:Brokerage             Dr 100 F
    Income:Trading               Cr 100 F
    Income:Trading               Dr $500
    Assets:Checking              Cr $500.00

A posting with a price generates 3 postings:

1. A posting whose DrCr is determined by the sign on the Ledger
entry. Its commodity is the same as the commodity on the Ledger
entry. Its quantity is the same as the quantity on the Ledger entry.
Its account is the same as the account on the Ledger entry.

2. A posting whose account is Income:Trading. Its DrCr is the opposite
of the DrCr in number 1. Its commodity and quantity are the same as
in number 1.

3. A posting whose account is Income:Trading. Its DrCr is the same as
in number 1. Its commodity is the same as the commodity on the
Ledger price. Its quantity is the quantity in the Ledger price
multiplied by the quantity on the Ledger entry.

You also generate a price. Its From commodity is the same
as the commodity on the Ledger entry. Its To commodity is
the commodity on the Ledger price. The countPerUnit is the same
as the quantity on the Ledger price.

Another example. This Penny transaction and price:

@ 2011/06/06 LUV $10

2011/06/06 Sell stock
    Assets:Brokerage                 Cr 100 LUV
    Income:Trading                   Dr 100 LUV
    Income:Trading                   Cr $1000
    Assets:Checking                  Dr $1000

translates to this Ledger transaction:
2011/06/06 Sell stock
    Assets:Brokerage                -100 LUV @ $10
    Assets:Checking                 $1000
-}
