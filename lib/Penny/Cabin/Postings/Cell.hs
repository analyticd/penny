{-# LANGUAGE OverloadedStrings #-}
module Penny.Cabin.Postings.Cell where

import Penny.Lincoln.Clatch
import Data.Text (Text)
import qualified Data.Text as X


{-
import qualified Penny.Lincoln as L
import Control.Applicative
import Data.Text (Text)
import qualified Penny.Queries.Tranche as Q
import Data.Maybe
import Control.Monad
import qualified Data.Sequence as S
import Data.Monoid
import qualified Data.Map as M

date :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
date trch = do
  t <- Q.best (L.pdScalarUser (isJust . L.scalarDate)) trch
  txt <- maybe (return X.empty) L.displayTree t
  return [(InfoTag, txt)]

account :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
account trch = do
  frst <- Q.postingTrees trch
  mayTree <- L.findTreeInForest L.pdNoScalarUser frst
  subFrst <- maybe (return S.empty) L.children mayTree
  txt <- L.displayForest subFrst
  return [(InfoTag, txt)]

tags :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
tags trch = do
  frst <- Q.postingTrees trch
  let pd s = maybe False (const True) $ do
        txt <- L.scalarChars s
        guard (txt == "tags")
  mayTree <- L.findTreeInForest (L.pdScalarUser pd) frst
  subFrst <- maybe (return S.empty) L.children mayTree
  txt <- L.displayForest subFrst
  return [(InfoTag, txt)]


number :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
number trch = do
  t <- Q.best (L.pdScalarUser (isJust . L.scalarInteger)) trch
  txt <- maybe (return X.empty) L.displayTree t
  return [(InfoTag, txt)]

payee :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
payee trch = do
  let pd s = maybe False (const True) $ do
        txt <- L.scalarChars s
        guard . not . X.null $ txt
        guard (X.head txt /= '(')
        guard (X.last txt /= ')')
  t <- Q.best (L.pdScalarUser pd) trch
  txt <- maybe (return X.empty) L.displayTree t
  return [(InfoTag, txt)]

userRealm
  :: (L.Scalar -> Bool)
  -> L.Realm
  -> L.Scalar
  -> Bool
userRealm pd r s = r == L.User && pd s

flag :: L.Ledger l => L.Tranche l -> l [(CellTag, Text)]
flag trch = do
  let pd sc = maybe False (const True) $ do
        txt <- L.scalarChars sc
        guard . not . X.null $ txt
        guard (X.head txt == '(')
        guard (X.last txt == ')')
  t <- Q.best (L.pdScalarUser pd) trch
  txt <- maybe (return X.empty) L.displayTree t
  return [(InfoTag, txt)]

side :: L.Tranche l -> [(CellTag, Text)]
side = (:[]) . cellMaySide . Q.side

cellMaySide :: Maybe L.Side -> (CellTag, Text)
cellMaySide s = case s of
  Nothing -> (ZeroTag, "--")
  Just L.Debit -> (DebitTag, "<")
  Just L.Credit -> (CreditTag, ">")

commodity :: Applicative l => L.Tranche l -> l [(CellTag, Text)]
commodity trch =
  let L.Commodity txt = Q.commodity trch
  in pure [(InfoTag, txt)]

-- | A cell with the Qty representation only.  Uses the converted
-- 'L.Amount' if available; otherwise, uses the 'L.Trio' amount;
-- otherwise, uses the calculated 'L.Amount'.
qty
  :: L.Ledger l
  => (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -- ^ Computes representations from calculated 'L.Amount'
  -> L.Tranche l
  -> l [(CellTag, Text)]
qty rep trch = fmap f $ selectQtyRep rep (Q.bevy trch)
  where
    f qtyRep = [(cellTag trch, displayNilOrBrimScalarAnyRadix qtyRep)]

selectQtyRep
  :: L.Ledger l
  => (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> L.Bevy l
  -> l L.NilOrBrimScalarAnyRadix
selectQtyRep rep (L.Bevy pstg am mayC) = case mayC of
  Nothing -> do
    tri <- L.postingTrio pstg
    return $ case L.trioRepresentation tri of
      Nothing -> rep am
      Just nbs -> nbs
  Just (L.Converted convAm) -> return $ rep convAm

cellTag :: L.Tranche l -> CellTag
cellTag = tagMaySide . L.qtySide . Q.qty


tagMaySide :: Maybe L.Side -> CellTag
tagMaySide s = case s of
  Nothing -> ZeroTag
  Just L.Debit -> DebitTag
  Just L.Credit -> CreditTag


-- | A cell with the 'L.Amount' -- that is, the 'Qty' and the 'Commodity'.
-- They are arranged properly--that is, the 'Commodity' is positioned
-- correctly relative to the 'Qty', and there is a space in between
-- the two if appropriate.
amount
  :: L.Ledger l
  => (L.Commodity -> L.Arrangement)
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> L.Tranche l
  -> l [(CellTag, Text)]
amount arnge rep trch = f <$> selectQtyRep rep (Q.bevy trch) <*> Q.trio trch
  where
    f qr tri = [(cellTag trch, lft <> sp <> rgt)]
      where
        L.Arrangement ori (L.SpaceBetween sb) = case L.trioRendering tri of
          Nothing -> arnge (L.Commodity cy)
          Just (_, a, _) -> a
        L.Commodity cy = Q.commodity trch
        (lft, rgt) = case ori of
          L.CommodityOnLeft -> (cy, qrTxt)
          L.CommodityOnRight -> (qrTxt, cy)
        sp = if sb then " " else ""
        qrTxt = displayNilOrBrimScalarAnyRadix qr

-- | A cell with all 'L.Balances'; similar to 'amount' but includes a
-- line for every 'L.Commodity' in the 'Balances'.
balAmounts
  :: (L.Commodity -> L.Arrangement)
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> L.Tranche l
  -> [(CellTag, Text)]
balAmounts arnge rep
  = map toCell . M.toList . (\(L.Balances mp) -> mp) . Q.balances
  where
    toCell (L.Commodity cy, q) = (tag, lft <> sp <> rgt)
      where
        L.Arrangement ori (L.SpaceBetween sb) = arnge (L.Commodity cy)
        sp = if sb then " " else ""
        qr = rep (L.Amount (L.Commodity cy) q)
        qrTxt = displayNilOrBrimScalarAnyRadix qr
        (lft, rgt) = case ori of
          L.CommodityOnLeft -> (cy, qrTxt)
          L.CommodityOnRight -> (qrTxt, cy)
        tag = tagMaySide . L.qtySide $ q


balSides :: L.Tranche l -> [(CellTag, Text)]
balSides
  = fmap (cellMaySide . L.qtySide . snd)
  . M.toList
  . (\(L.Balances m) -> m)
  . Q.balances

balQtys
  :: (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> L.Tranche l
  -> [(CellTag, Text)]
balQtys rep = map toCell . M.toList . (\(L.Balances m) -> m) . Q.balances
  where
    toCell (L.Commodity cy, q) = (tag, displayNilOrBrimScalarAnyRadix qr)
      where
        qr = rep (L.Amount (L.Commodity cy) q)
        tag = tagMaySide . L.qtySide $ q

balCommodities
  :: L.Tranche l
  -> [(CellTag, Text)]
balCommodities = map toCell . M.toList . (\(L.Balances m) -> m) . Q.balances
  where
    toCell (L.Commodity cy, q) = (tag, cy)
      where
        tag = tagMaySide . L.qtySide $ q

-- | Reduces a 'QtyRepAnyRadix' to text, for the Qty only.  Prefixes
-- representations that have a comma radix with a backtick.
displayNilOrBrimScalarAnyRadix
  :: L.NilOrBrimScalarAnyRadix
  -> Text
displayNilOrBrimScalarAnyRadix (L.NilOrBrimScalarAnyRadix ei) = pfx <> rep
  where
    (pfx, rep) = case ei of
      Left radCom -> ("`", X.pack . L.display radCom $ "")
      Right radPer -> ("", X.pack . L.display radPer $ "")
-}
