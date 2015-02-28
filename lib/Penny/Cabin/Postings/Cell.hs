{-# LANGUAGE OverloadedStrings #-}
module Penny.Cabin.Postings.Cell where

import qualified Penny.Lincoln as L
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as X
import qualified Penny.Queries.Tranche as Q
import Data.Maybe
import Control.Monad
import qualified Data.Sequence as S
import qualified Data.Map as M
import Data.Monoid

data CellTag
  = DebitTag
  | CreditTag
  | ZeroTag
  | InfoTag
  | NoticeTag
  deriving (Eq, Ord, Show, Enum, Bounded)

spacer :: Int -> (CellTag, Text)
spacer i = (InfoTag, X.replicate i (X.singleton ' '))

date :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
date trch = do
  t <- Q.best (\r s -> isJust (L.scalarDate s) && r == L.User) trch
  txt <- maybe (return X.empty) L.displayTree t
  return (InfoTag, txt)

account :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
account trch = do
  frst <- Q.postingTrees trch
  let pd r s = maybe False (const True) $ do
        guard (r == L.User)
        txt <- L.scalarChars s
        guard (txt == "acct")
  mayTree <- L.findTreeInForest pd frst
  subFrst <- maybe (return S.empty) L.children mayTree
  txt <- L.displayForest subFrst
  return (InfoTag, txt)

number :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
number trch = do
  t <- Q.best (\r s -> isJust (L.scalarInteger s) && r == L.User) trch
  txt <- maybe (return X.empty) L.displayTree t
  return (InfoTag, txt)

payee :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
payee trch = do
  t <- Q.best (\r s -> isJust (L.scalarChars s) && r == L.User) trch
  txt <- maybe (return X.empty) L.displayTree t
  return (InfoTag, txt)

userRealm
  :: (L.Scalar -> Bool)
  -> L.Realm
  -> L.Scalar
  -> Bool
userRealm pd r s = r == L.User && pd s

flag :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
flag trch = do
  let pd sc = maybe False (const True) $ do
        txt <- L.scalarChars sc
        guard . not . X.null $ txt
        guard (X.head txt == '(')
        guard (X.last txt == ')')
  t <- Q.best (userRealm pd) trch
  txt <- maybe (return X.empty) L.displayTree t
  return (InfoTag, txt)

side :: L.Ledger l => L.Tranche l a -> l (CellTag, Text)
side trch = return $ case Q.side trch of
  Nothing -> (ZeroTag, "--")
  Just L.Debit -> (DebitTag, "<")
  Just L.Credit -> (CreditTag, ">")

commodity :: Applicative l => L.Tranche l a -> l (CellTag, Text)
commodity trch =
  let L.Commodity txt = Q.commodity trch
  in pure (InfoTag, txt)

-- | A cell with the Qty representation only.
qty
  :: (L.Commodity -> L.Qty -> L.QtyRepAnyRadix)
  -- ^ Use what is in the Trio if possible.  If not, use this function
  -- to get the representation.
  -> L.Tranche l a -> l (CellTag, Text)
qty mkRep = undefined

-- | A cell with the Amount--that is, the 'Qty' and the 'Commodity'.
-- They are arranged properly--that is, the 'Commodity' is positioned
-- correctly relative to the 'Qty', and there is a space in between
-- the two if appropriate.
amount
  :: (L.Commodity -> L.Arrangement)
  -> (L.Commodity -> L.Qty -> L.QtyRepAnyRadix)
  -> L.Tranche l a -> l (CellTag, Text)
amount = undefined

-- | Reduces a 'QtyRepAnyRadix' to text, for the Qty only.  Prefixes
-- representations that have a comma radix with a backtick.
displayQtyRepAnyRadix
  :: L.QtyRepAnyRadix
  -> Text
displayQtyRepAnyRadix (L.QtyRepAnyRadix ei) = pfx <> rep
  where
    (pfx, rep) = case ei of
      Left (L.QtyRep (L.NilOrBrimPolar coc)) -> ("`", rp)
        where
          rp = case coc of
            L.Center nl -> X.pack . L.display nl $ ""
            L.OffCenter br _ -> X.pack . L.display br $ ""
      Right (L.QtyRep (L.NilOrBrimPolar coc)) -> ("", rp)
        where
          rp = case coc of
            L.Center nl -> X.pack . L.display nl $ ""
            L.OffCenter br _ -> X.pack . L.display br $ ""
