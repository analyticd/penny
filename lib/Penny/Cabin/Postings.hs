{-# LANGUAGE OverloadedStrings #-}
-- | The Postings report
module Penny.Cabin.Postings where

import Control.Applicative
import Control.Monad
import Penny.Lincoln
import Rainbox
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as X
import qualified Penny.Queries.Tranche as Q
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import Data.Monoid

postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l Box
postingsBox cols
  = fmap (gridByRows . F.toList . fmap F.toList)
  . makeRows cols

makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l (t2 (t1 Cell))
makeRows cols = T.sequenceA . fmap T.sequenceA . fmap mkRow
  where
    mkRow trch = fmap ($ trch) cols

data CellTag
  = DebitTag
  | CreditTag
  | ZeroTag
  | InfoTag
  | NoticeTag
  deriving (Eq, Ord, Show, Enum, Bounded)

type SimpleCell l = Tranche l -> l (CellTag, Text)

spacerCell :: Applicative l => Int -> SimpleCell l
spacerCell i _ = pure (InfoTag, X.replicate i (X.singleton ' '))

dateCell :: Ledger l => SimpleCell l
dateCell trch = do
  t <- Q.best (\r s -> isJust (scalarDate s) && r == User) trch
  txt <- maybe (return X.empty) displayTree t
  return (InfoTag, txt)

accountCell :: Ledger l => SimpleCell l
accountCell trch = do
  frst <- Q.postingTrees trch
  let pd r s = maybe False (const True) $ do
        guard (r == User)
        txt <- scalarChars s
        guard (txt == "acct")
  mayTree <- findTreeInForest pd frst
  subFrst <- maybe (return S.empty) children mayTree
  txt <- displayForest subFrst
  return (InfoTag, txt)

numberCell :: Ledger l => SimpleCell l
numberCell trch = do
  t <- Q.best (\r s -> isJust (scalarInteger s) && r == User) trch
  txt <- maybe (return X.empty) displayTree t
  return (InfoTag, txt)

payeeCell :: Ledger l => SimpleCell l
payeeCell trch = do
  t <- Q.best (\r s -> isJust (scalarChars s) && r == User) trch
  txt <- maybe (return X.empty) displayTree t
  return (InfoTag, txt)

userRealm
  :: (Scalar -> Bool)
  -> Realm
  -> Scalar
  -> Bool
userRealm pd r s = r == User && pd s

flagCell :: Ledger l => SimpleCell l
flagCell trch = do
  let pd sc = maybe False (const True) $ do
        txt <- scalarChars sc
        guard . not . X.null $ txt
        guard (X.head txt == '(')
        guard (X.last txt == ')')
  t <- Q.best (userRealm pd) trch
  txt <- maybe (return X.empty) displayTree t
  return (InfoTag, txt)

sideCell :: Ledger l => SimpleCell l
sideCell trch = return $ case Q.side trch of
  Nothing -> (ZeroTag, "--")
  Just Debit -> (DebitTag, "<")
  Just Credit -> (CreditTag, ">")

commodityCell :: Ledger l => SimpleCell l
commodityCell trch =
  let Commodity txt = Q.commodity trch
  in return (InfoTag, txt)

qtyCell
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Default rendering
  -> M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer)))
  -- ^ History map
  -> SimpleCell l
qtyCell dflt hist = undefined

-- | Reduces a 'QtyRepAnyRadix' to text, for the Qty only.  Prefixes
-- representations that have a comma radix with a backtick.
displayQtyRepAnyRadix
  :: QtyRepAnyRadix
  -> Text
displayQtyRepAnyRadix (QtyRepAnyRadix ei) = pfx <> rep
  where
    (pfx, rep) = case ei of
      Left (QtyRep (NilOrBrimPolar coc)) -> ("`", rp)
        where
          rp = case coc of
            Center nl -> X.pack . display nl $ ""
            OffCenter br _ -> X.pack . display br $ ""
      Right (QtyRep (NilOrBrimPolar coc)) -> ("", rp)
        where
          rp = case coc of
            Center nl -> X.pack . display nl $ ""
            OffCenter br _ -> X.pack . display br $ ""
