{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Viewpost where

import Data.Sums
import Penny.Ledger
import Penny.SeqUtil
import Penny.Converted
import Control.Lens
import Penny.Amount
import Penny.Qty
import Penny.Representation

data Viewpost a = Viewpost
  { _viewpost :: View PostingL
  , _viewpostee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Viewpost

-- | Gets the converted Amount if there is one; otherwise, gets the
-- Amount from the PostingL.
bestAmount :: Viewpost (Converted a) -> Ledger Amount
bestAmount v = case v ^. viewpostee.converted of
  Nothing -> do
    cy <- Penny.Ledger.commodity . view (viewpost.onView) $ v
    qt <- Penny.Ledger.qty . view (viewpost.onView) $ v
    return $ Amount cy qt
  Just a -> return a

originalAmount :: Viewpost a -> Ledger Amount
originalAmount v = do
  cy <- Penny.Ledger.commodity . view (viewpost.onView) $ v
  qt <- Penny.Ledger.qty . view (viewpost.onView) $ v
  return $ Amount cy qt


-- | Gets the 'Qty' from the converted Amount, if there is one.
-- Otherwise, gets the 'QtyRep' from the 'Trio', if there is one.
-- Otherwise, gets the 'Qty'.
bestQtyRep
  :: Viewpost (Converted a)
  -> Ledger (S3 RepNonNeutralNoSide QtyRepAnyRadix Qty)
bestQtyRep vp = case vp ^. viewpostee.converted of
  Nothing -> Penny.Ledger.originalQtyRep (vp ^. viewpost.onView)
  Just (Amount _ qt) -> return $ S3c qt

