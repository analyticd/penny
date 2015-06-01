{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Viewpost where

import Penny.Ledger
import Penny.SeqUtil
import Control.Lens
import Data.Foldable (Foldable)
import Penny.Converted
import Penny.Amount

data Viewpost l a = Viewpost
  { _viewpost :: View (PostingL l)
  , _viewpostee :: a
  } deriving (Functor, Foldable, Traversable)

makeLenses ''Viewpost

bestAmount :: Ledger l => Viewpost l (Converted a) -> l Amount
bestAmount v = case v ^. viewpostee.converted of
  Nothing -> do
    cy <- Penny.Ledger.commodity . view (viewpost.onView) $ v
    qt <- Penny.Ledger.qty . view (viewpost.onView) $ v
    return $ Amount cy qt
  Just a -> return a

originalAmount :: Ledger l => Viewpost l a -> l Amount
originalAmount v = do
  cy <- Penny.Ledger.commodity . view (viewpost.onView) $ v
  qt <- Penny.Ledger.qty . view (viewpost.onView) $ v
  return $ Amount cy qt
