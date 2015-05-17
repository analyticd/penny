{-# LANGUAGE TemplateHaskell #-}
module Penny.ConvertedPosting where

import Control.Lens
import Control.Monad
import qualified Data.Map as M
import Data.Monoid
import Penny.Amount
import Penny.Converted
import Penny.Ledger
import Penny.NonEmpty
import Penny.Popularity
import Penny.Trio
import qualified Data.Sequence as Seq

data ConvertedPosting l = ConvertedPosting
  { _convertedAmount :: Maybe Amount
  , _posting :: PostingL l
  }

makeLenses ''ConvertedPosting

convertPosting
  :: Ledger l
  => Converter
  -> PostingL l
  -> l (ConvertedPosting l)
convertPosting (Converter cnv) p = do
  qt <- qty p
  cy <- commodity p
  let amt = Amount cy qt
  return $ ConvertedPosting (cnv amt) p

vote
  :: Ledger l
  => ConvertedPosting l
  -> l Renderings
vote = liftM f . trio . _posting
  where
    f tri = case trioRendering tri of
      Nothing -> mempty
      Just (cy, ar, ei) -> Renderings $ M.singleton cy (NonEmpty (ar, ei) Seq.empty)


-- | Gets the converted Amount if there is one; otherwise, gets the
-- Amount from the PostingL.
bestAmount :: Ledger l => ConvertedPosting l -> l Amount
bestAmount (ConvertedPosting mayAmt pstg) = case mayAmt of
  Just a -> return a
  Nothing -> liftM2 Amount (commodity pstg) (qty pstg)
