-- | Queries on 'Penny.Lincoln.Clatch.Clatch'.

module Penny.Queries.Clatch where

import Control.Monad
import qualified Penny.Lincoln as L

postingL :: L.Clatch m -> L.PostingL m
postingL
  (L.Filtered (L.Sersetted _ (L.RunningBalance _ (L.Sorted (L.Sersetted _
    (L.Filtered (L.Sersetted _ (_, vw)))))))) = pstg
  where
    L.Converted _ pstg = L.onView vw
