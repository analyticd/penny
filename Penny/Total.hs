module Penny.Total where

import Data.Map ( Map )
import qualified Data.Map as M
import Penny.Posting
import qualified Penny.Qty as Q

newtype Total = Total (Map Commodity (DrCr, Q.Qty))

addAmount :: Total -> Posting -> Total
addAmount (Total m) p = Total $ M.alter f c m where
  dc = drCr . entry $ p
  q = qty . amount . entry $ p
  c = commodity . amount . entry $ p
  f m = let
    new@(_, q') = case m of
      Nothing -> (dc, q)
      (Just (oldDrCr, oldQ)) -> let
        newDrCr = if oldQ > q then oldDrCr else dc
        newQ = Q.difference oldQ q
        in (newDrCr, newQ)
    in if q' == Q.zero then Nothing else Just new

