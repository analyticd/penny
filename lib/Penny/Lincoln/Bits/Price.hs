module Penny.Lincoln.Bits.Price (
  From ( From, unFrom ),
  To ( To, unTo ),
  CountPerUnit ( CountPerUnit, unCountPerUnit ),
  Price ( from, to, countPerUnit ),
  convert,
  newPrice) where

import qualified Penny.Lincoln.Bits.Open as O
import Penny.Lincoln.Bits.Qty (Qty, mult)

newtype From = From { unFrom :: O.Commodity }
               deriving (Eq, Ord, Show)

newtype To = To { unTo :: O.Commodity }
             deriving (Eq, Ord, Show)

newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Qty }
                       deriving (Eq, Ord, Show)

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }
             deriving (Eq, Ord, Show)

-- | Convert an amount from the From price to the To price. Fails if
-- the From commodity in the Price is not the same as the commodity in
-- the Amount.
convert :: Price -> O.Amount -> Maybe O.Amount
convert p (O.Amount q c sd sb) =
  if (unFrom . from $ p) /= c
  then Nothing
  else let q' = q `mult` (unCountPerUnit . countPerUnit $ p)
       in Just (O.Amount q' (unTo . to $ p) sd sb)

-- | Succeeds only if From and To are different commodities.
newPrice :: From -> To -> CountPerUnit -> Maybe Price
newPrice f t cpu =
  if unFrom f == unTo t
  then Nothing
  else Just $ Price f t cpu

