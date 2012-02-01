module Penny.Lincoln.Bits.Price (
  From ( From, unFrom ),
  To ( To, unTo ),
  CountPerUnit ( CountPerUnit, unCountPerUnit ),
  Price ( from, to, countPerUnit ),
  convert,
  newPrice) where

import Penny.Lincoln.Bits.Amount (Amount(Amount))
import Penny.Lincoln.Bits.Commodity (Commodity)
import Penny.Lincoln.Bits.Qty (Qty, mult)

newtype From = From { unFrom :: Commodity }
               deriving (Eq, Ord, Show)

newtype To = To { unTo :: Commodity }
             deriving (Eq, Ord, Show)

newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Qty }
                       deriving (Eq, Ord, Show)

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }
             deriving (Eq, Ord, Show)

convert :: Price -> Amount -> Maybe Amount
convert p (Amount q c) =
  if (unFrom . from $ p) /= c
  then Nothing
  else let q' = q `mult` (unCountPerUnit . countPerUnit $ p)
       in Just (Amount q' (unTo . to $ p))

newPrice :: From -> To -> CountPerUnit -> Maybe Price
newPrice f t cpu =
  if unFrom f == unTo t
  then Nothing
  else Just $ Price f t cpu

