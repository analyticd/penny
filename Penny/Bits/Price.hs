module Penny.Bits.Price (
  From ( From, unFrom ),
  To ( To, unTo ),
  CountPerUnit ( CountPerUnit, unCountPerUnit ),
  Price ( from, to, countPerUnit ),
  convert,
  price) where

import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Entry as E
import qualified Penny.Bits.Qty as Q
import Penny.Bits.Qty ( mult )

newtype From = From { unFrom :: C.Commodity }
               deriving (Eq, Ord, Show)

newtype To = To { unTo :: C.Commodity }
             deriving (Eq, Ord, Show)

newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Q.Qty }
                       deriving (Eq, Ord, Show)

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }
             deriving (Eq, Ord, Show)

convert :: Price -> A.Amount -> Maybe A.Amount
convert p (A.Amount q c) =
  if (unFrom . from $ p) /= c
  then Nothing
  else let q' = q `mult` (unCountPerUnit . countPerUnit $ p)
       in Just (A.Amount q' (unTo . to $ p))

price :: From -> To -> CountPerUnit -> 
price t c e = Price (From (A.commodity . E.amount $ e)) t c
