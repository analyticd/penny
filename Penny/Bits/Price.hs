module Penny.Bits.Price where

import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Entry as E
import Penny.Bits.Qty ( mult )

newtype From = From { unFrom :: C.Commodity }
newtype To = To { unTo :: C.Commodity }
newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Q.Qty }

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }

convert :: Price -> A.Amount -> Maybe A.Amount
convert p (A.Amount q c) =
  if (unFrom . from $ p) /= c
  then Nothing
  else let q' = q `mult` (unCountPerUnit . countPerUnit $ p)
       in Just (A.Amount q' (unTo . to $ p))

price :: To -> CountPerUnit -> E.Entry -> Price
price t c e = Price (From (A.commodity . E.amount $ e)) t c
