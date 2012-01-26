module Penny.Bits.Price where

import qualified Penny.Bits.Amount as A
import qualified Penny.Bits.Commodity as C
import qualified Penny.Bits.Qty as Q
import qualified Penny.Bits.Entry as E
import Penny.Bits.Qty ( mult )

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }

convert :: Price -> A.Amount -> Maybe A.Amount
convert p (A.Amount q c) =
  if (unFrom . from $ p) /= c
  then Nothing
  else let q' = q `mult` (unCountPerUnit . countPerUnit $ p)
       in Just (A.Amount q' (unTo . to $ p))

newtype From = From { unFrom :: C.Commodity }
newtype To = To { unTo :: C.Commodity }
newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Q.Qty }

-- | A value of a is an entry multiplied by a price or, if there is no
-- price, simply the entry.
data Value = Value { drCr :: E.DrCr
                   , amount :: A.Amount }

price :: From -> To -> CountPerUnit -> E.Entry -> (Price, Value)
price f t c e = (p, v) where
  q' = (unCountPerUnit c) `mult` (A.qty . E.amount $ e)  
  a = A.Amount q' (unTo t)
  v = Value (E.drCr e) a
  p = Price f t c
