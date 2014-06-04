module Penny.Lincoln.Bits.Price (
    From ( From, unFrom )
  , To ( To, unTo )
  , CountPerUnit ( CountPerUnit, unCountPerUnit )
  , Price ( from, to, countPerUnit )
  , newPrice
  ) where

import Data.Monoid (mconcat)
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent ((==~))
import qualified Penny.Lincoln.Bits.Open as O
import Penny.Lincoln.Bits.Qty (QtyRep)


newtype From = From { unFrom :: O.Commodity }
  deriving (Eq, Ord, Show)

newtype To = To { unTo :: O.Commodity }
  deriving (Eq, Ord, Show)

newtype CountPerUnit = CountPerUnit { unCountPerUnit :: QtyRep }
  deriving (Eq, Ord, Show)

instance Ev.Equivalent CountPerUnit where
  equivalent (CountPerUnit x) (CountPerUnit y) = x ==~ y
  compareEv (CountPerUnit x) (CountPerUnit y) = Ev.compareEv x y

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }
             deriving (Eq, Ord, Show)

-- | Two Price are equivalent if the From and To are equal and the
-- CountPerUnit is equivalent.

instance Ev.Equivalent Price where
  equivalent (Price xf xt xc) (Price yf yt yc) =
    xf == yf && xt == yt && xc ==~ yc

  compareEv (Price xf xt xc) (Price yf yt yc) = mconcat
    [ compare xf yf
    , compare xt yt
    , Ev.compareEv xc yc
    ]

-- | Succeeds only if From and To are different commodities.
newPrice :: From -> To -> CountPerUnit -> Maybe Price
newPrice f t cpu =
  if unFrom f == unTo t
  then Nothing
  else Just $ Price f t cpu
