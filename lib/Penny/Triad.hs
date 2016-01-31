{-# LANGUAGE TemplateHaskell #-}
module Penny.Triad where

import Control.Lens.TH (makeLenses)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.State (State)

import Penny.Arrangement
import Penny.Balance
import Penny.Commodity
import Penny.Decimal
import qualified Penny.Grammar as G
import Penny.Polar

type QtyRep = Either (G.NonNeutral, Pole) G.Neutral

data ArrangedQtyRep = ArrangedQtyRep
  { _qtyRep :: QtyRep
  , _arrangement :: Maybe Arrangement
  } deriving (Eq, Ord, Show)

makeLenses ''ArrangedQtyRep

type Quantum = Either ArrangedQtyRep Decimal

-- | Has the original quantity representation (if there is one) along
-- with the actual arithmetic quantity (if there is no original
-- representation).  Always has the commodity.
data Triad = Triad
  { _quantum :: Quantum
  , _commodity :: Commodity
  } deriving (Eq, Ord, Show)

makeLenses ''Triad

