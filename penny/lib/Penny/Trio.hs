-- | See "Penny.Troika" for more on how the 'Trio' relates to the
-- 'Penny.Troika.Troika'.  Also, some functions that operate entirely
-- on 'Trio' are in "Penny.Troika" to avoid cycyclic module
-- dependencies.
module Penny.Trio where

import qualified Data.Map as M
import Data.Sequence (Seq)

import Penny.Arrangement
import Penny.Commodity
-- import Penny.Copper.Decopperize
import Penny.Decimal
import Penny.Copper.Types
  (GrpRadCom, GrpRadPer)
import Penny.Mimode
import Penny.NonEmpty
import Penny.Polar
import Penny.Rep

-- | Given a particular 'Commodity', deliver the correct 'Arrangement'
-- depending on the history of how this commodity was arranged.
arrange
  :: M.Map Commodity (NonEmpty Arrangement)
  -- ^ History map
  -> Commodity
  -> Maybe Arrangement
arrange mp cy = M.lookup cy mp >>= mimode


-- | To convet a 'Trio' to an 'Amount', see
-- 'Penny.Troika.trioToAmount'.
data Trio
  = QC RepAnyRadix Commodity Arrangement
  -- ^ Specify a quantity and commodity and a corresponding entry is
  -- always recorded.
  --
  -- Preconditions: none
  --
  -- Postconditions: the balance is appropriately affected.

  | Q RepAnyRadix
  -- ^ Specify a quantity only.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  -- This commodity is selected to create the entry.
  --
  -- Postconditions: the balance is appropriately affected.


  | SC Pole Commodity
  -- ^ Specify the 'Side' and the 'Commodity'.
  --
  -- Preconditions: the imbalances contain the given commodity, and
  -- its balance has a side opposite the side given here.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.


  | S Pole
  -- ^ Specify a 'Side' only.
  --
  -- Preconditions: the imbalances contain exactly one commodity, and
  -- its 'Side' is opposite to the one specified here.
  --
  -- Postconditions: the imbalances is empty.

  | UC BrimAnyRadix Commodity Arrangement
  -- ^ Specify an unsigned abstract quantity only and a 'Commodity'
  -- and how they are arranged.
  --
  -- Preconditions: the imbalances contains the given commodity.
  --
  -- Postconditions: the given commodity in the imbalances either has
  -- its absolute value reduced or it flips to the opposite side.

  | NC NilAnyRadix Commodity Arrangement
  -- ^ Specify a nil quantity and a 'Commodity' and how they are
  -- arranged.
  --
  -- Preconditions: None.
  --
  -- Postconditions: the given commodity and quantity is added to
  -- the imbalances map.


  | US BrimAnyRadix
  -- ^ Specify an unsigned quantity only.
  --
  -- Preconditions: the imbalances contains exactly one commodity, and
  -- its absolute value is greater than or equal to than the given amount.
  --
  -- Postconditions: the given commodity in the imbalances is reduced
  -- by the amount given.

  | UU NilAnyRadix
  -- ^ Specify a nil quantity only.
  --
  -- Preconditions: the imbalances contain exactly one commodity.
  --
  -- Postconditions: no change in imbalances.

  | C Commodity
  -- ^ Specify a 'Commodity' only.
  --
  -- Preconditions: the given commodity is in the imbalances.
  --
  -- Postconditions: the given commodity is eliminated from the
  -- imbalances.


  | E
  -- ^ Specify nothing at all.
  --
  -- Preconditions: there is exactly one commodity in the imbalances.
  --
  -- Postconditions: the imbalances is empty.


  deriving Show

data TrioError
  = NoImbalance
  | MultipleImbalance (Commodity, DecNonZero) (Commodity, DecNonZero)
                       [(Commodity, DecNonZero)]
  | CommodityNotFound Commodity
  | BalanceIsSameSide Pole
  | UnsignedTooLarge BrimAnyRadix DecNonZero
  deriving Show

trioRendering
  :: Trio
  -> Maybe (Commodity, Arrangement,
        Either (Seq (GrpRadCom Char ())) (Seq (GrpRadPer Char ())))
trioRendering tri = case tri of
  QC qr cy ar -> Just (cy, ar, ei)
    where
      ei = groupers'RepAnyRadix qr
  UC rnn cy ar -> Just (cy, ar, ei)
    where
      ei = groupers'BrimAnyRadix rnn
  _ -> Nothing

-- | Extracts the representation from the 'Trio', if there is a
-- representation.  Does not return a 'Side'.
trioRepresentation
  :: Trio
  -> Maybe NilOrBrimAnyRadix
trioRepresentation tri = case tri of
  QC qr _ _ -> Just $ c'NilOrBrimAnyRadix'RepAnyRadix qr
  Q qr -> Just $ c'NilOrBrimAnyRadix'RepAnyRadix qr
  UC rn _ _ -> Just $ c'NilOrBrimAnyRadix'BrimAnyRadix rn
  US brim -> Just $ c'NilOrBrimAnyRadix'BrimAnyRadix brim
  UU nil -> Just $ c'NilOrBrimAnyRadix'NilAnyRadix nil
  _ -> Nothing

