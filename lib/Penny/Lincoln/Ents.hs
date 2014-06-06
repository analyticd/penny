-- | Balanced sets.  This module is the guardian of the core principle
-- of double-entry accounting, which is that all transactions must be
-- balanced.
module Penny.Lincoln.Ents where

import Penny.Lincoln.Balance
import Penny.Lincoln.Common
import Penny.Lincoln.Decimal

-- | A written record of an entry, like what you would get from a
-- ledger file.  Holds the abstract (which represents both the
-- quantity and the side) and the arrangement.
data Record = Record
  { rAbstract :: Abstract Side
  , rArrange :: Arrangement
  } deriving (Eq, Ord, Show)

-- | Information from a single entry.  Always contains a 'Commodity'
-- and a 'Qty' which holds the quantity information in concrete form.
-- There is also a 'Maybe' 'Record', which is 'Just' only if the
-- 'ents' function was originally supplied with a 'Record'.  This
-- holds the quantity and commodity information as they were
-- originally written.
--
-- There is also arbitrary metadata.
data Ent a = Ent
  { entCommodity :: Commodity
  , entAbstract :: Maybe Record
  , entConcrete :: Qty
  , entMeta :: a
  } deriving (Eq, Ord, Show)

instance Functor Ent where
  fmap f e = e { entMeta = f (entMeta e) }

newtype Ents a = Ents { unEnts :: [Ent a] }
  deriving (Eq, Ord, Show)

-- | Creates an 'Ents' only if the data is balanced.
ents
  :: [(Commodity, Maybe (Either Record Qty), a)]
  -> Maybe (Ents a)
ents = undefined

-- | Indicates whether the list in question is either balanced or is
-- not balanced but can be inferred.
inferable
  :: [Maybe (Commodity, (Either Record Qty))]
  -> Maybe ([Maybe Qty], Maybe Qty)
  -- ^ Returns Nothing if the list in question cannot be balanced.
  -- Otherwise, returns Just with each Qty that was present in the
  -- original list or that was calculated from the Record, along with
  -- a Just with the inferable Qty.
  --
  -- The list has, at most, one Nothing.  If it has one Nothing, the
  -- pair also has a Just Qty; if it has no Nothings, the second of
  -- the pair is also Nothing.
inferable = undefined
