module Penny.Core.Ent where

import qualified Penny.Core.Qty as Qty
import qualified Penny.Core.Commodity as Commodity
import Data.Foldable
import Data.Traversable

-- | A single entry, consisting of a 'Penny.Core.Qty.T',
-- 'Penny.Core.Commodity.T', and arbitrary metadata.
--
-- See also:
--
-- * 'Penny.Core.Ents.T', a collection of 'T' (not necessarily
-- balanced)
--
-- * 'Penny.Core.Balanced.T', a balanced collection of 'T'
data T m = T
  { qty :: Qty.T
  , commodity :: Commodity.T
  , meta :: m
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f e = e { meta = f (meta e) }

instance Foldable T where
  foldr f z (T _ _ m) = f m z

instance Traversable T where
  sequenceA (T q c m) = fmap (T q c) m
