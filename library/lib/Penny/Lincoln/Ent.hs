module Penny.Lincoln.Ent where

import qualified Penny.Qty as Qty
import qualified Penny.Commodity as Commodity
import Data.Foldable
import Data.Traversable

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
