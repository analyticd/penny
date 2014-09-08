module Penny.Exchange where

import qualified Penny.Concrete as Concrete
import qualified Penny.Stone as Stone

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

fromConcrete :: Concrete.T -> T
fromConcrete = T

fromStone :: Stone.T -> T
fromStone = T . Concrete.fromCement . Stone.toCement

toStone :: T -> Stone.T
toStone = Stone.fromCement . Concrete.toCement . toConcrete

