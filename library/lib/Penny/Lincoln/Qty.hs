module Penny.Lincoln.Qty where

import qualified Penny.Lincoln.Concrete as Concrete

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)
