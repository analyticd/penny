module Penny.Qty where

import qualified Penny.Concrete as Concrete

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)
