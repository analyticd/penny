module Penny.Lincoln.Exchange where

import qualified Penny.Lincoln.Concrete as Concrete

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

