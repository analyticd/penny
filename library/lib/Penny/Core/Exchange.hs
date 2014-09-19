module Penny.Core.Exchange where

import qualified Penny.Core.Concrete as Concrete

newtype T = T { toConcrete :: Concrete.T }
  deriving (Eq, Ord, Show)

