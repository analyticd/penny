module Penny.Lincoln.Common
  ( module Penny.Lincoln.Common.DateTime
  , Commodity
  ) where

newtype Commodity =
  Commodity { unCommodity :: Text }
  deriving (Eq, Ord, Show)

