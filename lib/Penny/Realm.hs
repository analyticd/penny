module Penny.Realm where

data Realm = User | System
  deriving (Eq, Ord, Show, Enum, Bounded)
