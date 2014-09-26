module Penny.Harvest.Serialize.Indexes where

data T = T
  { topLine :: Int
  , posting :: Int
  } deriving (Eq, Ord, Show)

incrTopLine :: T -> T
incrTopLine (T t p) = T (succ t) p

incrPosting :: T -> T
incrPosting (T t p) = T t (succ p)

decrTopLine :: T -> T
decrTopLine (T t p) = T (pred t) p

decrPosting :: T -> T
decrPosting (T t p) = T t (pred p)
