module Penny.Harvest.Serialize.State where

data T = T
  { localTopLine :: !Int
  , localPosting :: !Int
  , globalTopLine :: !Int
  , globalPosting :: !Int
  } deriving (Eq, Ord, Show)

incrTopLine :: T -> T
incrTopLine (T lt lp gt gp) = T (succ lt) lp (succ gt) gp

incrPosting :: T -> T
incrPosting (T lt lp gt gp) = T lt (succ lp) gt (succ gp)

decrTopLine :: T -> T
decrTopLine (T lt lp gt gp) = T (pred lt) lp (pred gt) gp

decrPosting :: T -> T
decrPosting (T lt lp gt gp) = T lt (pred lp) gt (pred gp)
