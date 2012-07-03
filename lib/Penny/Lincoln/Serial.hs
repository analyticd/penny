module Penny.Lincoln.Serial (Serial, forward, backward, serials) where

-- | A type for serial numbers, used widely for different purposes
-- throughout Penny.

data Serial = Serial {
  -- | Numbered from first to last, beginning at zero.
  forward :: !Int
  
  -- | Numbered from last to first, ending at zero.
  , backward :: !Int
  } deriving Show
             
-- | Applied to a list of items, return a list of Serials usable to
-- identify the list of items.
serials :: [a] -> [Serial]
serials as = zipWith Serial fs rs where
  len = length as
  fs = take len . iterate succ $ 0
  rs = take len . iterate pred $ (len - 1)
