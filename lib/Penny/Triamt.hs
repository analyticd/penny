{-# LANGUAGE TemplateHaskell #-}
module Penny.Triamt where

import Penny.Amount
import Penny.Trio

-- | A 'Trio' combined with an 'Amount'.
data Triamt = Triamt
  { _trio :: Trio
  , _amount :: Amount
  } deriving (Eq, Ord, Show)
