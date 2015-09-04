{-# LANGUAGE TypeFamilies #-}
module Penny.Side where

import Penny.Offset
import Penny.PluMin
import Penny.Display

data Side = Debit | Credit
  deriving (Eq, Ord, Show)

instance Display Side where
  display Debit = ('<':)
  display Credit = ('>':)

instance HasOffset Side where
  offset Debit = Credit
  offset Credit = Debit

instance Signed Side where
  sign Debit = Minus
  sign Credit = Plus
  fromSign Minus = Debit
  fromSign Plus = Credit

class HasSide a where
  side :: a -> Side

class SidedOrNeutral a where
  sideOrNeutral :: a -> Maybe Side
