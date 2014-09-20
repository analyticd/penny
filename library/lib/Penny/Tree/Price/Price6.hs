module Penny.Tree.Price.Price6 where

import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Spaces as Spaces
import qualified Penny.Tree.Square.Close as Close

-- | Appears after a Lewis RadCom and optional spaces in a Price4.

data T
  = Commodity (Either Currency.T Commodity.T)
              (Maybe Spaces.T)
              Close.T
  | Close Close.T
          (Maybe Spaces.T)
          (Either Currency.T Commodity.T)
  deriving (Eq, Ord, Show)
