module Penny.Tree.Price.Price4 where

import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Tree.Spaces as Spaces
import qualified Penny.Tree.Price.Price6 as Price6
import qualified Penny.Tree.Square.Close as Close

-- | Appears immediately after an opening square bracket and optional
-- spaces.
data T
  = Commodity (Either Currency.T Commodity.T)
              (Maybe Spaces.T)
              (Lewis.T RadCom.T)
              Close.T
  -- ^ A commodity or currency appears right away
  | Lewis (Lewis.T RadCom.T)
          (Maybe Spaces.T)
          Price6.T
  deriving (Eq, Ord, Show)
