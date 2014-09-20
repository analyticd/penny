module Penny.Tree.Price.Price2 where

import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Square.Open as Open
import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Tree.Spaces as Spaces
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Price.Price3 as Price3
import qualified Penny.Tree.Price.Price4 as Price4

data T
  = To (Either Commodity.T Currency.T) (Maybe Spaces.T) Price3.T
       Newline.T
  -- ^ The To commodity appears immediately
  | Square Open.T (Maybe Spaces.T) Price4.T
  -- ^ An opening square brace appears immediately
  | Lewis (Lewis.T RadPer.T) (Maybe Spaces.T)
          (Either Commodity.T Currency.T) Newline.T
  -- ^ An unquoted quantity with a period radix appears immediately
  deriving (Eq, Ord, Show)
