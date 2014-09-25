-- | Prices.  A price has the date, the optional time, the from
-- commodity, the to commodity, and the quantity of the to commodity.

module Penny.Tree.Price where

import qualified Penny.Tree.PostSpace as PostSpace
import qualified Penny.Tree.Ampersand as Ampersand
import qualified Penny.Tree.Date as Date
import qualified Penny.Tree.Time as Time
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Price.Price2 as Price2
import qualified Penny.Tree.Newline as Newline
import Control.Applicative
import Text.Parsec.Text

data T = T
  { ampersand :: PostSpace.T Ampersand.T
  , date :: PostSpace.T Date.T
  , time :: Maybe (PostSpace.T Time.T)
  , from :: PostSpace.T (Either Currency.T Commodity.T)
  , price2 :: Price2.T
  , newline :: Newline.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = T
  <$> PostSpace.parser Ampersand.parser
  <*> PostSpace.parser Date.parser
  <*> optional (PostSpace.parser Time.parser)
  <*> PostSpace.parser ( fmap Left Currency.parser
                         <|> fmap Right Commodity.parser)
  <*> Price2.parser
  <*> Newline.parser
