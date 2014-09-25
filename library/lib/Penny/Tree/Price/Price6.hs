module Penny.Tree.Price.Price6 where

import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Spaces as Spaces
import qualified Penny.Tree.Square.Close as Close
import Control.Applicative
import Text.Parsec.Text

-- | Appears after a Lewis RadCom and optional spaces in a Price4.

data T
  = Commodity (Either Currency.T Commodity.T)
              (Maybe Spaces.T)
              Close.T
  | Close Close.T
          (Maybe Spaces.T)
          (Either Currency.T Commodity.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Commodity <$> ( fmap Left Currency.parser
                    <|> fmap Right Commodity.parser)
              <*> optional Spaces.parser
              <*> Close.parser
  <|> Close <$> Close.parser
            <*> optional Spaces.parser
            <*> ( fmap Left Currency.parser
                  <|> fmap Right Commodity.parser )
