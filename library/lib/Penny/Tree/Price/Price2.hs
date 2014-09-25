module Penny.Tree.Price.Price2 where

import qualified Penny.Tree.Commodity as Commodity
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Square.Open as Open
import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Tree.Spaces as Spaces
import qualified Penny.Tree.Price.Price3 as Price3
import qualified Penny.Tree.Price.Price4 as Price4
import Text.Parsec.Text
import Control.Applicative

data T
  = To (Either Commodity.T Currency.T) (Maybe Spaces.T) Price3.T
  -- ^ The To commodity appears immediately
  | Square Open.T (Maybe Spaces.T) Price4.T
  -- ^ An opening square brace appears immediately
  | Lewis (Lewis.T RadPer.T) (Maybe Spaces.T)
          (Either Commodity.T Currency.T)
  -- ^ An unquoted quantity with a period radix appears immediately
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = To <$> (fmap Left Commodity.parser <|> fmap Right Currency.parser)
       <*> optional Spaces.parser
       <*> Price3.parser

  <|> Square <$> Open.parser <*> optional (Spaces.parser)
             <*> Price4.parser

  <|> Lewis <$> Lewis.parser RadPer.parseRadix RadPer.parser
            <*> optional Spaces.parser
            <*> ( fmap Left Commodity.parser
                  <|> fmap Right Currency.parser)
