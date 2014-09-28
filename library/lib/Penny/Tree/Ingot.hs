-- | An Ingot is a Lewis paired with an optional Currency.

module Penny.Tree.Ingot where

import qualified Penny.Tree.Ingot.Period as Period
import qualified Penny.Tree.Ingot.Comma as Comma
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Anna as Anna
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Core.Anna.RadPer as RadPer
import qualified Penny.Core.Commodity as Commodity
import qualified Penny.Core.Orient as Orient
import qualified Penny.Core.Side as Side
import qualified Penny.Tree.Ingot.Error as Error
import qualified Penny.Core.Trio as Trio

data T
  = Comma Comma.T
  | Period Period.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Comma <$> Comma.parser
  <|> Period <$> Period.parser

toAnna :: T -> Either (Anna.T RadCom.T) (Maybe (Anna.T RadPer.T))
toAnna (Comma c) = Left $ Comma.toAnna c
toAnna (Period p) = Right $ Period.toAnna p

toCurrency :: T -> Maybe Currency.T
toCurrency (Comma c) = Comma.toCurrency c
toCurrency (Period p) = Period.toCurrency p

toTrio
  :: Maybe (Commodity.T, Orient.T)
  -> Maybe Side.T
  -> T
  -> Either Error.T Trio.T
toTrio mp ms (Comma c) = Comma.toTrio mp ms c
toTrio mp ms (Period p) = Period.toTrio mp ms p
