module Penny.Tree.TopLine.Item where

import qualified Penny.Tree.Time as Time
import qualified Penny.Tree.Flag as Flag
import qualified Penny.Tree.Number as Number
import Control.Applicative
import Text.Parsec.Text

data T
  = Time Time.T
  | Flag Flag.T
  | Number Number.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Time <$> Time.parser
  <|> Flag <$> Flag.parser
  <|> Number <$> Number.parser
