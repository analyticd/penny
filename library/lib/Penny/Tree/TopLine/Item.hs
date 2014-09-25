module Penny.Tree.TopLine.Item where

import qualified Penny.Tree.Date as Date
import qualified Penny.Tree.Time as Time
import qualified Penny.Tree.Flag as Flag
import qualified Penny.Tree.Number as Number
import Control.Applicative
import Text.Parsec.Text

data T
  = T0 Date.T
  | T1 Time.T
  | T2 Flag.T
  | T3 Number.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T0 <$> Date.parser
  <|> T1 <$> Time.parser
  <|> T2 <$> Flag.parser
  <|> T3 <$> Number.parser
