module Penny.Tree.Time.Time3 where

import qualified Penny.Tree.Colon as Colon
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Time.Zone as Zone
import qualified Penny.Tree.Time.Time4 as Time4
import Text.Parsec.Text
import Control.Applicative

data T
  = Seconds Colon.T D2.T Time4.T
  | Zone Zone.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Seconds <$> Colon.parser <*> D2.parser <*> Time4.parser
  <|> Zone <$> Zone.parser
