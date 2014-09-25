module Penny.Tree.Time.Time4 where

import qualified Penny.Tree.Time.Zone as Zone
import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Brace.Close as Close
import Text.Parsec.Text
import Control.Applicative

data T
  = Zone Zone.T
  | Space Space.T Zone.T
  | Close Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = Zone <$> Zone.parser
  <|> Space <$> Space.parser <*> Zone.parser
  <|> Close <$> Close.parser
