module Penny.Tree.Time.Time2 where

import qualified Penny.Tree.Space as Space
import qualified Penny.Tree.Time.Time3 as Time3
import qualified Penny.Tree.Brace.Close as Close
import Text.Parsec.Text
import Control.Applicative

data T
  = End Close.T
  | Space Space.T Time3.T
  | Time3 Time3.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = fmap End Close.parser
  <|> Space <$> Space.parser <*> Time3.parser
  <|> fmap Time3 Time3.parser
