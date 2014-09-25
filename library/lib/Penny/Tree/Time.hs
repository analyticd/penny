module Penny.Tree.Time where

import qualified Penny.Tree.Brace.Open as Open
import qualified Penny.Tree.Colon as Colon
import qualified Penny.Tree.Digits.D1or2 as D2
import qualified Penny.Tree.Time.Time2 as Time2
import Text.Parsec.Text
import Control.Applicative

data T = T Open.T D2.T Colon.T D2.T Time2.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Open.parser <*> D2.parser <*> Colon.parser
  <*> D2.parser <*> Time2.parser
