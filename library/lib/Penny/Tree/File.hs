module Penny.Tree.File where

import Data.Sequence (Seq)
import qualified Penny.Tree.EOF as EOF
import qualified Penny.Tree.Line as Line
import Text.Parsec.Text
import qualified Data.Sequence as S
import Control.Applicative

data T = T (Seq Line.T) EOF.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> fmap S.fromList (many Line.parser) <*> EOF.parser
