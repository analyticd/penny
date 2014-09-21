module Penny.Tree.Masuno1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Tree.Masuno1Radix1 as Masuno1Radix1
import Text.Parsec.Text
import Control.Applicative

data T a
  = T (Radix.T a) (Maybe (Masuno1Radix1.T a))
  deriving (Eq, Ord, Show)

parser :: Parser (Radix.T a) -> Parser a -> Parser (T a)
parser pr pa =
  T <$> pr <*> optional (Masuno1Radix1.parser pa)
