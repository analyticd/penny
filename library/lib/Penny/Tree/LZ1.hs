module Penny.Tree.LZ1 where

import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Tree.LZ2 as LZ2
import Control.Applicative
import Text.Parsec.Text

data T a
  = T (Radix.T a) (Maybe (LZ2.T a))
  deriving (Eq, Ord, Show)

parser :: Parser (Radix.T a) -> Parser a -> Parser (T a)
parser pr pa = T <$> pr <*> optional (LZ2.parser pa)
