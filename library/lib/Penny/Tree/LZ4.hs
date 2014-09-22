module Penny.Tree.LZ4 where

import Data.Sequence (Seq)
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import qualified Penny.Core.Anna.DecsGroup as DecsGroup

data T a = T (DecsGroup.T a) (Seq (DecsGroup.T a))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa = T <$> DecsGroup.parser pa <*> P.seq (DecsGroup.parser pa)
