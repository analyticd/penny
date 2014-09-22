module Penny.Core.Anna.SeqDecs where

import qualified Penny.Core.Anna.DecsGroup as DecsGroup
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Parsec.Text
import qualified Penny.Tree.Parsec as P

newtype T r = T { toSeq :: Seq (DecsGroup.T r) }
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser p = fmap T $ P.seq (DecsGroup.parser p)

empty :: T a
empty = T S.empty
