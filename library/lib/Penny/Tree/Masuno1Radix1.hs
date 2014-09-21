module Penny.Tree.Masuno1Radix1 where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import Text.Parsec.Text

data T a
  = T N.Decem (Seq N.Decem) (Seq (a, N.Decem, (Seq N.Decem)))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa =
  T <$> P.decem <*> P.seq (P.decem)
  <*> P.seq ((,,) <$> pa <*> P.decem <*> P.seq P.decem)
