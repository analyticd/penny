module Penny.Tree.Masuno1Radix1 where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)
import Control.Applicative
import qualified Penny.Tree.Parsec as P
import Text.Parsec.Text
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup

data T a
  = T DecDecs.T (Seq (DecsGroup.T a))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa =
  T <$> DecDecs.parser
  <*> P.seq (DecsGroup.parser pa)
