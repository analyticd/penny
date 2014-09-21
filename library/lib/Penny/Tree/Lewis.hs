module Penny.Tree.Lewis where

import qualified Deka.Native.Abstract as N
import Data.Sequence (Seq)
import qualified Penny.Tree.Masuno1 as Masuno1
import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Tree.LZ1 as LZ1
import Text.Parsec.Text
import qualified Penny.Tree.Parsec as P
import Control.Applicative
import qualified Penny.Core.Anna.Radix as Radix

-- | The root of parse trees for number representations.
-- BROKEN - needs lead radix
data T a
  = Novem N.Novem (Seq N.Decem) (Maybe (Masuno1.T a))
  | Zero Zero.T (Maybe (LZ1.T a))
  deriving (Eq, Ord, Show)

parser :: Parser (Radix.T a) -> Parser a -> Parser (T a)
parser pr pa =
  Novem <$> P.novem <*> P.seq (P.decem)
        <*> optional (Masuno1.parser pr pa)

  <|> Zero <$> Zero.parser <*> optional (LZ1.parser pr pa)
