module Penny.Tree.LR1 where

import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Tree.LZ3 as LZ3
import qualified Penny.Tree.LZ4 as LZ4
import Text.Parsec.Text
import Control.Applicative

data T a
  = Zero Zeroes.T (Maybe (LZ3.T a))
  | Novem NovDecs.T (Maybe (LZ4.T a))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser p
  = Zero <$> Zeroes.parser <*> optional (LZ3.parser p)
  <|> Novem <$> NovDecs.parser <*> optional (LZ4.parser p)
