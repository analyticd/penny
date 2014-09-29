module Penny.Tree.LZ2 where

import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Tree.LZ3 as LZ3
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE
import Text.Parsec.Text
import Control.Applicative

data T a
  = Zero Zeroes.T (Maybe (LZ3.T a))
  | Novem NovDecs.T (Maybe (SeqDecsNE.T a))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa =
  Zero <$> Zeroes.parser <*> optional (LZ3.parser pa)
  <|> Novem <$> NovDecs.parser <*> optional (SeqDecsNE.parser pa)
