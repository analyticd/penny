module Penny.Tree.Memo.Transaction where

import Data.Sequence (Seq)
import qualified Penny.Tree.NonNewline as NonNewline
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Semicolon as Semicolon
import qualified Data.Sequence as S
import Text.Parsec.Text
import Control.Applicative

data T = T Semicolon.T (Seq NonNewline.T) Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Semicolon.parser
  <*> fmap S.fromList (many NonNewline.parser)
  <*> Newline.parser
