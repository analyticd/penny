module Penny.Tree.Memo.Posting where

import Data.Sequence (Seq)
import qualified Penny.Tree.NonNewline as NonNewline
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Semicolon as Semicolon
import qualified Penny.Tree.Spaces as Spaces
import Control.Applicative
import Text.Parsec.Text
import qualified Data.Sequence as S

data T = T Spaces.T Semicolon.T (Seq NonNewline.T) Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Spaces.parser <*> Semicolon.parser
  <*> fmap S.fromList (many NonNewline.parser) <*> Newline.parser
