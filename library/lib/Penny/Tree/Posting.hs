module Penny.Tree.Posting where

import qualified Penny.Tree.Posting.Item as Item
import qualified Penny.Tree.PreSpace as PreSpace
import qualified Penny.Tree.Newline as Newline
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Parsec.Text
import Control.Applicative

data T = T (PreSpace.T Item.T) (Seq (PreSpace.T Item.T)) Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser
  = T
  <$> PreSpace.parser Item.parser
  <*> fmap S.fromList (many (PreSpace.parser Item.parser))
  <*> Newline.parser
