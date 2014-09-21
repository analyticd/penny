module Penny.Tree.TopLine where

import qualified Penny.Tree.Payee.TopLine as Payee
import qualified Penny.Tree.Newline as Newline
import Data.Sequence (Seq)
import qualified Penny.Tree.PostSpace as PostSpace
import qualified Penny.Tree.TopLine.Item as Item

data T = T (Seq (PostSpace.T Item.T)) (Maybe Payee.T) Newline.T
  deriving (Eq, Ord, Show)
