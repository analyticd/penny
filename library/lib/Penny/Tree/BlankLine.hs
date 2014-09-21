module Penny.Tree.BlankLine where

import qualified Penny.Tree.Newline as Newline
import Penny.Tree.Parsec

newtype T = T Newline.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap T Newline.parser
