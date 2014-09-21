module Penny.Tree.Commodity where

import qualified Penny.Tree.Caret as Caret
import qualified Penny.Tree.Commodity.Char as CharC
import Data.Sequence (Seq)
import Penny.Tree.Parsec

data T = T Caret.T (Seq CharC.T) Caret.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM3 T Caret.parser
                  (yarn "commodity character" CharC.fromChar)
                  Caret.parser
