module Penny.Tree.Comment where

import qualified Penny.Tree.Comment.Char as Char
import qualified Penny.Tree.Octothorpe as Octothorpe
import Data.Sequence (Seq)
import qualified Penny.Tree.Newline as Newline
import qualified Penny.Tree.Parsec as P

data T = T Octothorpe.T (Seq Char.T) Newline.T
  deriving (Eq, Ord, Show)

parser :: P.Parser T
parser = P.liftM3 T Octothorpe.parser
                  (P.seq (P.accept "comment character" Char.fromChar))
                  Newline.parser
