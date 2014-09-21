module Penny.Tree.Digits.D4 where

import qualified Penny.Tree.Digit as D
import Penny.Tree.Parsec

data T = T D.T D.T D.T D.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM4 T p p p p
  where
    p = D.parser
