module Penny.Tree.Digits.D1or2 where

import qualified Penny.Tree.Digit as D

import Penny.Tree.Parsec

data T = T D.T (Maybe D.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM2 T D.parser (optional D.parser)
