module Penny.Tree.Flag where

import qualified Penny.Tree.Square.Open as Open
import qualified Penny.Tree.Square.Close as Close
import Data.Sequence (Seq)
import Data.Foldable (toList)
import qualified Penny.Tree.Flag.Char as Char
import qualified Penny.Core.Flag as Flag
import qualified Data.Text as X

import Penny.Tree.Parsec
import qualified Penny.Tree.Parsec as P

data T = T Open.T (Seq Char.T) Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM3 T Open.parser (P.seq Char.parser) Close.parser

toCore :: T -> Flag.T
toCore (T _ sq _) = Flag.T . X.pack . toList
  . fmap Char.toChar $ sq
