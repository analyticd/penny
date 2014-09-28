module Penny.Tree.Commodity where

import qualified Penny.Tree.Caret as Caret
import qualified Penny.Tree.Commodity.Char as CharC
import Data.Sequence (Seq)
import Penny.Tree.Parsec
import qualified Data.Text as X
import qualified Penny.Core.Commodity as Commodity
import Data.Foldable (toList)


data T = T Caret.T (Seq CharC.T) Caret.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = liftM3 T Caret.parser
                  (yarn "commodity character" CharC.fromChar)
                  Caret.parser

toCore :: T -> Commodity.T
toCore (T _ sq _) = Commodity.T . X.pack . toList
  . fmap CharC.toChar $ sq
