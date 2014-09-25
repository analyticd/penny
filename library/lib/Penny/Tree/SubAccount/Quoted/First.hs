module Penny.Tree.SubAccount.Quoted.First where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Text.Parsec.Text
import Control.Applicative

data T = T (Seq Char.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = fmap (T . S.fromList) $ many Char.parser
