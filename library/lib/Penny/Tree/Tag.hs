module Penny.Tree.Tag where

import qualified Penny.Tree.Asterisk as Asterisk
import Data.Sequence (Seq)
import qualified Penny.Tree.Tag.Char as Char
import Control.Applicative
import Text.Parsec.Text
import qualified Data.Sequence as S

data T = T Asterisk.T (Seq Char.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Asterisk.parser <*> fmap S.fromList (many Char.parser)
