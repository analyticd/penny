module Penny.Tree.Tag where

import qualified Penny.Tree.Asterisk as Asterisk
import Data.Sequence (Seq)
import qualified Penny.Tree.Tag.Char as Char
import Control.Applicative
import Text.Parsec.Text
import qualified Data.Sequence as S
import qualified Data.Text as X
import qualified Penny.Core.Tag as Tag
import Data.Foldable (toList)


data T = T Asterisk.T (Seq Char.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Asterisk.parser <*> fmap S.fromList (many Char.parser)

toCore :: T -> Tag.T
toCore (T _ sq) = Tag.T . X.pack . toList . fmap Char.toChar $ sq
