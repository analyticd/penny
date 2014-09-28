module Penny.Tree.Payee.Posting where

import qualified Penny.Tree.Tilde as Tilde
import Data.Sequence (Seq)
import qualified Penny.Tree.Payee.Posting.Char as Char
import Text.Parsec.Text
import Control.Applicative
import qualified Data.Sequence as S
import Data.Foldable (toList)
import qualified Data.Text as X
import qualified Penny.Core.Payee as Payee

data T = T Tilde.T (Seq Char.T) Tilde.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Tilde.parser <*> fmap S.fromList (many Char.parser)
  <*> Tilde.parser

toCore :: T -> Payee.T
toCore (T _ sq _) = Payee.T . X.pack . toList . fmap Char.toChar $ sq
