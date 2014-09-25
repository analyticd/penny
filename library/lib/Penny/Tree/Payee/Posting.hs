module Penny.Tree.Payee.Posting where

import qualified Penny.Tree.Tilde as Tilde
import Data.Sequence (Seq)
import qualified Penny.Tree.Payee.Posting.Char as Char
import Text.Parsec.Text
import Control.Applicative
import qualified Data.Sequence as S

data T = T Tilde.T (Seq Char.T) Tilde.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Tilde.parser <*> fmap S.fromList (many Char.parser)
  <*> Tilde.parser
