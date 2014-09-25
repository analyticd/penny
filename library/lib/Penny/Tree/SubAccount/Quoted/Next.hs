module Penny.Tree.SubAccount.Quoted.Next where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import qualified Penny.Tree.Colon as Colon
import Data.Sequence (Seq)
import Control.Applicative
import qualified Data.Sequence as S
import Text.Parsec.Text

data T = T Colon.T (Seq Char.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Colon.parser <*> fmap S.fromList (many Char.parser)
