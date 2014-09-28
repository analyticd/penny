module Penny.Tree.SubAccount.Quoted.Next where

import qualified Penny.Tree.SubAccount.Quoted.Char as Char
import qualified Penny.Tree.Colon as Colon
import Data.Sequence (Seq)
import Control.Applicative
import qualified Data.Sequence as S
import Text.Parsec.Text
import qualified Data.Text as X
import qualified Penny.Core.SubAccount as SubAccount
import Data.Foldable (toList)

data T = T Colon.T (Seq Char.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Colon.parser <*> fmap S.fromList (many Char.parser)

toCore :: T -> SubAccount.T
toCore (T _ sq) = SubAccount.T . X.pack . toList . fmap Char.toChar $ sq
