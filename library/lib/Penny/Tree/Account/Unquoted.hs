module Penny.Tree.Account.Unquoted where

import qualified Penny.Tree.SubAccount.Unquoted.First as First
import qualified Penny.Tree.SubAccount.Unquoted.Next as Next
import Data.Sequence (Seq)
import Text.Parsec.Text
import Control.Applicative
import qualified Data.Sequence as S

data T = T First.T (Seq Next.T)
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> First.parser <*> fmap S.fromList (many Next.parser)
