module Penny.Tree.SubAccount.Unquoted.First where

import qualified Penny.Tree.SubAccount.Unquoted.Char.First as FirstC
import qualified Penny.Tree.SubAccount.Unquoted.Char.Next as NextC
import Data.Sequence (Seq)
import Control.Applicative
import qualified Data.Sequence as S
import Text.Parsec.Text

data T = T
  { first :: FirstC.T
  , rest :: Seq NextC.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> FirstC.parser <*> fmap S.fromList (many NextC.parser)
