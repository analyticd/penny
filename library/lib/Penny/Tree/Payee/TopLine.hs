module Penny.Tree.Payee.TopLine where

import qualified Penny.Tree.Payee.TopLine.Char.First as First
import qualified Penny.Tree.Payee.TopLine.Char.Next as Next
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Control.Applicative
import Text.Parsec.Text

data T = T
  { first :: First.T
  , rest :: Seq Next.T
  } deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> First.parser <*> fmap S.fromList (many Next.parser)
