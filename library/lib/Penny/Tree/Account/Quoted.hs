module Penny.Tree.Account.Quoted where

import Data.Sequence (Seq)
import qualified Penny.Tree.SubAccount.Quoted.First as First
import qualified Penny.Tree.SubAccount.Quoted.Next as Next
import qualified Penny.Tree.Brace.Open as Open
import qualified Penny.Tree.Brace.Close as Close
import Text.Parsec.Text
import qualified Data.Sequence as S
import Data.Sequence ((<|))
import Control.Applicative
import qualified Penny.Core.Account as Account

data T = T Open.T First.T (Seq Next.T) Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> Open.parser <*> First.parser
  <*> fmap S.fromList (many Next.parser) <*> Close.parser

toCore :: T -> Account.T
toCore (T _ fi nx _)
  = Account.T $ First.toCore fi <| fmap Next.toCore nx
