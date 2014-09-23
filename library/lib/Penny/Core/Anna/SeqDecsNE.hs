module Penny.Core.Anna.SeqDecsNE where

import Data.Sequence ((<|))
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup
import Text.Parsec.Text
import Control.Applicative

data T r = T
  { group1 :: DecsGroup.T r
  , groupsRest :: SeqDecs.T r
  } deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser p = T <$> DecsGroup.parser p <*> SeqDecs.parser p

toSeqDecs :: T r -> SeqDecs.T r
toSeqDecs (T g (SeqDecs.T sq)) = SeqDecs.T (g <| sq)
