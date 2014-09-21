module Penny.Tree.LZ4 where

import qualified Penny.Core.Anna.DecDecs as DecDecs
import Data.Sequence (Seq)
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Tree.Parsec as P

data T a = T a DecDecs.T (Seq (a, DecDecs.T))
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa = T <$> pa <*> DecDecs.parser
  <*> P.seq ((,) <$> pa <*> DecDecs.parser)
