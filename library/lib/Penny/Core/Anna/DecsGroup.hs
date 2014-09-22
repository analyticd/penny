module Penny.Core.Anna.DecsGroup where

import qualified Penny.Core.Anna.DecDecs as DecDecs
import Text.Parsec.Text
import Control.Applicative

data T r = T
  { grouper :: r
  , decDecs :: DecDecs.T
  } deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser p = T <$> p <*> DecDecs.parser
