module Penny.Tree.Masuno1Radix1 where

import Control.Applicative
import Text.Parsec.Text
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup

data T a
  = T DecDecs.T [DecsGroup.T a]
  deriving (Eq, Ord, Show)

parser :: Parser a -> Parser (T a)
parser pa =
  T <$> DecDecs.parser
  <*> many (DecsGroup.parser pa)
