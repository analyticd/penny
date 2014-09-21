module Penny.Tree.Hyphen where

import qualified Penny.Tree.Parsec as P
import Control.Applicative ((<$))

data T = T
  deriving (Eq, Ord, Show)

parser :: P.Parser T
parser = T <$ P.char '-'
