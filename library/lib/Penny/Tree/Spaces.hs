module Penny.Tree.Spaces where

import qualified Penny.Natural.NonZero as NonZero
import qualified Penny.Natural.Unsigned as Unsigned
import Text.Parsec.Text
import Text.Parsec (char)
import Control.Applicative

newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = f <$ char ' ' <*> many (char ' ')
  where
    f ls = T $ NonZero.one `NonZero.addUnsigned` (Unsigned.length ls)
