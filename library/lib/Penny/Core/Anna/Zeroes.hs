module Penny.Core.Anna.Zeroes where

import qualified Penny.Natural.NonZero as NonZero
import qualified Penny.Natural.Unsigned as Unsigned
import Text.Parsec.Text
import Text.Parsec (char)
import Control.Applicative

-- | One or more zeroes.
newtype T = T { toNonZero :: NonZero.T }
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = f <$ char '0' <*> many (char '0')
  where
    f zs = T $ NonZero.addUnsigned NonZero.one (Unsigned.length zs)
