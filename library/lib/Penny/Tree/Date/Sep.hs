module Penny.Tree.Date.Sep where

import qualified Penny.Tree.Hyphen as Hyphen
import qualified Penny.Tree.Solidus as Solidus
import qualified Penny.Tree.Parsec as P

newtype T = T (Either Hyphen.T Solidus.T)
  deriving (Eq, Ord, Show)

parser :: P.Parser T
parser = fmap T $ P.either Hyphen.parser Solidus.parser
