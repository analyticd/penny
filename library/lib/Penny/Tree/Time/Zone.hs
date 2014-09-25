module Penny.Tree.Time.Zone where

import qualified Penny.Tree.Hyphen as Hyphen
import qualified Penny.Tree.Plus as Plus
import qualified Penny.Tree.Digits.D4 as D4
import qualified Penny.Tree.Brace.Close as Close
import Text.Parsec.Text
import Control.Applicative

data T = T (Either Hyphen.T Plus.T) D4.T Close.T
  deriving (Eq, Ord, Show)

parser :: Parser T
parser = T <$> (fmap Left Hyphen.parser <|> fmap Right Plus.parser)
  <*> D4.parser <*> Close.parser
