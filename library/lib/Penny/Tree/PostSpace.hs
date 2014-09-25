module Penny.Tree.PostSpace where

import qualified Penny.Tree.Spaces as Spaces
import Control.Applicative
import Text.Parsec.Text

data T a = T
  { payload :: a
  , spaces :: Spaces.T
  } deriving (Eq, Ord, Show)

instance Functor T where
  fmap f (T p s) = T (f p) s

parser :: Parser a -> Parser (T a)
parser p = T <$> p <*> Spaces.parser
