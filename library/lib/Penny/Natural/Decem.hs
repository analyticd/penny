module Penny.Natural.Decem where

import Control.Applicative ((<$), (<$>))
import qualified Penny.Natural.Novem as Novem
import Text.Parsec
import Text.Parsec.Text

data T
  = D0
  | Novem Novem.T
  deriving (Eq, Ord, Show)

toInt :: Integral a => T -> a
toInt x = case x of
  D0 -> 0
  Novem n -> Novem.toInt n

fromInt :: Integral a => a -> Maybe T
fromInt i = case i of
  0 -> Just D0
  x -> fmap Novem $ Novem.fromInt x

parser :: Parser T
parser
  = D0 <$ char '0'
  <|> Novem <$> Novem.parser
