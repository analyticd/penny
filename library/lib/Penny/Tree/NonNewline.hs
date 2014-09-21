module Penny.Tree.NonNewline
  ( T
  , toChar
  , fromChar
  , parser
  ) where

import Text.Parsec.Text
import qualified Penny.Tree.Parsec as P

newtype T = T { toChar :: Char }
  deriving (Eq, Ord, Show)

fromChar :: Char -> Maybe T
fromChar c
  | c /= '\n' = Just $ T c
  | otherwise = Nothing

parser :: Parser T
parser = P.accept "non-newline" fromChar
