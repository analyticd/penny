module Penny.Natural.Novem where

import Control.Applicative ((<$))
import Text.Parsec
import Text.Parsec.Text

data T
  = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Ord, Show)

toInt :: Integral a => T -> a
toInt x = case x of
  { D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5; D6 -> 6;
    D7 -> 7; D8 -> 8; D9 -> 9 }

fromInt :: Integral a => a -> Maybe T
fromInt i = case i of
  { 1 -> Just D1; 2 -> Just D2; 3 -> Just D3; 4 -> Just D4;
    5 -> Just D5; 6 -> Just D6; 7 -> Just D7; 8 -> Just D8;
    9 -> Just D9; _ -> Nothing }

parser :: Parser T
parser
  = D1 <$ char '1'
  <|> D2 <$ char '2'
  <|> D3 <$ char '3'
  <|> D4 <$ char '4'
  <|> D5 <$ char '5'
  <|> D6 <$ char '6'
  <|> D7 <$ char '7'
  <|> D8 <$ char '8'
  <|> D9 <$ char '9'
