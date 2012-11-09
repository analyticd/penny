module Penny.Copper.Number (isNumChar, number, render) where

import Data.Text ( pack, cons, snoc, Text )
import qualified Data.Text as X
import Text.Parsec ( char, satisfy, many1, between, (<?>))
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln.Bits as B

isNumChar :: Char -> Bool
isNumChar c = allowed && not banned where
  allowed = U.unicodeAll c || U.asciiAll c
  banned = c == ')'

number :: Parser B.Number
number = between (char '(') (char ')') p <?> "number"
  where
    p = fmap (B.Number . pack) (many1 (satisfy isNumChar))


render :: B.Number -> Maybe Text
render (B.Number t) =
  if X.all isNumChar t
  then Just $ '(' `cons` t `snoc` ')'
  else Nothing
