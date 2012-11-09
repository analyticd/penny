module Penny.Copper.Flag (flag, isFlagChar, render) where

import Control.Applicative ((<$>))
import Data.Text ( pack, cons, snoc )
import qualified Data.Text as X
import Text.Parsec ( char, satisfy, many1, between, (<?>))
import Text.Parsec.Text ( Parser )

import qualified Penny.Copper.Util as U
import qualified Penny.Lincoln.Bits as B

isFlagChar :: Char -> Bool
isFlagChar c = allowed && not banned
  where
    allowed = U.unicodeAll c || U.asciiAll c
    banned = c == ']'

flag :: Parser B.Flag
flag = between (char '[') (char ']') p <?> "flag" where
  p = (B.Flag . pack) <$> many1 (satisfy isFlagChar)


render :: B.Flag -> Maybe X.Text
render (B.Flag fl) =
  if X.all isFlagChar fl
  then Just $ '[' `cons` fl `snoc` ']'
  else Nothing
