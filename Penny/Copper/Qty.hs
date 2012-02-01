module Penny.Parser.Qty (
  Radix, Separator, unRadix,
  unSeparator, qty, radixAndSeparator ) where

import Text.Parsec ( char, digit, (<|>), many, try )
import Text.Parsec.Text ( Parser )

import Penny.Bits.Qty ( Qty, partialNewQty )

newtype Radix = Radix { unRadix :: Char }
newtype Separator = Separator { unSeparator :: Char }

radixAndSeparator :: Char -> Char -> (Radix, Separator)
radixAndSeparator r s =
  if r == s
  then error "radix and separator must be different characters"
  else (Radix r, Separator s)

radix :: Radix -> Parser Char
radix (Radix r) = char r >> return '.'

qtyDigit :: Separator -> Parser Char
qtyDigit (Separator separator) = digit <|> (char separator >> digit)

qty :: Radix -> Separator -> Parser Qty
qty rdx sep = let
  digitRun = do
    c <- digit
    cs <- many (qtyDigit sep)
    return (c : cs)
  withPoint = do
    l <- digitRun
    p <- radix rdx
    r <- digitRun
    return (l ++ (p : r))
  withoutPoint = digitRun
  in do
    s <- try withPoint <|> withoutPoint
    let d = read s
    return $ partialNewQty d

