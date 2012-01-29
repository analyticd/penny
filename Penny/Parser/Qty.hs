module Penny.Parser.Qty where

import Text.Parsec ( char, digit, (<|>), many, try )
import Text.Parsec.Text ( Parser )

import Penny.Bits.Qty ( Qty, partialNewQty )

newtype Radix = Radix { unRadix :: Char }
newtype Separator = Separator { unSeparator :: Char }

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

