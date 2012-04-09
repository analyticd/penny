module Penny.Brass.Translator where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Decimal as D
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Penny.Brass.Start as T
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Strict as S
import qualified Penny.Brass.Scanner as C

data Radix = RComma | RPeriod deriving (Eq, Show)
data Grouper = GComma | GPeriod | GSpace deriving (Eq, Show)

data RadGroup = RadGroup Radix Grouper deriving (Eq, Show)

-- | Radix is period, grouping is comma
periodComma :: RadGroup
periodComma = RadGroup RPeriod GComma

-- | Radix is period, grouping is space
periodSpace :: RadGroup
periodSpace = RadGroup RPeriod GSpace

-- | Radix is comma, grouping is period
commaPeriod :: RadGroup
commaPeriod = RadGroup RComma GPeriod

-- | Radix is comma, grouping is space
commaSpace :: RadGroup
commaSpace = RadGroup RComma GSpace

data QtyReader = QtyReader { exponent :: !Int
                           , total :: !Integer }

-- | Reads a single digit of a number string. Adjusts the QtyReader as
-- appropriate.
readDigit :: QtyReader -> Char -> QtyReader
readDigit (QtyReader ex tot) c = QtyReader (ex - 1) tot' where
  tot' = tot + newValue
  newValue = digit * 10 ^ ex
  digit = case c of {
    '0' -> 0; '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4; '5' -> 5;
    '6' -> 6; '7' -> 7; '8' -> 8; '9' -> 9;
    _ -> error "readDigit error" }

-- | Reads an integer from a text that has only digits. Is bottom if
-- the text contains non-digit characters.
readInteger :: X.Text -> Integer
readInteger x = total qr' where
  qr = QtyReader (X.length x - 1) 0
  qr' = X.foldl' readDigit qr x

data QtyStrItem = Radix | Digits !X.Text

qtyStrItem ::
  RadGroup
  -> T.QtyItem
  -> Maybe QtyStrItem
qtyStrItem (RadGroup r _) qi = case qi of
  T.QtyDigits x -> Just (Digits x)
  T.QtyPeriod -> case r of RPeriod -> Just Radix; _ -> Nothing
  T.QtyComma -> case r of RComma -> Just Radix; _ -> Nothing
  T.QtySpace -> Nothing

data QtyReadAcc = QtyReadAcc { _numStr :: !X.Text
                             , _expLen :: !Int
                             , _seenRadix :: !Bool }
                  | TooManyRadixError

readQtyItem :: QtyReadAcc -> QtyStrItem -> QtyReadAcc
readQtyItem a i = case a of
  TooManyRadixError -> TooManyRadixError
  QtyReadAcc n e s -> case i of
    Radix -> if s
             then TooManyRadixError
             else QtyReadAcc n e True
    Digits x ->
      let e' = if s then e else e + X.length x in
      QtyReadAcc (n `X.append` x) e' s

data Error =
  MultipleRadixError C.Location
  | ZeroQtyError C.Location
  | ExponentTooBig C.Location

readQty :: RadGroup -> T.Qty -> Ex.Exceptional Error L.Qty
readQty rg (T.Qty l i1 ir) =
  let is = i1: (F.toList ir)
      acc = QtyReadAcc X.empty 0 False
      acc' = F.foldl' readQtyItem acc
             . mapMaybe (qtyStrItem rg)
             $ is
  in case acc' of
    TooManyRadixError -> Ex.Exception (MultipleRadixError l)
    QtyReadAcc n e _ -> r where
      r | e > 255 = Ex.Exception (ExponentTooBig l)
        | int == 0 = Ex.Exception (ZeroQtyError l)
        | otherwise = Ex.Success . L.partialNewQty
                      . D.Decimal e' $ int
        where
          e' = fromIntegral e
          int = readInteger n
