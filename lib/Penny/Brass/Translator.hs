module Penny.Brass.Translator where

import qualified Data.Text as X
import qualified Data.Foldable as F
import qualified Penny.Brass.Start as T
import qualified Penny.Lincoln.Strict as S

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

data QtyToken = QtyDigits !X.Text
                | QtyRadix
                deriving (Show, Eq)

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

data QtyStringData =
  QtyStringData { qtyStr :: !X.Text
                , expSize :: !Int 
                , sawRadix :: !Bool }
  | TooManyRadixError

getQtyStringData ::
  RadGroup
  -> T.QtyItem
  -> S.List T.QtyItem
  -> QtyStringData
getQtyStringData (RadGroup r g) qi l = d' where
  d = QtyStringData X.empty 0
  d' = undefined
  
-- | Translate a list of 

{-
addItemToQsd :: RadGroup -> QtyStringData -> T.QtyItem -> QtyStringData
addItemToQsd _ TooManyRadixError _ = TooManyRadixError
addItemToQsd (RadGroup r g) (QtyStringData qstr e sawRad) i =
  case i of
    QtyDigits x -> let
      e' = if sawRad then e + X.length x else e
      in QtyStringData (x `X.append` qstr) e sawRad
    QtyPeriod ->
      case (r, g) of
        (RComma, 
        RPeriod ->
          if sawRad
          then TooManyRadixError
          else QtyStringData qstr e True
        RComma -> case g of
          GPeriod ->
            if sawRad
            then TooManyRadixError
            else QtySringData qstr e True
          _ -> QtyStringData qstr e sawRad
      
          GComma -> QtyStringData qstr e sawRad
          GSpace -> 
                                  

    QtyPeriod -> case (r, g) of
      (RPeiod, _) -> QtyStringData (qtyStr qsd, 
    
  addItem qsd i = case i of
    
    case (r, g) of
    (RPeriod, RComma) -> case i of
-}  
