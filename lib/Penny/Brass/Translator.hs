module Penny.Brass.Translator where

import Control.Monad (guard)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Decimal as D
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (mapMaybe)
import qualified Data.Text as X
import qualified Data.Time as Time
import qualified Data.Foldable as F
import qualified Penny.Brass.Start as T
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Strict as S
import Penny.Lincoln.Strict (List((:|:)))
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

newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: L.TimeZoneOffset }
  deriving (Eq, Show)

utcDefault :: DefaultTimeZone
utcDefault = DefaultTimeZone L.noOffset

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

-- | Reads an Int from a text that has only digits. Is bottom if the
-- text contains non-digit characters. Is Nothing if the number is an
-- integer but is too big to be an Int.
readInt :: X.Text -> Maybe Int
readInt x = let big = readInteger x in
  if (big >= fromIntegral (maxBound :: Int))
  then Nothing
  else Just (fromIntegral big)

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
  | BadDateTime C.Location

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

txtConcat :: S.List X.Text -> X.Text
txtConcat = F.foldl' X.append X.empty

timeZoneMins :: X.Text -> Maybe Int
timeZoneMins x =
  if X.length x /= 4
  then Nothing
  else
    let hs = X.take 2 x
        ms = X.drop 2 x
    in do
      h <- readInt hs
      m <- readInt ms
      return $ h * 60 + m

timeZoneOffset :: T.TimeZone -> Maybe L.TimeZoneOffset
timeZoneOffset (T.TimeZone s x) = do
  i <- timeZoneMins x
  let iSigned = case s of
        T.TzPlus -> i
        T.TzMinus -> negate i
  L.minsToOffset (fromIntegral iSigned)

timeOfDay :: T.HoursMinsSecs -> Maybe Time.TimeOfDay
timeOfDay (T.HoursMinsSecs (T.HoursMins th tm) mayTSecs) = do
  h <- readInt th
  m <- readInt tm
  s <- case mayTSecs of
    S.Nope -> return 0
    S.Here (T.Secs secs) -> readInt secs
  guard (h < 24 && m < 60 && s < 60)
  return $ Time.TimeOfDay h m (fromIntegral s)

localDay :: T.Date -> Maybe Time.Day
localDay (T.Date ty tm td) = do
  let y = readInteger ty
  m <- readInt tm
  d <- readInt td
  Time.fromGregorianValid y m d

dateTime :: DefaultTimeZone
            -> T.DateTime
            -> Ex.Exceptional Error L.DateTime
dateTime (DefaultTimeZone dtz) (T.DateTime l d maybeTimeAndOrZone) =
  Ex.fromMaybe (BadDateTime l) $ do
    ld <- localDay d
    (t, z) <- case maybeTimeAndOrZone of
      S.Nope -> return (Time.TimeOfDay 0 0 0, dtz)
      S.Here timeAndOrZone -> case timeAndOrZone of
        T.TimeMaybeZone hmst maybeZone -> do
          tod <- timeOfDay hmst
          case maybeZone of
            S.Nope -> return (tod, dtz)
            (S.Here tzt) -> do
              tz <- timeZoneOffset tzt
              return (tod, tz)
        T.ZoneOnly tzt -> do
          tzo <- timeZoneOffset tzt
          return (Time.TimeOfDay 0 0 0, tzo)
    return (L.DateTime (Time.LocalTime ld t) z)

newtype Comment = Comment X.Text
                  deriving (Show, Eq)

comment :: T.Comment -> Comment
comment (T.Comment ts) = Comment (txtConcat ts)

number :: T.Number -> L.Number
number (T.Number ts) =
  L.Number
  . L.unsafeTextToNonEmpty
  . txtConcat
  $ ts

flag :: T.Flag -> L.Flag
flag (T.Flag ts) =
  L.Flag . L.unsafeTextToNonEmpty . txtConcat $ ts

payee :: T.Payee -> L.Payee
payee (T.Payee p ps) =
  L.Payee . L.unsafeTextToNonEmpty
  . txtConcat $ (p :|: ps)

subAccount :: T.SubAccount -> L.SubAccountName
subAccount (T.SubAccount a as) =
  L.SubAccountName . L.unsafeTextToNonEmpty
  . txtConcat $ (a :|: as)

account :: T.Account -> L.Account
account (T.Account s ss) = L.Account ((subAccount s) :| ss') where
  ss' = F.toList . fmap subAccount $ ss

tag :: T.Tag -> L.Tag
tag (T.Tag t ts) =
  L.Tag . L.unsafeTextToNonEmpty
  . txtConcat $ (t :|: ts)

tags :: T.Tags -> L.Tags
tags (T.Tags ls) = L.Tags . F.toList . fmap tag $ ls

subCommodity :: T.SubCommodity -> L.SubCommodity
subCommodity (T.SubCommodity c cs) =
  L.SubCommodity . L.unsafeTextToNonEmpty
  . txtConcat $ (c :|: cs)

commodity :: T.Commodity -> L.Commodity
commodity (T.Commodity s1 ss) = L.Commodity (s1' :| ss') where
  s1' = subCommodity s1
  ss' = F.toList . fmap subCommodity $ ss
  
