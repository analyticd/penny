module Penny.Copper.DateTime where

import Data.Time (
  minutesToTimeZone, TimeOfDay, makeTimeOfDayValid,
  localTimeToUTC, midnight, LocalTime ( LocalTime ),
  Day, fromGregorianValid, TimeZone )
import Text.Parsec (
  optionMaybe, char, try, digit, (<|>), option )
import Text.Parsec.Text ( Parser )
import Control.Monad ( replicateM, void )
import Data.Fixed ( Pico )

import qualified Penny.Lincoln.Bits as B

newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: TimeZone }

dateTime ::
  DefaultTimeZone
  -> Parser B.DateTime
dateTime (DefaultTimeZone dtz) = do
  d <- day
  maybeTime <- optionMaybe (try (char ' ' >> timeOfDay))
  (tod, tz) <- case maybeTime of
    Nothing -> return (midnight, dtz)
    (Just t) -> do
      maybeTz <- optionMaybe (try (char ' ' >> timeZone))
      case maybeTz of
        (Just zone) -> return (t, zone)
        Nothing -> return (t, dtz)
  let local = LocalTime d tod
      utc = localTimeToUTC tz local
  return $ B.DateTime utc

-- Format for dates is:
-- 2011/01/22 or 2011-01-22
-- followed by a time spec:
-- 16:42 -0400
-- or HMS:
-- 16:42:45 -0400
-- You can omit the time zone spec, in which case
-- the parser assumes the time is local to the timezone that is
-- passed in to the function (generally this will be the local time
-- the machine is in.)

digits1or2 :: Parser String
digits1or2 = do
  d1 <- digit
  d2 <- optionMaybe digit
  let r = case d2 of
        Nothing -> d1:[]
        (Just d) -> d1:d:[]
  return r

monthOrDayNum :: Parser Int
monthOrDayNum = do
  i <- digits1or2
  return $ read i

year :: Parser Integer
year = do
  i <- replicateM 4 digit
  return $ read i

day :: Parser Day
day = do
  let slash = void $ char '/' <|> char '-'
  y <- year
  slash
  m <- monthOrDayNum
  slash
  d <- monthOrDayNum
  case fromGregorianValid y m d of
    Nothing -> fail "invalid date"
    (Just da) -> return da
  
hoursMins :: Parser (Int, Int)
hoursMins = do
  h <- digits1or2
  void $ char ':'
  m <- replicateM 2 digit
  return (read h, read m)

secs :: Parser Pico
secs = do
  void $ char ':'
  s <- replicateM 2 digit
  let fi = fromIntegral :: Integer -> Pico
  return (fi . read $ s)

timeOfDay :: Parser TimeOfDay
timeOfDay = do
  (h, m) <- hoursMins
  let fi = fromIntegral :: Int -> Pico
  s <- option (fi 0) secs
  case makeTimeOfDayValid h m s of
    Nothing -> fail "invalid time of day"
    (Just tod) -> return tod

sign :: Parser (Int -> Int)
sign = let
  pos = char '+' >> return id
  neg = char '-' >> return negate
  in pos <|> neg

timeZone :: Parser TimeZone
timeZone = do
  s <- sign
  hh <- replicateM 2 digit
  mm <- replicateM 2 digit
  let hr = read hh
      mi = read mm
      mins = s (hr * 60 + mi)
      zone = minutesToTimeZone mins
  return zone

