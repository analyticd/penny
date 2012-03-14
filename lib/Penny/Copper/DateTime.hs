module Penny.Copper.DateTime where

import Control.Applicative ((<$>), (<*>))
import Data.Time (
  minutesToTimeZone, TimeOfDay, makeTimeOfDayValid,
  midnight, LocalTime ( LocalTime ),
  Day, fromGregorianValid, TimeZone )
import qualified Data.Time as T
import Text.Parsec (
  optionMaybe, char, digit, (<|>), option, (<?>))
import Text.Parsec.Text ( Parser )
import Control.Monad ( replicateM, void )
import Data.Fixed ( Pico )

import qualified Penny.Lincoln.Bits as B

newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: TimeZone }

dateTime ::
  DefaultTimeZone
  -> Parser B.DateTime
dateTime (DefaultTimeZone dtz) = p <?> "date" where
  p = do
    d <- day
    _ <- char ' '
    maybeTime <- optionMaybe timeOfDay
    (tod, tz) <- case maybeTime of
      Nothing -> return (midnight, dtz)
      (Just t) -> do
        _ <- char ' '
        maybeTz <- optionMaybe timeZone
        case maybeTz of
          (Just zone) -> return (t, zone)
          Nothing -> return (t, dtz)
    let local = LocalTime d tod
        zoned = T.ZonedTime local tz
    return $ B.DateTime zoned

-- Format for dates is:
-- 2011/01/22 or 2011-01-22
-- followed by a time spec:
-- 16:42 -0400
-- or HMS:
-- 16:42:45 -0400
-- You can omit the time zone spec, in which case
-- the parser assumes the time is local to the timezone that is
-- passed in to the function (generally this will be the local time
-- the machine is in, or UTC).

digits1or2 :: Parser String
digits1or2 = do
  d1 <- digit
  d2 <- optionMaybe digit
  let r = case d2 of
        Nothing -> d1:[]
        (Just d) -> d1:d:[]
  return r

monthOrDayNum :: Parser Int
monthOrDayNum = read <$> digits1or2

year :: Parser Integer
year = read <$> (replicateM 4 digit)

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
hoursMins = f <$> digits1or2 <*> char ':' <*> replicateM 2 digit where
  f h _ m = (read h, read m)

secs :: Parser Pico
secs = f <$> char ':' <*> replicateM 2 digit where
  f _ s = (fromIntegral . (read :: String -> Int) $ s)


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
timeZone = f <$> sign <*> replicateM 2 digit <*> replicateM 2 digit
  where
    f s h m = let mins = s (hr * 60 + mi)
                  (hr, mi) = (read h, read m)
              in minutesToTimeZone mins

