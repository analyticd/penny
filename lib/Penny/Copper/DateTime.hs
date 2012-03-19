module Penny.Copper.DateTime (
  DefaultTimeZone(DefaultTimeZone, unDefaultTimeZone)
  , dateTime
  , render
  ) where

import Control.Applicative ((<$>), optional)
import qualified Data.Text as X
import Data.Time (fromGregorianValid)
import Data.Maybe (fromMaybe)
import qualified Data.Time as T
import Text.Parsec (char, digit, (<|>), (<?>))
import Text.Parsec.Text ( Parser )
import Control.Monad ( replicateM, void, when )
import Data.Fixed ( Pico )
import System.Locale (defaultTimeLocale)

import Penny.Copper.Util (spaces)
import qualified Penny.Lincoln.Bits as B

newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: B.TimeZoneOffset }
  deriving (Eq, Show)

date :: Parser T.Day
date = do
  let slash = void $ char '/' <|> char '-'
  y <- read <$> replicateM 4 digit
  slash
  m <- read <$> replicateM 2 digit
  slash
  d <- read <$> replicateM 2 digit
  case fromGregorianValid y m d of
    Nothing -> fail "invalid date"
    Just dt -> return dt

colon :: Parser ()
colon = void $ char ':'

hrs :: Parser Int
hrs = do
  h <- read <$> replicateM 2 digit
  when (h > 23) $ fail "invalid hour"
  return h

mins :: Parser Int
mins = do
  m <- read <$> replicateM 2 digit
  when (m > 59) $ fail "invalid minute"
  return m

secs :: Parser Pico
secs = do
  s <- (read :: String -> Int) <$> replicateM 2 digit
  when (s > 59) $ fail "invalid seconds"
  return $ fromIntegral s

timeOfDay :: Parser T.TimeOfDay
timeOfDay = do
  h <- hrs
  colon
  m <- mins
  maybeS <- optional (colon >> secs)
  let s = fromMaybe (fromIntegral (0 :: Int)) maybeS
  return $ T.TimeOfDay h m s

timeZoneOffset :: Parser B.TimeZoneOffset
timeZoneOffset = do
  changeSign <-
    (char '+' >> return id)
    <|> (char '-' >> return (negate :: Int -> Int))
    <?> "time zone sign"
  h <- read <$> replicateM 2 digit
  m <- read <$> replicateM 2 digit
  let mi = h * 60 + m
  maybe (fail "invalid time zone offset") return
    $ B.minsToOffset (changeSign mi)

dateTime :: DefaultTimeZone -> Parser B.DateTime
dateTime (DefaultTimeZone dtz) = do
  d <- date
  spaces
  mayTod <- optional timeOfDay
  spaces
  mayTz <- optional timeZoneOffset
  let tod = fromMaybe T.midnight mayTod
      tz = fromMaybe dtz mayTz
  return (B.DateTime (T.LocalTime d tod) tz)

-- | Render a DateTime. If the DateTime is in the given
-- DefaultTimeZone, and the DateTime is midnight, then the time and
-- time zone will not be printed. Otherwise, the time and time zone
-- will both be printed. The test for time zone equality depends only
-- upon the time zone's offset from UTC.
render :: DefaultTimeZone -> B.DateTime -> X.Text
render (DefaultTimeZone dtz) (B.DateTime lt off) = let
  fmtLong = "%F %T %z"
  fmtShort = "%F"
  sameZone = dtz == off
  local = T.localTimeOfDay lt
  isMidnight = local == T.midnight
  fmt = if sameZone && isMidnight
        then fmtShort
        else fmtLong
  zt = T.ZonedTime lt (T.minutesToTimeZone (B.offsetToMins off))
  in X.pack $ T.formatTime defaultTimeLocale fmt zt
