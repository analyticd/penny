module Penny.Copper.DateTime (
  DefaultTimeZone(DefaultTimeZone, unDefaultTimeZone)
  , dateTime
  , utcDefault
  , render
  ) where

import Control.Applicative ((<$>), optional, (<*>))
import qualified Data.Text as X
import Data.Time (fromGregorianValid)
import Data.Maybe (fromMaybe)
import qualified Data.Time as T
import Text.Parsec (char, digit, (<|>), (<?>))
import Text.Parsec.Text ( Parser )
import Control.Monad ( void, when )
import Data.Fixed ( Pico )
import System.Locale (defaultTimeLocale)

import Penny.Copper.Util (spaces)
import qualified Penny.Lincoln.Bits as B

newtype DefaultTimeZone =
  DefaultTimeZone { unDefaultTimeZone :: B.TimeZoneOffset }
  deriving (Eq, Show)

utcDefault :: DefaultTimeZone
utcDefault = DefaultTimeZone B.noOffset

charToDigit :: Char -> Int
charToDigit c = case c of
  '0' -> 0
  '1' -> 1
  '2' -> 2
  '3' -> 3
  '4' -> 4
  '5' -> 5
  '6' -> 6
  '7' -> 7
  '8' -> 8
  '9' -> 9
  _ -> error "unrecognized digit"

read2digits :: Parser Int
read2digits = f <$> digit <*> digit where
  f d1 d2 = charToDigit d1 * 10 + charToDigit d2

read4digits :: Parser Integer
read4digits = f <$> digit <*> digit <*> digit <*> digit where
  f d1 d2 d3 d4 = fromIntegral $
    charToDigit d1 * 1000
    + charToDigit d2 * 100
    + charToDigit d3 * 10
    + charToDigit d4

date :: Parser T.Day
date = do
  let slash = void $ char '/' <|> char '-'
  y <- read4digits
  slash
  m <- read2digits
  slash
  d <- read2digits
  case fromGregorianValid y m d of
    Nothing -> fail "invalid date"
    Just dt -> return dt

colon :: Parser ()
colon = void $ char ':'

hrs :: Parser Int
hrs = do
  h <- read2digits
  when (h > 23) $ fail "invalid hour"
  return h

mins :: Parser Int
mins = do
  m <- read2digits
  when (m > 59) $ fail "invalid minute"
  return m

secs :: Parser Pico
secs = do
  s <- fromIntegral <$> read2digits
  when (s > 59) $ fail "invalid seconds"
  return s

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
  h <- read2digits
  m <- read2digits
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
  return (B.dateTime (T.LocalTime d tod) tz)

-- | Render a DateTime. If the DateTime is in the given
-- DefaultTimeZone, and the DateTime is midnight, then the time and
-- time zone will not be printed. Otherwise, the time and time zone
-- will both be printed. The test for time zone equality depends only
-- upon the time zone's offset from UTC.
render :: DefaultTimeZone -> B.DateTime -> X.Text
render (DefaultTimeZone dtz) dt = let
  lt = B.localTime dt
  off = B.timeZone dt
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
