module Penny.Copper.DateTime
  ( dateTime
  , render
  ) where

import Control.Applicative ((<$>), optional, (<*>))
import qualified Data.Text as X
import Data.Time (fromGregorianValid)
import Data.Maybe (fromMaybe)
import qualified Data.Time as T
import Text.Parsec (char, digit, (<|>), (<?>))
import Text.Parsec.Text ( Parser )
import Control.Monad ( void )
import System.Locale (defaultTimeLocale)

import Penny.Copper.Util (spaces)
import qualified Penny.Lincoln.Bits as B

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

hrs :: Parser B.Hours
hrs = do
  h <- read2digits
  maybe (fail "invalid hour") return $ B.intToHours h


mins :: Parser B.Minutes
mins = do
  m <- read2digits
  maybe (fail "invalid minutes") return $ B.intToMinutes m

secs :: Parser B.Seconds
secs = do
  s <- fmap fromIntegral read2digits
  maybe (fail "invalid seconds") return $ B.picoToSeconds s


timeOfDay :: Parser (B.Hours, B.Minutes, B.Seconds)
timeOfDay = do
  h <- hrs
  colon
  m <- mins
  maybeS <- optional (colon >> secs)
  let s = fromMaybe B.zeroSeconds maybeS
  return (h, m, s)

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

dateTime :: Parser B.DateTime
dateTime = do
  d <- date
  spaces
  mayTod <- optional timeOfDay
  spaces
  mayTz <- optional timeZoneOffset
  let (h, m, s) = fromMaybe B.midnight mayTod
      tz = fromMaybe B.noOffset mayTz
  return $ B.DateTime d h m s tz

