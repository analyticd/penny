{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Date and time support functions.
module Penny.Copper.DateTime where

import Control.Applicative
  ((<|>))
import qualified Data.Time as Time

import Penny.Copper.Conversions
import Penny.Copper.Singleton
import Penny.Copper.Types

intsToDate
  :: Int
  -- ^ Year
  -> Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe (Date Char ())
intsToDate y m d = nonLeap <|> leap
  where
    dash = DateSep'Hyphen sHyphen
    leap
      | m == 2 && d == 29 = do
          yr <- c'LeapYear'Int y
          return . Date'LeapDay $ LeapDay yr dash sZero sTwo dash
            sTwo sNine
      | otherwise = Nothing
    nonLeap
      | m == 2 && d == 29 = Nothing
      | otherwise = do
          yr <- c'Year'Int y
          md <- intsToMonthDay m d
          return . Date'NonLeapDay $ NonLeapDay yr dash md

c'Date'Day :: Time.Day -> Maybe (Date Char ())
c'Date'Day day = do
  let (yr, m, d) = Time.toGregorian day
  y <- getYear yr
  intsToDate y m d
  where
    getYear y | y > fromIntegral (maxBound :: Int) = Nothing
              | y < fromIntegral (minBound :: Int) = Nothing
              | otherwise = Just $ fromIntegral y

c'Copper'ZonedTime
  :: Time.ZonedTime
  -> Maybe (Date Char (), Time Char (), Zone Char ())
c'Copper'ZonedTime = undefined

c'Time'TimeOfDay
  :: Time.TimeOfDay
  -> Maybe (Time Char ())
c'Time'TimeOfDay = undefined

intsToMonthDay
  :: Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe (MonthDay Char ())
intsToMonthDay m d
  | m == 1 = do
      d31 <- c'Days31'Int d
      return $ Jan sZero sOne dash d31
  | m == 2 = do
      d28 <- c'Days28'Int d
      return $ Feb sZero sTwo dash d28
  | m == 3 = do
      d31 <- c'Days31'Int d
      return $ Mar sZero sThree dash d31
  | m == 4 = do
      d30 <- c'Days30'Int d
      return $ Apr sZero sFour dash d30
  | m == 5 = do
      d31 <- c'Days31'Int d
      return $ May sZero sFive dash d31
  | m == 6 = do
      d30 <- c'Days30'Int d
      return $ Jun sZero sSix dash d30
  | m == 7 = do
      d31 <- c'Days31'Int d
      return $ Jul sZero sSeven dash d31
  | m == 8 = do
      d31 <- c'Days31'Int d
      return $ Aug sZero sEight dash d31
  | m == 9 = do
      d30 <- c'Days30'Int d
      return $ Sep sZero sNine dash d30
  | m == 10 = do
      d31 <- c'Days31'Int d
      return $ Oct sOne sZero dash d31
  | m == 11 = do
      d30 <- c'Days30'Int d
      return $ Nov sOne sOne dash d30
  | m == 12 = do
      d31 <- c'Days31'Int d
      return $ Dec sOne sTwo dash d31
  | otherwise = Nothing
  where
    dash = DateSep'Hyphen sHyphen

monthDayToInts
  :: MonthDay t a
  -> (Int, Int)
  -- ^ Int for month (1 is January, 12 is December) and for day
monthDayToInts x = case x of
  Jan _ _ _ d31 -> (1, c'Int'Days31 d31)
  Feb _ _ _ d28 -> (2, c'Int'Days28 d28)
  Mar _ _ _ d31 -> (3, c'Int'Days31 d31)
  Apr _ _ _ d30 -> (4, c'Int'Days30 d30)
  May _ _ _ d31 -> (5, c'Int'Days31 d31)
  Jun _ _ _ d30 -> (6, c'Int'Days30 d30)
  Jul _ _ _ d31 -> (7, c'Int'Days31 d31)
  Aug _ _ _ d31 -> (8, c'Int'Days31 d31)
  Sep _ _ _ d30 -> (9, c'Int'Days30 d30)
  Oct _ _ _ d31 -> (10, c'Int'Days31 d31)
  Nov _ _ _ d30 -> (11, c'Int'Days30 d30)
  Dec _ _ _ d31 -> (12, c'Int'Days31 d31)

c'Day'Date :: Date t a -> Time.Day
c'Day'Date x = case x of
  Date'NonLeapDay (NonLeapDay yr _ md) ->
    Time.fromGregorian (fromIntegral y) m d
    where
      y = c'Int'Year yr
      (m, d) = monthDayToInts md
  Date'LeapDay (LeapDay yr _ _ _ _ _ _) ->
    Time.fromGregorian (fromIntegral y) m d
    where
      y = c'Int'LeapYear yr
      m = 2
      d = 29

c'TimeOfDay'Time :: Time t a -> Time.TimeOfDay
c'TimeOfDay'Time = undefined
