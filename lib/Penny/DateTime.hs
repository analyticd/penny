{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Date and time support functions.
module Penny.DateTime where

import Control.Applicative
  ((<|>))
import qualified Data.Time as Time

import Penny.Digit
import Penny.Grammar

intsToDate
  :: Int
  -- ^ Year
  -> Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe Date
intsToDate y m d = nonLeap <|> leap
  where
    dash = DateSep'Hyphen $ Hyphen '-'
    leap
      | m == 2 && d == 29 = do
          yr <- intToDigit y
          return . Date'LeapDay $ LeapDay yr dash zero two dash two nine
      | otherwise = Nothing
    nonLeap
      | m == 2 && d == 29 = Nothing
      | otherwise = do
          yr <- intToDigit y
          md <- intsToMonthDay m d
          return . Date'NonLeapDay $ NonLeapDay yr dash md


intsToMonthDay
  :: Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe MonthDay
intsToMonthDay m d
  | m == 1 = do
      d31 <- intToDigit d
      return $ Jan zero one dash d31
  | m == 2 = do
      d28 <- intToDigit d
      return $ Feb zero two dash d28
  | m == 3 = do
      d31 <- intToDigit d
      return $ Mar zero three dash d31
  | m == 4 = do
      d30 <- intToDigit d
      return $ Apr zero four dash d30
  | m == 5 = do
      d31 <- intToDigit d
      return $ May zero five dash d31
  | m == 6 = do
      d30 <- intToDigit d
      return $ Jun zero six dash d30
  | m == 7 = do
      d31 <- intToDigit d
      return $ Jul zero seven dash d31
  | m == 8 = do
      d31 <- intToDigit d
      return $ Aug zero eight dash d31
  | m == 9 = do
      d30 <- intToDigit d
      return $ Sep zero nine dash d30
  | m == 10 = do
      d31 <- intToDigit d
      return $ Oct one zero dash d31
  | m == 11 = do
      d30 <- intToDigit d
      return $ Nov one one dash d30
  | m == 12 = do
      d31 <- intToDigit d
      return $ Dec one two dash d31
  | otherwise = Nothing
  where
    dash = DateSep'Hyphen $ Hyphen '-'

monthDayToInts
  :: MonthDay
  -> (Int, Int)
  -- ^ Int for month (1 is January, 12 is December) and for day
monthDayToInts x = case x of
  Jan _ _ _ d31 -> (1, digitToInt d31)
  Feb _ _ _ d28 -> (2, digitToInt d28)
  Mar _ _ _ d31 -> (3, digitToInt d31)
  Apr _ _ _ d30 -> (4, digitToInt d30)
  May _ _ _ d31 -> (5, digitToInt d31)
  Jun _ _ _ d30 -> (6, digitToInt d30)
  Jul _ _ _ d31 -> (7, digitToInt d31)
  Aug _ _ _ d31 -> (8, digitToInt d31)
  Sep _ _ _ d30 -> (9, digitToInt d30)
  Oct _ _ _ d31 -> (10, digitToInt d31)
  Nov _ _ _ d30 -> (11, digitToInt d30)
  Dec _ _ _ d31 -> (12, digitToInt d31)

c'Day'Date :: Date -> Time.Day
c'Day'Date x = case x of
  Date'NonLeapDay (NonLeapDay yr _ md) -> Time.fromGregorian y m d
    where
      y = digitToInt yr
      (m, d) = monthDayToInts md
  Date'LeapDay (LeapDay yr _ _ _ _ _ _) -> Time.fromGregorian y m d
    where
      y = digitToInt yr
      m = 2
      d = 29

