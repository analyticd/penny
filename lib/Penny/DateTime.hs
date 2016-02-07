{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Date and time support functions.
module Penny.DateTime where

import Control.Applicative
  ((<|>), (<$>))

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
    leap
      | m == 2 && d == 29 = do
          yr <- intToDigit y
          let dash = DateSep '-'
          return . Date'LeapDay $ LeapDay yr dash zero two dash two nine
      | otherwise = Nothing
    nonLeap
      | m == 2 && d == 29 = Nothing
      | otherwise = undefined

intsToMonthDay
  :: Int
  -- ^ Month
  -> Int
  -- ^ Day
  -> Maybe MonthDay
intsToMonthDay m d
  | m == 1 = undefined
