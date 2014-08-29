module Penny.Copper.DateTime where

import Penny.Copper.Render
import Control.Monad
import qualified Data.Text as X
import Text.Parsec

-- | A four-digit year, from 1000 to 9999.

newtype Year = Year { unYear :: Int }
  deriving (Eq, Ord, Show)

year :: Int -> Maybe Year
year i
  | i < 1000 = Nothing
  | i > 9999 = Nothing
  | otherwise = Just $ Year i

instance Renderable Year where
  render = X.pack . show . unYear
  parser = fmap (Year . f) $ replicateM 4 digit
    where
      f (d1:d2:d3:d4:[]) = charToInt d1 * 1000
                         + charToInt d2 * 100
                         + charToInt d3 * 10
                         + charToInt d4
      f _ = error "year parser: error"

charToInt :: Char -> Int
charToInt c = case c of
  { '0' -> 0; '1' -> 1; '2' -> 2; '3' -> 3; '4' -> 4; '5' -> 5;
    '6' -> 6; '7' -> 7; '8' -> 8; '9' -> 9;
    _ -> error "charToInt: error" }

-- | A two-digit month, from 1 to 12 (with leading zeroes for 1 to 9).

newtype Month = Month { unMonth :: Int }
  deriving (Eq, Ord, Show)

month :: Int -> Maybe Month
month i
  | i < 1 = Nothing
  | i > 12 = Nothing
  | otherwise = Just $ Month i

pad0 :: Show a => a -> X.Text
pad0 a = X.pack str
  where
    str | length s < 2 = '0' : s
        | otherwise = s
    s = show a

-- | A two-digit day, from 1 to 31.

newtype Day = Day { unDay :: Int }
  deriving (Eq, Ord, Show)

day :: Int -> Maybe Day
day i
  | i < 1 = Nothing
  | i > 31 = Nothing
  | otherwise = Just $ Day i

