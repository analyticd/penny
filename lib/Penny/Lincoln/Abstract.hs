{-# LANGUAGE BangPatterns #-}
module Penny.Lincoln.Abstract where

import qualified Deka.Native.Abstract as A

-- | A group of digits, with at least one digit being non-zero.

data Lot = Lot
  { lotLeft :: [A.Decem]
  , lotNovem :: A.Novem
  , lotRight :: [A.Decem]
  } deriving (Eq, Ord, Show)

-- | A non-empty group of digits.

data Voll = Voll
  { vollDigit :: A.Decem
  , vollDigits :: [A.Decem]
  } deriving (Eq, Ord, Show)

-- | A group of digits with a grouping character; the digits appear
-- to the left of the grouping character.

data ChainL a = ChainL
  { clVoll :: Voll
  , clGrouper :: a
  } deriving (Eq, Ord, Show)

-- | A group of digits with a grouping character; the digits appear
-- to the right of the grouping character.

data ChainR a = ChainR
  { crGrouper :: a
  , crVoll :: Voll
  } deriving (Eq, Ord, Show)

-- | A group of digits with a non-zero digit, surrounded by other
-- grouped digits.
data Clatch a = Clatch
  { ctLeft :: [ChainL a]
  , ctCenter :: Lot
  , ctRight :: [ChainR a]
  } deriving (Eq, Ord, Show)

-- | A whole number, with no radix point.
newtype Whole a = Whole { unWhole :: Clatch a }
  deriving (Eq, Ord, Show)

-- | Several groups of digits; there must be at least one digit.

data Flock a = Flock
  { flFirst :: Voll
  , flRest :: [ChainR a]
  } deriving (Eq, Ord, Show)

-- | A non-zero number with a radix point; the portion to the left
-- of the radix point has a non-zero digit.

data PunctaL a = PunctaL
  { plLeft :: Clatch a
  , plRight :: Maybe (Flock a)
  } deriving (Eq, Ord, Show)

-- | A non-zero number with a radix point; the portion to the left
-- of the radix point has a non-zero digit.

data PunctaR a = PunctaR
  { prLeft :: Maybe (Flock a)
  , prRight :: Clatch a
  } deriving (Eq, Ord, Show)

data NonZero a
  = WholeOnly (Whole a)
  | NZLeft (PunctaL a)
  | NZRight (PunctaR a)
  deriving (Eq, Ord, Show)

-- # Zeroes

-- | Natural numbers, at least one.

data Nat = One | Succ !Nat
  deriving (Eq, Ord, Show)

natural :: Int -> Maybe Nat
natural i
  | i < 1 = Nothing
  | otherwise = Just $ go (i - 1) One
  where
    go !c n
      | c < 1 = n
      | otherwise = go (c - 1) (Succ n)

toInt :: Nat -> Int
toInt n = go 0 n
  where
    go !c nat = case nat of
      One -> c + 1
      Succ nat' -> go (c + 1) nat'

-- | A non-empty group of zeroes.

newtype Eggs = Eggs { unEggs :: Nat }
  deriving (Eq, Ord, Show)

-- | A non-empty group of zeroes with a grouping character.

data Basket a = Basket
  { bkGrouper :: a
  , bkEggs :: Eggs
  } deriving (Eq, Ord, Show)

-- | A non-empty group of zeroes, with optional additional groups.

data Coop a = Coop
  { cpEggs :: Eggs
  , cpBaskets :: [Basket a]
  } deriving (Eq, Ord, Show)

-- | A zero number with a whole part only.

newtype Beak a = Beak { unBeak :: Coop a } deriving (Eq, Ord, Show)

-- | A number with a radix point, with zeroes on the left and
-- optional zeroes on the right.

data WingL a = WingL
  { wlLeft :: Coop a
  , wlRight :: Maybe (Coop a)
  } deriving (Eq, Ord, Show)

-- | A number with a radix point, with optional zeroes on the left
-- and zeroes on the right.
data WingR a = WingR
  { wrLeft :: Maybe (Coop a)
  , wrRight :: Coop a
  } deriving (Eq, Ord, Show)

data Zero a
  = ZBeak (Beak a)
  | ZWingL (WingL a)
  | ZWingR (WingR a)
  deriving (Eq, Ord, Show)

data Side
  = Debit
  | Credit
  deriving (Eq, Ord, Show)

