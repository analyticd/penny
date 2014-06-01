{-# LANGUAGE BangPatterns #-}
module Penny.Lincoln.Decimal.Rep where

import qualified Deka.Native.Abstract as A
import Penny.Lincoln.Decimal.Side
import Penny.Lincoln.Nats

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

instance Functor ChainL where
  fmap f (ChainL v a) = ChainL v (f a)

newtype ChainsL a = ChainsL { unChainsL :: [ChainL a] }
  deriving (Eq, Ord, Show)

instance Functor ChainsL where
  fmap f = ChainsL . map (fmap f) . unChainsL

-- | A group of digits with a grouping character; the digits appear
-- to the right of the grouping character.

data ChainR a = ChainR
  { crGrouper :: a
  , crVoll :: Voll
  } deriving (Eq, Ord, Show)

instance Functor ChainR where
  fmap f (ChainR a v) = ChainR (f a) v

newtype ChainsR a = ChainsR { unChainsR :: [ChainR a] }
  deriving (Eq, Ord, Show)

instance Functor ChainsR where
  fmap f = ChainsR . map (fmap f) . unChainsR

-- | A group of digits with a non-zero digit, surrounded by other
-- grouped digits.
data Clatch a = Clatch
  { ctLeft :: ChainsL a
  , ctCenter :: Lot
  , ctRight :: ChainsR a
  } deriving (Eq, Ord, Show)

instance Functor Clatch where
  fmap f (Clatch l c r) = Clatch (fmap f l) c (fmap f r)

-- | A whole number, with no radix point.
newtype Whole a = Whole { unWhole :: Clatch a }
  deriving (Eq, Ord, Show)

instance Functor Whole where
  fmap f = Whole . fmap f . unWhole

-- | Several groups of digits; there must be at least one digit.

data Flock a = Flock
  { flFirst :: Voll
  , flRest :: ChainsR a
  } deriving (Eq, Ord, Show)

instance Functor Flock where
  fmap f (Flock v c) = Flock v (fmap f c)

-- | A non-zero number with a radix point; the portion to the left
-- of the radix point has a non-zero digit.

data PunctaL a = PunctaL
  { plLeft :: Clatch a
  , plRight :: Maybe (Flock a)
  } deriving (Eq, Ord, Show)

instance Functor PunctaL where
  fmap f (PunctaL l r) = PunctaL (fmap f l) (fmap (fmap f) r)

-- | A non-zero number with a radix point; the portion to the left
-- of the radix point has a non-zero digit.

data PunctaR a = PunctaR
  { prLeft :: Maybe (Flock a)
  , prRight :: Clatch a
  } deriving (Eq, Ord, Show)

instance Functor PunctaR where
  fmap f (PunctaR l r) = PunctaR (fmap (fmap f) l) (fmap f r)

data NonZero a
  = WholeOnly (Whole a)
  | NZLeft (PunctaL a)
  | NZRight (PunctaR a)
  deriving (Eq, Ord, Show)

instance Functor NonZero where
  fmap f r = case r of
    WholeOnly a -> WholeOnly (fmap f a)
    NZLeft a -> NZLeft (fmap f a)
    NZRight a -> NZRight (fmap f a)

-- # Zeroes

-- | A non-empty group of zeroes.

newtype Eggs = Eggs { unEggs :: Positive }
  deriving (Eq, Ord, Show)

-- | A non-empty group of zeroes with a grouping character.

data Basket a = Basket
  { bkGrouper :: a
  , bkEggs :: Eggs
  } deriving (Eq, Ord, Show)

instance Functor Basket where
  fmap f (Basket a e) = Basket (f a) e

newtype Baskets a = Baskets { unBaskets :: [Basket a] }
  deriving (Eq, Ord, Show)

instance Functor Baskets where
  fmap f = Baskets . map (fmap f) . unBaskets

-- | A non-empty group of zeroes, with optional additional groups.

data Coop a = Coop
  { cpEggs :: Eggs
  , cpBaskets :: Baskets a
  } deriving (Eq, Ord, Show)

instance Functor Coop where
  fmap f (Coop e b) = Coop e (fmap f b)

-- | A zero number with a whole part only.

newtype Beak a = Beak { unBeak :: Coop a } deriving (Eq, Ord, Show)

instance Functor Beak where
  fmap f = Beak . fmap f . unBeak

-- | A number with a radix point, with zeroes on the left and
-- optional zeroes on the right.

data WingL a = WingL
  { wlLeft :: Coop a
  , wlRight :: Maybe (Coop a)
  } deriving (Eq, Ord, Show)

instance Functor WingL where
  fmap f (WingL l r) = WingL (fmap f l) (fmap (fmap f) r)

-- | A number with a radix point, with optional zeroes on the left
-- and zeroes on the right.
data WingR a = WingR
  { wrLeft :: Maybe (Coop a)
  , wrRight :: Coop a
  } deriving (Eq, Ord, Show)

instance Functor WingR where
  fmap f (WingR l r) = WingR (fmap (fmap f) l) (fmap f r)

data Zero a
  = ZBeak (Beak a)
  | ZWingL (WingL a)
  | ZWingR (WingR a)
  deriving (Eq, Ord, Show)

instance Functor Zero where
  fmap f r = case r of
    ZBeak b -> ZBeak (fmap f b)
    ZWingL b -> ZWingL (fmap f b)
    ZWingR b -> ZWingR (fmap f b)

data Quant a = Quant
  { qNonZero :: NonZero a
  , qSide :: Side
  } deriving (Eq, Ord, Show)

instance Functor Quant where
  fmap f (Quant z s) = Quant (fmap f z) s

data Rep a
  = RQuant (Quant a)
  | RZero (Zero a)
  deriving (Eq, Ord, Show)

instance Functor Rep where
  fmap f r = case r of
    RQuant q -> RQuant (fmap f q)
    RZero z -> RZero (fmap f z)


