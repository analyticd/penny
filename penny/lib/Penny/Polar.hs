{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Polar where

import Control.Lens

data Pole = North | South
  deriving (Eq, Ord, Show)

opposite :: Pole -> Pole
opposite North = South
opposite South = North

-- | An object that is polar.
data Polarized a = Polarized
  { _charged :: a
  , _charge :: Pole
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

makeLenses ''Polarized

-- | An object that might be polar.
data Moderated n o
  = Moderate n
  | Extreme (Polarized o)
  deriving Show

makePrisms ''Moderated

pole'Moderated :: Moderated n o -> Maybe Pole
pole'Moderated (Moderate _) = Nothing
pole'Moderated (Extreme (Polarized _ p)) = Just p

debit :: Pole
debit = North

credit :: Pole
credit = South

positive :: Pole
positive = North

negative :: Pole
negative = South

integerPole :: Integer -> Maybe Pole
integerPole i
  | i < 0 = Just negative
  | i == 0 = Nothing
  | otherwise = Just positive

