{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
{-# LANGUAGE DeriveGeneric #-}
module Penny.Polar where

import qualified Control.Lens as Lens
import GHC.Generics (Generic)
import Text.Show.Pretty (PrettyVal)

data Pole = North | South
  deriving (Eq, Ord, Show, Generic)

instance PrettyVal Pole

opposite :: Pole -> Pole
opposite North = South
opposite South = North

-- | An object that is polar.
data Polarized a = Polarized
  { _charged :: a
  , _charge :: Pole
  } deriving (Eq, Ord, Show, Functor, Foldable, Traversable, Generic)

instance PrettyVal a => PrettyVal (Polarized a)

Lens.makeLenses ''Polarized

oppositePolarized :: Polarized a -> Polarized a
oppositePolarized = Lens.over charge opposite

-- | An object that might be polar.
data Moderated n o
  = Moderate n
  | Extreme (Polarized o)
  deriving (Show, Generic)

instance (PrettyVal n, PrettyVal o) => PrettyVal (Moderated n o)

Lens.makePrisms ''Moderated

oppositeModerated :: Moderated n o -> Moderated n o
oppositeModerated = Lens.over _Extreme oppositePolarized

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

