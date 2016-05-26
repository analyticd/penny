{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Copper.PriceParts where

import Control.Lens (makeLenses)
import Data.Time (ZonedTime)

import Penny.Commodity
import Penny.Decimal

data PriceParts a = PriceParts
  { _pricePos :: a
  , _priceTime :: ZonedTime
  , _priceFrom :: Commodity
  , _priceTo :: Commodity
  , _priceExch :: Decimal
  } deriving (Functor, Foldable, Traversable)

makeLenses ''PriceParts
