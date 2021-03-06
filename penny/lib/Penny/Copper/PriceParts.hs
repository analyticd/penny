{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
-- | The components of a price.  Unlike a 'P.Price', no checking has
-- been performed to ensure that the '_priceFrom' and '_priceTo' are
-- not the same 'Commodity'.
module Penny.Copper.PriceParts where

import Control.Lens (makeLenses)
import Data.Time (ZonedTime)

import Penny.Commodity
import Penny.Decimal
import qualified Penny.Price as P

data PriceParts a = PriceParts
  { _pricePos :: a
  , _priceTime :: ZonedTime
  , _priceFrom :: Commodity
  , _priceTo :: Commodity
  , _priceExch :: Decimal
  } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''PriceParts

-- | Fails if the from and to commodities are the same.
pricePartsToPrice :: PriceParts a -> Maybe (P.Price a)
pricePartsToPrice (PriceParts pos ti fr to exch)
  = fmap f $ P.makeFromTo fr to
  where
    f frTo = P.Price ti frTo exch pos

priceToPriceParts :: P.Price a -> PriceParts a
priceToPriceParts (P.Price zt frTo exch loc) = PriceParts loc zt fr to exch
  where
    fr = P.fromCy frTo
    to = P.toCy frTo
