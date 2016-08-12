{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Penny.Price.Internal where

import Control.Lens
import Penny.Commodity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Penny.Decimal

type FromCy = Commodity
type ToCy = Commodity

data FromTo = FromTo
  { fromCy :: Commodity
  , toCy :: Commodity
  } deriving (Eq, Ord, Show)

makeFromTo
  :: Commodity
  -- ^ From this commodity
  -> Commodity
  -- ^ To this commodity
  -> Maybe FromTo
makeFromTo fr to
  | fr /= to = Just $ FromTo fr to
  | otherwise = Nothing

convertQty
  :: Decimal
  -- ^ Price
  -> Decimal
  -- ^ Quantity
  -> Decimal
  -- ^ New quantity
convertQty exch orig = exch * orig

newtype PriceDb = PriceDb
  (Map FromCy (Map ToCy (Map UTCTime Decimal)))
  deriving Show

data Price a = Price
  { _zonedTime :: ZonedTime
  , _fromTo :: FromTo
  , _exch :: Decimal
  , _location :: a
  } deriving (Show, Functor, Foldable, Traversable)

makeLenses ''Price

emptyDb :: PriceDb
emptyDb = PriceDb M.empty

addPriceToDb :: PriceDb -> Price a -> PriceDb
addPriceToDb (PriceDb db) (Price dt (FromTo fr to) exch _)
  = PriceDb . M.alter fToMap fr $ db
  where
    utct = zonedTimeToUTC dt
    fToMap mayToMap = case mayToMap of
      Nothing -> Just (M.singleton to (M.singleton utct exch))
      Just toMap -> Just $ M.alter fUTCmap to toMap
        where
          fUTCmap mayUTCmap = case mayUTCmap of
            Nothing -> Just $ M.singleton utct exch
            Just utcMap -> Just $ M.insert utct exch utcMap

data ExchLookupError
  = FromCommodityNotFound
  | ToCommodityNotFound
  | NoPreviousPrice
  deriving (Eq, Ord, Show)

lookupExch
  :: FromTo
  -> ZonedTime
  -> PriceDb
  -> Either ExchLookupError (UTCTime, Decimal)
lookupExch (FromTo fr to) dt (PriceDb db) = do
  let utct = zonedTimeToUTC dt
  toMap <- maybe (Left FromCommodityNotFound) Right
    . M.lookup fr $ db
  timeMap <- maybe (Left ToCommodityNotFound) Right
    . M.lookup to $ toMap
  maybe (Left NoPreviousPrice) Right
    . M.lookupLT utct $ timeMap
