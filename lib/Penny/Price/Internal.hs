{-# LANGUAGE TemplateHaskell #-}
module Penny.Price.Internal where

import Control.Lens
import Penny.Commodity
import Data.Map (Map)
import qualified Data.Map as M
import Data.Time
import Penny.Decimal

newtype FromCy = FromCy Commodity
  deriving (Eq, Ord, Show)

newtype ToCy = ToCy Commodity
  deriving (Eq, Ord, Show)

data FromTo = FromTo
  { fromCy :: FromCy
  , toCy :: ToCy
  } deriving (Eq, Ord, Show)

makeFromTo :: FromCy -> ToCy -> Maybe FromTo
makeFromTo f@(FromCy fr) t@(ToCy to)
  | fr /= to = Just $ FromTo f t
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

data Price = Price
  { _zonedTime :: ZonedTime
  , _fromTo :: FromTo
  , _exch :: Decimal
  } deriving Show

makeLenses ''Price

emptyDb :: PriceDb
emptyDb = PriceDb M.empty

addPriceToDb :: PriceDb -> Price -> PriceDb
addPriceToDb (PriceDb db) (Price dt (FromTo fr to) exch)
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
