-- | A database of price information. A PricePoint has a DateTime, a
-- From commodity, a To commodity, and a QtyPerUnit. The PriceDb holds
-- this information for several prices. You can query the database by
-- supplying a from commodity, a to commodity, and a DateTime, and the
-- database will give you the QtyPerUnit, if there is one.
module Penny.Lincoln.PriceDb (
  PriceDb,
  emptyDb,
  addPrice,
  getPrice,
  PriceDbError(FromNotFound, ToNotFound, CpuNotFound),
  convert
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Penny.Lincoln.Bits as B

type CpuMap = M.Map T.UTCTime B.CountPerUnit
type ToMap = M.Map B.To CpuMap

-- | The PriceDb holds information about prices. Create an empty one
-- using 'emptyDb' then fill it with values using foldl or similar.
newtype PriceDb = PriceDb (M.Map B.From ToMap)

-- | An empty PriceDb
emptyDb :: PriceDb
emptyDb = PriceDb M.empty

-- | Add a single price to the PriceDb.
addPrice :: PriceDb -> B.PricePoint -> PriceDb
addPrice (PriceDb db) (B.PricePoint dt pr _ _ _) = PriceDb m'
  where
    m' = M.alter f (B.from pr) db
    utc = B.toUTC dt
    cpu = B.countPerUnit pr
    f k = case k of
      Nothing -> Just $ M.singleton (B.to pr) cpuMap
        where
          cpuMap = M.singleton utc cpu
      Just tm -> Just tm'
        where
          tm' = M.alter g (B.to pr) tm
          g maybeTo = case maybeTo of
            Nothing -> Just $ M.singleton utc cpu
            Just cpuMap -> Just $ M.insert utc cpu cpuMap



-- | Getting prices can fail; if it fails, an Error is returned.
data PriceDbError = FromNotFound | ToNotFound | CpuNotFound

-- | Looks up values from the PriceDb. Throws "Error" if something
-- fails.
--
-- The DateTime is the time at which to find a price. If a price
-- exists for that exact DateTime, that price is returned. If no price
-- exists for that exact DateTime, but there is a price for an earlier
-- DateTime, the latest possible price is returned. If there are no
-- earlier prices, CpuNotFound is thrown.

getPrice ::
  PriceDb
  -> B.From
  -> B.To
  -> B.DateTime
  -> Ex.Exceptional PriceDbError B.CountPerUnit
getPrice (PriceDb db) fr to dt = do
  let utc = B.toUTC dt
  toMap <- Ex.fromMaybe FromNotFound $ M.lookup fr db
  cpuMap <- Ex.fromMaybe ToNotFound $ M.lookup to toMap
  let (lower, exact, _) = M.splitLookup utc cpuMap
  case exact of
    Just c -> return c
    Nothing ->
      if M.null lower
      then Ex.throw CpuNotFound
      else return . snd . M.findMax $ lower


-- | Given an Amount and a Commodity to convert the amount to,
-- converts the Amount to the given commodity. If the Amount given is
-- already in the To commodity, simply returns what was passed in. Can
-- fail and throw PriceDbError. Internally uses 'getPrice', so read its
-- documentation for details on how price lookup works.
convert ::
  PriceDb
  -> B.DateTime
  -> B.To
  -> B.Amount
  -> Ex.Exceptional PriceDbError B.Qty
convert db dt to (B.Amount qt fr _ _)
  | fr == B.unTo to = return qt
  | otherwise = do
    cpu <- fmap B.unCountPerUnit (getPrice db (B.From fr) to dt)
    let qt' = B.mult cpu qt
    return qt'
