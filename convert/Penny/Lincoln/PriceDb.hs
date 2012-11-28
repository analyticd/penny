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
import qualified Data.Foldable as Fdbl
import qualified Data.Map as M
import qualified Data.Time as T
import qualified Penny.Lincoln.NestedMap as NM
import qualified Penny.Lincoln.Bits as B

type CpuMap = M.Map T.UTCTime B.CountPerUnit
type ToMap = M.Map B.To CpuMap

-- | The PriceDb holds information about prices. Create an empty one
-- using 'emptyDb' then fill it with values using foldl or similar.
newtype PriceDb = PriceDb (NM.NestedMap B.SubCommodity ToMap)

-- | An empty PriceDb
emptyDb :: PriceDb
emptyDb = PriceDb NM.empty

-- | Add a single price to the PriceDb.
addPrice :: PriceDb -> B.PricePoint -> PriceDb
addPrice (PriceDb db) pp@(B.PricePoint _ pr _) =
  PriceDb $ NM.relabel db ls where
    ls = firsts ++ [lst]
    cmdtyList = Fdbl.toList . B.unCommodity . B.unFrom . B.from $ pr
    firsts = map toFst (init cmdtyList) where
      toFst cty = (cty, f)
      f maybeL = case maybeL of
        Nothing -> M.empty
        Just m -> m
    lst = (last cmdtyList, insertIntoToMap pp)


-- | Returns a function to use when inserting a new value into the
-- ToMap label of a PriceDb.
insertIntoToMap ::
  B.PricePoint
  -> Maybe ToMap
  -> ToMap
insertIntoToMap (B.PricePoint dt pr _) =
  let toCmdty = B.to pr
      newToMap oldMap = M.alter alterTo toCmdty oldMap
      alterTo mayCpuMap =
        let newKey = dateTimeToUTC dt
            newVal = B.countPerUnit pr
        in Just $ case mayCpuMap of
          Nothing -> M.singleton newKey newVal
          Just m -> M.insert newKey newVal m
      relabeler maybeToMap =
        newToMap (maybe M.empty id maybeToMap)
  in relabeler

dateTimeToUTC :: B.DateTime -> T.UTCTime
dateTimeToUTC dt = T.localTimeToUTC tz lt where
  tz = T.minutesToTimeZone . B.offsetToMins $ tzo
  lt = B.localTime dt
  tzo = B.timeZone dt


-- | Getting prices can fail; if it fails, an Error is returned.
data PriceDbError = FromNotFound | ToNotFound | CpuNotFound

-- | Looks up values from the PriceDb. Throws "Error" if something
-- fails.
--
-- First, tries to find the best possible From match. For example, if
-- From is LUV:2001, first tries to see if there is a From match for
-- LUV:2001. If there is not an exact match for LUV:2001 but there
-- is a match for LUV, then LUV is used. If there is not a match
-- for either LUV:2001 or for LUV, then FromNotFound is thrown.
--
-- The To commodity must match exactly. So, if the TO commodity is
-- LUV:2001, only LUV:2001 will do. If the To commodity is not
-- found, ToNotFound is thrown.
--
-- The DateTime is the time at which to find a price. If a price
-- exists for that exact DateTime, that price is returned. If no price
-- exists for that exact DateTime, but there is a price for an earlier
-- DateTime, the latest possible price is returned. If there are no
-- earlier prices, CpuNotFound is thrown.
--
-- There is no backtracking on earlier decisions. For example, if From
-- is LUV:2001, and there is indeed at least one From price in the
-- PriceDb and CpuNotFound occurs, getPrice does not check to see if
-- the computation would have succeeded had it used LUV rather than
-- LUV:2001.

getPrice ::
  PriceDb
  -> B.From
  -> B.To
  -> B.DateTime
  -> Ex.Exceptional PriceDbError B.CountPerUnit
getPrice (PriceDb db) fr to dt = do
  let utc = dateTimeToUTC dt
      subs = Fdbl.toList . B.unCommodity . B.unFrom $ fr
  toMap <- case NM.descend subs db of
    [] -> Ex.throw FromNotFound
    xs -> return . snd . last $ xs
  cpuMap <- case M.lookup to toMap of
    Nothing -> Ex.throw ToNotFound
    Just m -> return m
  let (lower, exact, _) = M.splitLookup utc cpuMap
  cpu <- case exact of
    Just c -> return c
    Nothing ->
      if M.null lower
      then Ex.throw CpuNotFound
      else return . snd . M.findMax $ lower
  return cpu

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
convert db dt to (B.Amount qt fr)
  | fr == B.unTo to = return qt
  | otherwise = do
    cpu <- fmap B.unCountPerUnit (getPrice db (B.From fr) to dt)
    let qt' = B.mult cpu qt
    return qt'
