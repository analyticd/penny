module Penny.Popularity where

import Control.Lens
import Data.Semigroup
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Amount
import Penny.Commodity
import Penny.Mimode
import Penny.NonEmpty
import Penny.Qty
import Penny.Representation
import Penny.Trio
import Penny.Clatch
import Control.Applicative
import Penny.SeqUtil
import Control.Monad (join)
import qualified Data.Either
import Data.Maybe

-- | Map describing how different 'Commodity' are rendered.
newtype History = History
  (M.Map Commodity
         (Seq (Arrangement, Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

instance Monoid History where
  mempty = History M.empty
  mappend (History x) (History y) = History $ M.unionWith (<>) x y

-- | Determines how to arrange a particular commodity.  If the
-- commodity is present in the 'History', uses the 'mimode'
-- arrangement for that particular commodity.  If the commodity is not
-- present in the history, uses the 'mimode' arrangement for all
-- commodities in the 'History'.  If the 'History' has no commodities,
-- uses an arrangement with a space and with the commodity on the
-- left.
arrange :: History -> Commodity -> Arrangement
arrange (History hist) cy = maybe (Arrangement CommodityOnLeft True)
  id (thisCommodity <|> allCommodities)
  where
    thisCommodity = M.lookup cy hist >>= getArrangement
      where
        getArrangement = mimode . fmap fst
    allCommodities = mimode . fmap fst . mconcat . map F.toList . M.elems
      $ hist

-- | Gets all groupers for a given commodity, if a commodity is
-- supplied.  Otherwise, returns all groupers.
--
-- For each radix type, the outer sequence contains one sequence for
-- each representation, while the inner sequence contains all the
-- groupers for each particular representation.  Therefore, to count
-- how many representations use a particular radix, simply use the
-- length of the outer sequence.
groupers
  :: History
  -> Maybe Commodity
  -> (Seq (Seq RadCom), Seq (Seq RadPer))
groupers (History hist) mayCy = fromMaybe allGroupers $ do
  cy <- mayCy
  cyHist <- M.lookup cy hist
  return . partitionEithers . fmap snd $ cyHist
  where
    allGroupers = partitionEithers . fmap snd $ allHist
    allHist = mconcat . M.elems $ hist


vote :: Posting -> History
vote = make . (^. to core . trio)
  where
    make tri = case trioRendering tri of
      Nothing -> mempty
      Just (cy, ar, ei) -> History
        $ M.singleton cy (Seq.singleton (ar, ei))

elect :: Foldable f => f Transaction -> History
elect = F.foldl' f mempty
  where
    f hist = F.foldl' g hist . postings
      where
        g acc pstg = acc `mappend` vote pstg

-- | Given the result of 'groupers', selects the appropriate radix and
-- grouper.  The most frequently appearing radix is used; if both
-- radixes appear equally often, a period radix is used.  Then, the
-- most frequently appearing grouper is used.  If there is no most
-- frequently appearing grouper, then if the radix is a period, then a
-- comma grouper is used; if the radix is a comma, then a thin space
-- grouper is used.

selectGrouper
  :: (Seq (Seq RadCom), Seq (Seq RadPer))
  -> Either RadCom RadPer
selectGrouper (rcs, rps)
  | Seq.length rps >= Seq.length rcs = Right rp
  | otherwise = Left rc
  where
    rp = fromMaybe Comma . mimode . join $ rps
    rc = fromMaybe (RCGrouper ThinSpace) . mimode . join $ rcs

{-


smartRender
  :: Maybe (Either (Maybe RadCom) (Maybe RadPer))
  -> Abridged
  -> Amount
  -> NilOrBrimScalarAnyRadix
smartRender mayRndrer abridged amt
  = c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  $ repQtySmartly rndrer abridged amt
  where
    rndrer = maybe (Right Nothing) id mayRndrer
-}
