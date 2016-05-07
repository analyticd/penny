module Penny.Popularity where

import Control.Lens
import qualified Control.Lens as Lens
import Data.Semigroup
import qualified Data.Foldable as F
import qualified Data.Map as M
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Arrangement
import Penny.Commodity
import Penny.Copper.Types
  (GrpRadCom(GrpRadCom'Grouper), Grouper(Grouper'ThinSpace),
  GrpRadPer(GrpRadPer'Comma))
import Penny.Copper.Copperize
import Penny.Mimode
import Penny.Clatch
import Control.Applicative
import Penny.SeqUtil
import Control.Monad (join)
import Data.Maybe
import qualified Penny.Troika as T

-- | Map describing how different 'Commodity' are rendered.
newtype History = History
  (M.Map Commodity
         (Seq (Arrangement, Either (Seq (GrpRadCom Char ()))
                                   (Seq (GrpRadPer Char ())))))
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
  -> (Seq (Seq (GrpRadCom Char ())), Seq (Seq (GrpRadPer Char ())))
groupers (History hist) mayCy = fromMaybe allGroupers $ do
  cy <- mayCy
  cyHist <- M.lookup cy hist
  return . partitionEithers . fmap snd $ cyHist
  where
    allGroupers = partitionEithers . fmap snd $ allHist
    allHist = mconcat . M.elems $ hist


vote :: Posting -> History
vote = Lens.view (core . troika . to make)
  where
    make tri = case T.troikaRendering tri of
      Nothing -> mempty
      Just (cy, ar, ei) -> History
        $ M.singleton cy (Seq.singleton (ar, ei))

elect :: Foldable f => f Transaction -> History
elect = F.foldl' f mempty
  where
    f hist = F.foldl' g hist . view postings
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
  :: (Seq (Seq (GrpRadCom Char ())), Seq (Seq (GrpRadPer Char ())))
  -> Either (GrpRadCom Char ()) (GrpRadPer Char ())
selectGrouper (rcs, rps)
  | Seq.length rps >= Seq.length rcs = Right rp
  | otherwise = Left rc
  where
    rp = fromMaybe comma . mimode . join $ rps
    rc = fromMaybe thinSpace . mimode . join $ rcs
    thinSpace = GrpRadCom'Grouper (Grouper'ThinSpace cThinSpace)
    comma = GrpRadPer'Comma cComma

-- | Gets all arrangements from a history.
arrangements :: History -> Seq Arrangement
arrangements (History hist)
  = fmap fst
  . join
  . fmap snd
  . Seq.fromList
  . M.assocs
  $ hist

-- | Determines whether to use a space between the commodity and the
-- magnitude. Defaults to using a space.
isSpaceBetween :: History -> Maybe Commodity -> SpaceBetween
isSpaceBetween (History hist) mayCom = fromMaybe True $ thisCy <|> allCy
  where
    thisCy = do
      cy <- mayCom
      sq <- M.lookup cy hist
      mimode . fmap (\(Arrangement _ s) -> s) . fmap fst $ sq
    allCy = mimode
      . fmap (\(Arrangement _ s) -> s)
      . arrangements
      . History
      $ hist

-- | Determines whether to put a commodity to the left or to the right
-- of the magnitude.  Defaults to putting the commodity on the right.
orientation :: History -> Maybe Commodity -> Orient
orientation (History hist) mayCom = fromMaybe CommodityOnRight
  $ thisCy <|> allCy
  where
    thisCy = do
      cy <- mayCom
      sq <- M.lookup cy hist
      mimode . fmap (\(Arrangement a _) -> a) . fmap fst $ sq
    allCy = mimode
      . fmap (\(Arrangement a _) -> a)
      . arrangements
      . History
      $ hist
