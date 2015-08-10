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

-- | Map describing how different 'Commodity' are rendered.
newtype History = History
  (M.Map Commodity
         (NonEmpty (Arrangement, Either (Seq RadCom) (Seq RadPer))))
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

-- | Gets all comma radix groupers.
radComGroupers
  :: NonEmpty (a, Either (Seq RadCom) b)
  -> Seq RadCom
radComGroupers = F.foldl' add Seq.empty . fmap snd
  where
    add sq ei = case ei of
      Left sq' -> sq <> sq'
      Right _ -> sq

-- | Gets all period radix groupers.
radPerGroupers
  :: NonEmpty (a, Either b (Seq RadPer))
  -> Seq RadPer
radPerGroupers = F.foldl' add Seq.empty . fmap snd
  where
    add sq ei = case ei of
      Left _ -> sq
      Right sq' -> sq <> sq'

{-
whichRadCom
  :: History
  -> Maybe Commodity
  -> RadCom
whichRadCom (History hist) mayCom = maybe Period id $ thisCy <|> allCy
  where
    thisCy = case mayCom of
      Nothing -> Nothing
      Just cy -> do
        histogram <- M.lookup cy hist
        mimode . join . lefts . fmap snd . seqFromNonEmpty $ histogram
    allCy = mimode . fst . allGroupers . History $ hist
-}

groupers
  :: History
  -> Maybe Commodity
  -> (Seq RadCom, Seq RadPer)
groupers (History hist) mayCom = case mayCom of
  Nothing -> partitionEithers . fmap snd . join . 

-- | Determines what radix The 'mimode' radix point for the given
-- commodity is chosen.  If there is no 'mimode' for the commodity,
-- then the 'mimode' radix point for all commodities is chosen.  If
-- there is no 'mimode' for all commodities, then a period radix is used.
{-
whichRadix
  :: History
  -> Commodity
  -> WhichRadix
whichRadix (History hist) cy = maybe Period id $ thisCy <|> allCy
  where
-}


-- | Determines what radix point and grouping character to use for a
-- particular commodity.  The 'mimode' radix point for the given
-- commodity is chosen.  If there is no 'mimode' for the commodity,
-- then the 'mimode' radix point for all commodities is chosen.  If
-- there is no 'mimode' for all commodities, then a period radix is used.
--
-- Then, for the given radix point, the 'mimode' grouping
-- character for the given commodity is chosen.  If there is no
-- 'mimode' grouping character for this commodity, the 'mimode' from
-- all commodities is chosen.  If this also fails, then a comma grouper
-- is used for a period radix, or a thin space grouper is used for a
-- period radix.

radGroupForCommodity
  :: History
  -> Commodity
  -> Either (Maybe RadPer) (Maybe RadCom)
radGroupForCommodity = undefined


{-
-- | Like 'History' but does not include arrangements.
newtype Abridged = Abridged
  (M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer))))
  deriving (Eq, Ord, Show)

abridge :: History -> Abridged
abridge (History mp) = Abridged $ fmap (fmap snd) mp

-- | Represents a 'Qty' as \"smartly\" as possible, based on how its
-- corresponding 'Commodity' has been represented in the past.
--
-- In @repQtySmartly ei mp cy qt@, @mp@ is a map of 'Commodity' to
-- consider for history analysis.  Each 'Commodity' is paired with a
-- history list.  Each element of the history list is an 'Either',
-- with a 'Left' indicating that the radix was a comma, and a
-- 'Right' indicating the radix was a period.  The sequence contains
-- each grouping character used.
--
-- If the commodity @cy@ is found in the map @mp@, then the radix
-- point used is always the 'mimode' radix point appearing in the
-- history list.  For that radix point, if there is a 'mimode' grouping
-- character, then grouping is attempted using that grouping
-- character.  If there is no 'mimode' grouping character, no grouping
-- is attempted.
--
-- If the commodity @cy@ is not found in the map @mp@, then the radix
-- point used is the 'mimode' radix point for /all/ commodities in the
-- map @mp@.  For that radix point, if there is a 'mimode' grouping
-- character, then grouping is attempted using that grouping
-- character.  If there is no 'mimode' grouping character, no grouping
-- is attempted.
--
-- If the map @mp@ is completely empty, then the 'Qty' is rendered
-- using @ei@, where @ei@ is @Left Nothing@ for comma radix, no
-- grouping; @Right Nothing@ for period radix, no grouping; @Left
-- (Just c)@ for comma radix with attempted grouping using @c@; or
-- @Right (Just c)@ for period radix with grouping attempted using
-- @c@.
repQtySmartly
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Default rendering
  -> Abridged
  -- ^ History map
  -> Amount
  -> QtyRepAnyRadix
repQtySmartly dflt (Abridged mp) (Amount cy qty)
  = case repQtyByPopularCommodity mp cy of
  Just f -> f qty
  Nothing -> case map snd . M.assocs $ mp of
    [] -> repQty dflt qty
    x:xs -> repQtyByPopularity (F.foldl' (<>) x xs) qty


-- | Returns a function representing a Qty based on the radix point
-- and grouping character most frequently seen.
repQtyByPopularity
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -- ^ History list
  -> Qty
  -> QtyRepAnyRadix
repQtyByPopularity = repQty . pickRadix


-- | If possible, returns a function representing a Qty based on the
-- representations that have been seen so far.  @historyRepresent m c@
-- is applied to a map, @m@, which holds all commodities that have
-- been seen with a quantity representation in their respective
-- 'Trio'.  The values in the map are, at minimum, the radix point,
-- and may also contain any grouping characters used.
-- 'historyRepresent' will return a function that renders 'Qty' for
-- that 'Commodity', but only if that 'Commodity' is a key in @m@.
repQtyByPopularCommodity
  :: M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer)))
  -- ^ History map
  -> Commodity
  -> Maybe (Qty -> QtyRepAnyRadix)
repQtyByPopularCommodity mp cy = fmap (repQty . pickRadix) (M.lookup cy mp)

-- | Picks the most popular radix point and, if possible, the most
-- popular grouping character corresponding to that radix.
pickRadix
  :: NonEmpty (Either (Seq RadCom) (Seq RadPer))
  -- ^ History list
  -> Either (Maybe RadCom) (Maybe RadPer)
pickRadix ne =
  let NonEmpty rdx _ = modeFromNonEmpty
        . fmap (either (const (Left Radix)) (const (Right Radix)))
        $ ne
  in case rdx of
      Left _ -> Left grpr
        where
          grpr = mimode . mconcat . lefts . seqFromNonEmpty $ ne
      Right _ -> Right grpr
        where
          grpr = mimode . mconcat . rights . seqFromNonEmpty $ ne

vote :: Posting -> History
vote = make . (^. to core . trio)
  where
    make tri = case trioRendering tri of
      Nothing -> mempty
      Just (cy, ar, ei) -> History
        $ M.singleton cy (NonEmpty (ar, ei) Seq.empty)

elect :: Foldable f => f Transaction -> History
elect = F.foldl' f mempty
  where
    f hist = F.foldl' g hist . postings
      where
        g acc pstg = acc `mappend` vote pstg


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
