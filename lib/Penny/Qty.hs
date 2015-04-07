-- | Quantities.  A quantity is a signed decimal number; however, it
-- always represents quantities that may be a debit or a credit (as
-- opposed to prices, which do not have a debit or credit.)
--
-- Unlike the types in "Penny.Representation", you can perform arithmetic
-- on these types.  Unlike the types in "Penny.Representation", these
-- types may not be \"represented\"--that is, displayed to the user in
-- a friendly way.  To display a 'Qty', convert it to a representation
-- type using one of the functions in this module.
module Penny.Qty
  ( -- * Qty
    Qty(..)
  , qtySide
  , HasQty(..)
  , decPositiveToQty

  -- * Non-zero Qty
  , QtyNonZero(..)
  , displayQtyNonZero
  , HasQtyNonZero(..)
  , qtyNonZeroToQty
  , qtyToQtyNonZero
  , qtyNonZeroSide

  -- * Unsigned Qty
  , QtyUnsigned(..)
  , HasQtyUnsigned(..)
  , qtyUnsignedToQtyWithSide
  , addSideSign
  , qtyUnsignedToQtyNoSide

  -- * Representations
  , repUngroupedQty
  , repUngroupedQtyNonZero
  , repUngroupedQtyUnsigned
  , repQty
  , repQtySmartly
  , repQtyByPopularity
  , repQtyByPopularCommodity
  , pickRadix
  ) where

import Penny.Decimal
import Penny.Natural
import Penny.Side
import Penny.Representation
import Penny.NonZero
import Penny.Offset
import Penny.PluMin
import Penny.NonEmpty
import Penny.Commodity
import Penny.Mimode
import Penny.Display
import Data.Sequence (Seq)
import qualified Data.Map.Strict as M
import qualified Data.Foldable as F
import Data.Semigroup

newtype Qty = Qty Decimal
  deriving (Eq, Ord, Show)

class HasQty a where
  toQty :: a -> Qty

instance Num Qty where
  Qty x + Qty y = Qty $ x + y
  Qty x - Qty y = Qty $ x - y
  Qty x * Qty y = Qty $ x * y
  negate (Qty x) = Qty (negate x)
  abs (Qty x) = Qty (abs x)
  signum (Qty x) = Qty (signum x)
  fromInteger i = Qty (fromInteger i)

qtySide :: Qty -> Maybe Side
qtySide (Qty (Decimal sig _))
  | sig < 0 = Just Debit
  | sig > 0 = Just Credit
  | otherwise = Nothing

newtype QtyNonZero = QtyNonZero DecNonZero
  deriving (Eq, Ord, Show)

-- | Displays a QtyNonZero using a period radix and no grouping.
displayQtyNonZero :: QtyNonZero -> String
displayQtyNonZero qnz =  case stripDecimalSign . (\(Qty d) -> d)
  . qtyNonZeroToQty $ qnz of
  Left dz -> display (repUngroupedDecZero rdx dz) ""
  Right (dp, pm) -> display (conv pm) . (' ':)
    . display (repUngroupedDecPositive rdx dp) $ ""
    where
      conv = fromSign :: PluMin -> Side
  where
    rdx = Radix :: Radix RadPer

class HasQtyNonZero a where
  toQtyNonZero :: a -> QtyNonZero

instance HasOffset QtyNonZero where
  offset (QtyNonZero dnz) = QtyNonZero (offset dnz)

qtyNonZeroToQty :: QtyNonZero -> Qty
qtyNonZeroToQty (QtyNonZero dnz) = Qty (decNonZeroToDecimal dnz)

qtyToQtyNonZero :: Qty -> Maybe QtyNonZero
qtyToQtyNonZero (Qty d) = fmap QtyNonZero $ decimalToDecNonZero d

qtyNonZeroSide :: QtyNonZero -> Side
qtyNonZeroSide (QtyNonZero (DecNonZero nz _))
  | i < 0 = Debit
  | otherwise = Credit
  where
    i = nonZeroToInteger nz

newtype QtyUnsigned = QtyUnsigned DecUnsigned
  deriving (Eq, Ord, Show)

class HasQtyUnsigned a where
  toQtyUnsigned :: a -> QtyUnsigned

qtyUnsignedToQtyWithSide :: Side -> QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyWithSide s (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Nothing
  | otherwise = Just
      (Qty (Decimal (addSideSign s . naturalToInteger $ sig) expt))

addSideSign :: Num a => Side -> a -> a
addSideSign Debit = negate
addSideSign Credit = id

qtyUnsignedToQtyNoSide :: QtyUnsigned -> Maybe Qty
qtyUnsignedToQtyNoSide (QtyUnsigned (DecUnsigned sig expt))
  | naturalToInteger sig == 0 = Just
      (Qty (Decimal 0 expt))
  | otherwise = Nothing

instance HasQty QtyNonZero where
  toQty (QtyNonZero (DecNonZero sig expt)) =
    Qty $ Decimal (nonZeroToInteger sig) expt

decPositiveToQty :: Side -> DecPositive -> Qty
decPositiveToQty s (DecPositive pos expt)
  = Qty $ Decimal (addSideSign s . naturalToInteger $ pos) expt

instance HasQty (QtyRep a) where
  toQty (QtyRep (NilOrBrimPolar cof)) = case cof of
    Center nil -> Qty . Decimal 0 . toExponent $ nil
    OffCenter brim s -> Qty . addSideSign s . toDecimal
      . toDecPositive $ brim

instance HasQty QtyRepAnyRadix where
  toQty (QtyRepAnyRadix ei) = case ei of
    Left q -> toQty q
    Right q -> toQty q

-- # Representing

-- | Represents a 'Qty', with optional grouping.
repQty
  :: Either (Maybe RadCom) (Maybe RadPer)
  -- ^ Determines which radix is used.  If you also supply a grouping
  -- character, 'repQty' will try to group the 'Qty' as well.
  -- Grouping will fail if the absolute value of the 'Qty' is less
  -- than @10000@.  In that case the 'Qty' will be represented without
  -- grouping.
  -> Qty
  -> QtyRepAnyRadix
repQty ei q = QtyRepAnyRadix eiq
  where
    eiq = case ei of
      Left Nothing -> Left . QtyRep . NilOrBrimPolar
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Right Nothing -> Right . QtyRep . NilOrBrimPolar
        $ case repUngroupedQty Radix q of
            Center nu -> Center $ NilU nu
            OffCenter bu s -> OffCenter (BrimUngrouped bu) s
      Left (Just grp) -> Left . QtyRep . NilOrBrimPolar $
        case repUngroupedQty Radix q of
          Center nu -> Center . NilU $ nu
          OffCenter bu s -> case groupBrimUngrouped grp bu of
            Nothing -> OffCenter (BrimUngrouped bu) s
            Just grpd -> OffCenter (BrimGrouped grpd) s
      Right (Just grp) -> Right . QtyRep . NilOrBrimPolar $
        case repUngroupedQty Radix q of
          Center nu -> Center . NilU $ nu
          OffCenter bu s -> case groupBrimUngrouped grp bu of
            Nothing -> OffCenter (BrimUngrouped bu) s
            Just grpd -> OffCenter (BrimGrouped grpd) s


repUngroupedQty
  :: Radix r
  -> Qty
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) Side
repUngroupedQty rdx (Qty dec) = case repUngroupedDecimal rdx dec of
  Center n -> Center n
  OffCenter o p -> OffCenter o (fromSign p)

repUngroupedQtyNonZero
  :: Radix r
  -> QtyNonZero
  -> (BrimUngrouped r, Side)
repUngroupedQtyNonZero rdx (QtyNonZero qnz) = (bu, side)
  where
    (bu, sgn) = repUngroupedDecNonZero rdx qnz
    side = fromSign sgn

repUngroupedQtyUnsigned
  :: Radix r
  -> QtyUnsigned
  -> CenterOrOffCenter (NilUngrouped r) (BrimUngrouped r) ()
repUngroupedQtyUnsigned rdx (QtyUnsigned qu) = repUngroupedDecUnsigned rdx qu

-- ## Popularity representation


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
  -> M.Map Commodity (NonEmpty (Either (Seq RadCom) (Seq RadPer)))
  -- ^ History map
  -> Commodity
  -- ^ Associated commodity
  -> Qty
  -- ^ Render this 'Qty'
  -> QtyRepAnyRadix
repQtySmartly dflt mp cy = case repQtyByPopularCommodity mp cy of
  Just f -> f
  Nothing -> case map snd . M.assocs $ mp of
    [] -> repQty dflt
    x:xs -> repQtyByPopularity (F.foldl' (<>) x xs)

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

lefts :: F.Foldable f => f (Either a b) -> [a]
lefts = F.foldr f []
  where
    f ei acc = case ei of
      Left l -> l : acc
      Right _ -> acc

rights :: F.Foldable f => f (Either a b) -> [b]
rights = F.foldr f []
  where
    f ei acc = case ei of
      Left _ -> acc
      Right r -> r : acc


