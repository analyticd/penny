{-# LANGUAGE OverloadedStrings #-}
-- | The Postings report
module Penny.Cabin.Postings
  ( -- * Color schemes
    Parity(..)
  , C.CellTag(..)
  , Scheme(..)

  -- * Columns
  , ColumnFn
  , Column(..)

    -- * Making reports
  , postingsBox
  , makeRows
  , makeCell

  -- * Spacers
  , spacer

  -- * Columns for non-balance fields
  , date
  , account
  , number
  , payee
  , flag
  , amount
  , side
  , qty
  , commodity

  -- * Columns for balance fields
  , balAmounts
  , balSides
  , balQtys
  , balCommodities
  ) where

import Control.Applicative
import qualified Penny.Lincoln as L
import Rainbox
import Rainbow
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Penny.Cabin.Postings.Cell as C
import Data.Monoid

data Parity = Even | Odd
  deriving (Eq, Ord, Show)

data Scheme
  = Scheme (Parity -> Radiant) (C.CellTag -> Radiant)

type ColumnFn l
  = (L.Commodity -> L.Arrangement)
  -- ^ Arranges commodities and quantities
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -- ^ Renders amounts
  -> L.Tranche l
  -> l [(C.CellTag, Text)]

newtype Column l = Column (ColumnFn l)

postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => Scheme
  -- ^ Color scheme
  -> (L.Commodity -> L.Arrangement)
  -- ^ Arranges quantities and commodities
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -- ^ Renders amounts
  -> t1 (Column l)
  -- ^ All columns
  -> t2 (L.Tranche l)
  -- ^ 'L.Tranche' to include in report
  -> l Box
postingsBox sch arng rep cols
  = fmap (gridByRows . F.toList . fmap F.toList)
  . makeRows sch arng rep cols

makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => Scheme
  -- ^ Color scheme
  -> (L.Commodity -> L.Arrangement)
  -- ^ Arranges quantities and commodities
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -- ^ Renders amounts
  -> t1 (Column l)
  -- ^ All columns in report
  -> t2 (L.Tranche l)
  -- ^ All 'L.Tranche' to include in report
  -> l (t2 (t1 Cell))
makeRows (Scheme mkBak mkFore) arng rep cols
  = T.sequenceA . fmap T.sequenceA . fmap mkRow
  where
    mkRow trch = fmap (makeCell mkFore bkgnd arng rep trch) cols
      where
        prty = if even (L.naturalToInteger u) then Even else Odd
        L.Tranche _ (L.Serset (L.Forward (L.Serial u)) _) = trch
        bkgnd = mkBak prty

makeCell
  :: Applicative l
  => (C.CellTag -> Radiant)
  -- ^ Obtains foreground color
  -> Radiant
  -- ^ Background color
  -> (L.Commodity -> L.Arrangement)
  -- ^ Arranges quantities and commodities
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -- ^ Represents amounts
  -> L.Tranche l
  -- ^ 'L.Tranche' that is the subject of this cell
  -> Column l
  -- ^ Column that is the subject of this cell
  -> l Cell
makeCell mkFore bkgnd arng rep trch (Column colFn)
  = Cell <$> brs <*> pure right <*> pure top <*> pure bkgnd
  where
    brs = fmap (fmap mkBar) rws
    rws = colFn arng rep trch
    mkBar (tag, txt)
      = Bar [fromText txt <> back bkgnd <> fore (mkFore tag)]

spacer :: Applicative l => Int -> Column l
spacer i = Column $ \_ _ _ -> pure (C.spacer i)

date :: L.Ledger l => Column l
date = Column $ \_ _ -> C.date

account :: L.Ledger l => Column l
account = Column $ \_ _ -> C.account

number :: L.Ledger l => Column l
number = Column $ \_ _ -> C.number

payee :: L.Ledger l => Column l
payee = Column $ \_ _ -> C.payee

flag :: L.Ledger l => Column l
flag = Column $ \_ _ -> C.flag

side :: Applicative l => Column l
side = Column $ \_ _ -> pure . C.side

commodity :: L.Ledger l => Column l
commodity = Column $ \_ _ -> C.commodity

qty :: L.Ledger l => Column l
qty = Column $ \_ rp tr -> C.qty rp tr

amount :: L.Ledger l => Column l
amount = Column C.amount

balAmounts :: Applicative l => Column l
balAmounts = Column $ \ar rep tr -> pure (C.balAmounts ar rep tr)

balSides :: Applicative l => Column l
balSides = Column $ \_ _ tr -> pure (C.balSides tr)

balQtys :: Applicative l => Column l
balQtys = Column $ \_ rep tr -> pure (C.balQtys rep tr)

balCommodities :: Applicative l => Column l
balCommodities = Column $ \_ _ tr -> pure (C.balCommodities tr)
