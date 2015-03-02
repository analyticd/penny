{-# LANGUAGE OverloadedStrings #-}
-- | The Postings report
module Penny.Cabin.Postings
  ( -- * Making reports
    postingsBox
  , makeRows

  -- * Columns
  , ColumnFn
  , Column(..)

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
import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Penny.Cabin.Postings.Cell as C

postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => (L.Commodity -> L.Arrangement)
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> t1 (Column l)
  -> t2 (L.Tranche l)
  -> l Box
postingsBox = undefined

{-
postingsBox
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l Box
postingsBox cols
  = fmap (gridByRows . F.toList . fmap F.toList)
  . makeRows cols
-}

makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => (L.Commodity -> L.Arrangement)
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> t1 (Column l)
  -> t2 (L.Tranche l)
  -> l (t2 (t1 Cell))
makeRows = undefined

{-
makeRows
  :: (Applicative l, T.Traversable t1, T.Traversable t2)
  => t1 (Tranche l -> l Cell)
  -> t2 (Tranche l)
  -> l (t2 (t1 Cell))
makeRows cols = T.sequenceA . fmap T.sequenceA . fmap mkRow
  where
    mkRow trch = fmap ($ trch) cols
-}

type ColumnFn l
  = (L.Commodity -> L.Arrangement)
  -> (L.Amount -> L.NilOrBrimScalarAnyRadix)
  -> L.Tranche l
  -> l [(C.CellTag, Text)]

newtype Column l = Column (ColumnFn l)

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
