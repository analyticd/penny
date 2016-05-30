{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Functions that create 'Cell' that are useful across multiple
-- reports, such as both the reports in "Penny.Columns" and in
-- "Penny.BalanceReport".

module Penny.Cell where

{-

module Penny.Cell
  ( Env(Env, _rowBackground, _history, _colors)
  , rowBackground
  , history
  , colors
  , sidedChunk
  , sideCell
  , commodityCell
  , spaceCell
  , textCell
  , qtyRepAnyRadixMagnitudeChunk
  , qtyRepAnyRadixMagnitudeCell
  , brimScalarAnyRadixMagnitudeChunk
  , brimScalarAnyRadixMagnitudeCell
  , qtyMagnitudeCell
  ) where

import Penny.Arrangement
import Penny.Colors
import Penny.Copper.Copperize
import Penny.Commodity
import Penny.Decimal
import Penny.Polar (Pole)
import qualified Penny.Polar as P
import Penny.Popularity
import Penny.Rep

import Control.Lens ( view, (^.), makeLenses )
import Data.Foldable (toList)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Text as X
import Rainbox
  ( Cell (Cell, _rows, _horizontal, _vertical, _background)
  , top
  , left
  , right
  )
import Rainbow


data Env = Env
  { _rowBackground :: Radiant
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env

sideCell
  :: Env
  -> Maybe Pole
  -> Cell
sideCell env maySide = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back (view rowBackground env) . fore fgColor
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = view rowBackground env
  }
  where
    (fgColor, txt) = case maySide of
      Nothing -> (env ^. colors.neutral, "--")
      Just side
        | side == P.debit -> (env ^. colors.debit, "<")
        | otherwise -> (env ^. colors.credit, ">")

commodityCell
  :: Env
  -> Maybe Pole
  -> Orient
  -> Commodity
  -> Cell
commodityCell env maySide orient cy = Cell
  { _rows = Seq.singleton . Seq.singleton
      . sidedChunk env maySide
      $ cy
  , _horizontal = top
  , _vertical = vertOrient
  , _background = view rowBackground env
  }
  where
    vertOrient
      | orient == CommodityOnLeft = right
      | otherwise = left


spaceCell :: Int -> Env -> Cell
spaceCell i env = textCell _nonLinear (view rowBackground env) (view colors env)
  (X.replicate i . X.singleton $ ' ')

-- | Makes a single cell with a Text.
textCell
  :: (Colors -> Radiant)
  -- ^ Selects the foreground color.
  -> Radiant
  -- ^ Background color
  -> Colors
  -> Text
  -- ^ The text to display
  -> Cell
textCell fg bkgd colors txt = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back bkgd . fore (fg colors)
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = bkgd
  }

sidedChunk
  :: Env
  -> Maybe Pole
  -> Text
  -> Chunk Text
sidedChunk env maySide
  = back (view rowBackground env)
  . fore fgColor
  . chunk
  where
    fgColor = case maySide of
      Nothing -> env ^. colors.neutral
      Just side
        | side == P.debit -> env ^. colors.debit
        | otherwise -> env ^. colors.credit

qtyRepAnyRadixMagnitudeChunk
  :: Env
  -> RepAnyRadix
  -> Chunk Text
qtyRepAnyRadixMagnitudeChunk env qr
  = sidedChunk env (pole'RepAnyRadix qr)
  . X.pack
  . toList
  . t'NilOrBrimAnyRadix
  . c'NilOrBrimAnyRadix'RepAnyRadix
  $ qr

qtyRepAnyRadixMagnitudeCell
  :: Env
  -> RepAnyRadix
  -> Cell
qtyRepAnyRadixMagnitudeCell env qr
  = textCell getColor (view rowBackground env) (view colors env)
  . X.pack
  . toList
  . t'NilOrBrimAnyRadix
  . c'NilOrBrimAnyRadix'RepAnyRadix
  $ qr
  where
    getColor = case pole'RepAnyRadix qr of
      Nothing -> _neutral
      Just side
        | side == P.debit -> _debit
        | otherwise -> _credit

brimScalarAnyRadixMagnitudeChunk
  :: Env
  -> Maybe Pole
  -> BrimAnyRadix
  -> Chunk Text
brimScalarAnyRadixMagnitudeChunk env maySide
  = sidedChunk env maySide
  . X.pack
  . toList
  . t'NilOrBrimAnyRadix
  . c'NilOrBrimAnyRadix'BrimAnyRadix

brimScalarAnyRadixMagnitudeCell
  :: Env
  -> Maybe Pole
  -> BrimAnyRadix
  -> Cell
brimScalarAnyRadixMagnitudeCell env maySide rnn
  = textCell getColor (view rowBackground env) (view colors env)
  . X.pack
  . toList
  . t'NilOrBrimAnyRadix
  . c'NilOrBrimAnyRadix'BrimAnyRadix
  $ rnn
  where
    getColor = case maySide of
      Nothing -> _neutral
      Just side
        | side == P.debit -> _debit
        | otherwise -> _credit

qtyMagnitudeCell
  :: Env
  -> Maybe Commodity
  -- ^ If a commodity is supplied, it is used to better determine how
  -- to render the Qty.
  -> Decimal
  -> Cell
qtyMagnitudeCell env mayCy
  = qtyRepAnyRadixMagnitudeCell env
  . repDecimal ei
  where
    ei = either (Left . Just) (Right . Just)
      . selectGrouper
      . Penny.Popularity.groupers (env ^. history)
      $ mayCy
-}
