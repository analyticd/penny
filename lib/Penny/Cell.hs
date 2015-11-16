{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Penny.Cell where

import Control.Lens (makeLenses)

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Colors
import Penny.Commodity
import Penny.DateTime
import Penny.Decimal
import Penny.Display
import Penny.Natural
import Penny.Polar (Pole, equatorial)
import qualified Penny.Polar as P
import Penny.Popularity
import Penny.Qty
import Penny.Realm
import Penny.Report
import Penny.Representation
import Penny.Scalar
import Penny.SeqUtil (intersperse)
import Penny.Serial
import Penny.Tree
import Penny.Troika

import Control.Lens
  ( to, view, (^.), makeWrapped, (|>), (<|), Getter, makeLenses,
    (<>~), _Wrapped )
import Data.Foldable (foldl', toList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import qualified Data.Time as Time
import Data.Monoid
import Data.Text (Text)
import qualified Data.Text as X
import Rainbox
  ( Cell (Cell, _rows, _horizontal, _vertical, _background)
  , rows
  , render
  , top
  , left
  , right
  , tableByColumns
  )
import Rainbow
import Rainbow.Types (yarn)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq


data Env = Env
  { _clatch :: Clatch
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
      . back (background (view clatch env) (view colors env)) . fore fgColor
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background (view clatch env) (view colors env)
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
  , _background = background (view clatch env) (view colors env)
  }
  where
    vertOrient
      | orient == CommodityOnLeft = right
      | otherwise = left


background :: Clatch -> Colors -> Radiant
background clatch colors
  | odd i = view oddBackground colors
  | otherwise = view evenBackground colors
  where
    i = view (postFiltset.forward.to naturalToInteger) clatch

spaceCell :: Int -> Env -> Cell
spaceCell i env = textCell _nonLinear (view clatch env) (view colors env)
  (X.replicate i . X.singleton $ ' ')

-- | Makes a single cell with a Text.
textCell
  :: (Colors -> Radiant)
  -- ^ Selects the foreground color.
  -> Clatch
  -> Colors
  -> Text
  -- ^ The text to display
  -> Cell
textCell fg clatch colors txt = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back (background clatch colors) . fore (fg colors)
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background clatch colors
  }

sidedChunk
  :: Env
  -> Maybe Pole
  -> Text
  -> Chunk Text
sidedChunk env maySide
  = back (background (view clatch env) (view colors env))
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
  = sidedChunk env (either equatorial equatorial qr)
  . X.pack
  . ($ "")
  . either (either display display) (either display display)
  . c'NilOrBrimScalarAnyRadix'RepAnyRadix
  $ qr

qtyRepAnyRadixMagnitudeCell
  :: Env
  -> RepAnyRadix
  -> Cell
qtyRepAnyRadixMagnitudeCell env qr
  = textCell getColor (view clatch env) (view colors env)
  . X.pack
  . ($ "")
  . either (either display display) (either display display)
  . c'NilOrBrimScalarAnyRadix'RepAnyRadix
  $ qr
  where
    getColor = case either equatorial equatorial qr of
      Nothing -> _neutral
      Just side
        | side == P.debit -> _debit
        | otherwise -> _credit

brimScalarAnyRadixMagnitudeChunk
  :: Env
  -> Maybe Pole
  -> BrimScalarAnyRadix
  -> Chunk Text
brimScalarAnyRadixMagnitudeChunk env maySide
  = sidedChunk env maySide
  . X.pack
  . ($ "")
  . either (either display display) (either display display)
  . c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix

brimScalarAnyRadixMagnitudeCell
  :: Env
  -> Maybe Pole
  -> BrimScalarAnyRadix
  -> Cell
brimScalarAnyRadixMagnitudeCell env maySide rnn
  = textCell getColor (view clatch env) (view colors env)
  . X.pack
  . ($ "")
  . either (either display display) (either display display)
  . c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix
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
