{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Penny.Columns where

import Control.Lens
import Data.Foldable (foldl', toList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Commodity
import Penny.DateTime
import Penny.Decimal
import Penny.Display
import Penny.Natural
import Penny.Popularity
import Penny.Qty
import Penny.Realm
import Penny.Representation
import Penny.Scalar
import Penny.SeqUtil (intersperse)
import Penny.Serial
import Penny.Tree
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
import Penny.Troika
import Penny.Polar (Pole, equatorial)
import qualified Penny.Polar as P

-- | Load data into this record to make a color scheme that has
-- different colors for debits and credits, with an alternating
-- background for odd- and even-numbered postings.
data Colors = Colors
  { _debit :: Radiant
  , _credit :: Radiant
  , _neutral :: Radiant
  , _nonLinear :: Radiant
  , _notice :: Radiant
  , _oddBackground :: Radiant
  , _evenBackground :: Radiant
  } deriving (Eq, Ord, Show)

makeLenses ''Colors

instance Monoid Colors where
  mempty = Colors
    { _debit = mempty
    , _credit = mempty
    , _neutral = mempty
    , _nonLinear = mempty
    , _notice = mempty
    , _oddBackground = mempty
    , _evenBackground = mempty
    }

  mappend (Colors x0 x1 x2 x3 x4 x5 x6) (Colors y0 y1 y2 y3 y4 y5 y6)
    = Colors (x0 <> y0) (x1 <> y1) (x2 <> y2) (x3 <> y3)
             (x4 <> y4) (x5 <> y5) (x6 <> y6)

-- | Colors for use on a terminal with a dark background.

dark :: Colors
dark = Colors
  { _debit = cyan
  , _credit = magenta
  , _neutral = yellow
  , _nonLinear = mempty
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = mempty <> only256 (color256 236)
  }

-- | Colors for use on a terminal with a light background.

light :: Colors
light = Colors
  { _debit = cyan
  , _credit = magenta
  , _neutral = yellow
  , _nonLinear = mempty
  , _notice = red
  , _oddBackground = mempty
  , _evenBackground = mempty <> only256 (color256 255)
  }

data Env = Env
  { _clatch :: Clatch
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env

newtype Columns = Columns (Env -> Seq Cell)

makeWrapped ''Columns

instance Monoid Columns where
  mempty = Columns (const mempty)
  mappend (Columns cx) (Columns cy)
    = Columns (\a -> (cx a) <> (cy a))


background :: Clatch -> Colors -> Radiant
background clatch colors
  | odd i = colors ^. oddBackground
  | otherwise = colors ^. evenBackground
  where
    i = clatch ^. to postFiltset.forward.to naturalToInteger

-- | Removes all entirely empty columns from a table.
removeEmptyColumns
  :: IM.IntMap (Seq Cell)
  -> IM.IntMap (Seq Cell)
removeEmptyColumns mp
  = foldl' (flip removeIfEmpty) mp (IM.keys mp)

-- | Removes the column with the given index if it is empty.
removeIfEmpty :: Int -> IM.IntMap (Seq Cell) -> IM.IntMap (Seq Cell)
removeIfEmpty idx mp
  | columnIsEmpty = IM.delete idx mp
  | otherwise = mp
  where
    columnIsEmpty = case IM.lookup idx mp of
      Nothing -> False
      Just column -> all cellIsEmpty column
        where
          cellIsEmpty cell = all cellRowIsEmpty (cell ^. rows)
            where
              cellRowIsEmpty cellRow = all chunkIsEmpty cellRow
                where
                  chunkIsEmpty chunk = X.null (chunk ^. yarn)

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
      . back (background clatch colors) . fore (colors ^. to fg)
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background clatch colors
  }

spacerColumn
  :: Colors
  -> Seq Clatch
  -> Seq Cell
spacerColumn clrs = fmap mkSpacerCell
  where
    mkSpacerCell clatch = textCell _nonLinear clatch clrs " "

addRowToMap :: IM.IntMap (Seq Cell) -> Seq Cell -> IM.IntMap (Seq Cell)
addRowToMap mp = foldl' addColToMap mp . zip [0..] . toList
  where
    addColToMap mp (idx, cell) = IM.alter alterer idx mp
      where
        alterer mayVal = Just $ case mayVal of
          Nothing -> Seq.singleton cell
          Just sq -> sq |> cell

-- | Creates a table from a 'Columns'.  Deletes any column that is
-- entirely empty, then intersperses single-space spacer columns.
table
  :: History
  -> Colors
  -> Columns
  -> Seq Clatch
  -> Seq (Chunk Text)
table hist clrs col clatches
  = render
  . tableByColumns
  . intersperse (spacerColumn clrs clatches)
  . Seq.fromList
  . IM.elems
  . removeEmptyColumns
  . foldl' addRowToMap IM.empty
  . fmap (mkDataRow $)
  $ clatches
  where
    mkDataRow clatch = ($ env) . (^. _Wrapped) $ col
      where
        env = Env clatch hist clrs


class Colable a where
  column :: (Clatch -> a) -> Columns

instance Colable Text where
  column f = Columns $ \env -> Seq.singleton $
    textCell _nonLinear (env ^. clatch) (env ^. colors) (f (env ^. clatch))

spaces :: Int -> Columns
spaces i = column (const ((X.replicate i . X.singleton $ ' ')))

spaceCell :: Int -> Env -> Cell
spaceCell i env = textCell _nonLinear (env ^. clatch) (env ^. colors)
  (X.replicate i . X.singleton $ ' ')

singleCell
  :: Colable a
  => Env
  -> a
  -> Seq Cell
singleCell env a = ($ env) . (^. _Wrapped) $ column (const a)

instance Colable Bool where
  column f = Columns cell
    where
      cell env = Seq.singleton $ Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background (env ^. clatch) (env ^. colors)) . fore fg
            . chunk . X.singleton $ char
        , _horizontal = top
        , _vertical = left
        , _background = background (env ^. clatch) (env ^. colors)
        }
        where
          (char, fg)
            | f (_clatch env) = ('T', green)
            | otherwise = ('F', red)

instance Colable Integer where
  column f = column (X.pack . show . f)

instance Colable Unsigned where
  column f = column (naturalToInteger . f)

instance Colable a => Colable (Maybe a) where
  column f = Columns g
    where
      g env = case f (env ^. clatch) of
        Nothing -> mempty
        Just v -> singleCell env v

sideCell
  :: Env
  -> Maybe Pole
  -> Cell
sideCell env maySide = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back (background (env ^. clatch) (env ^. colors)) . fore fgColor
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background (env ^. clatch) (env ^. colors)
  }
  where
    (fgColor, txt) = case maySide of
      Nothing -> (env ^. colors.neutral, "--")
      Just side
        | side == P.debit -> (env ^. colors.debit, "<")
        | otherwise -> (env ^. colors.credit, ">")

sidedChunk
  :: Env
  -> Maybe Pole
  -> Text
  -> Chunk Text
sidedChunk env maySide
  = back (background (env ^. clatch) (env ^. colors))
  . fore fgColor
  . chunk
  where
    fgColor = case maySide of
      Nothing -> env ^. colors.neutral
      Just side
        | side == P.debit -> env ^. colors.debit
        | otherwise -> env ^. colors.credit

commodityCell
  :: Env
  -> Maybe Pole
  -> Orient
  -> Commodity
  -> Cell
commodityCell env maySide orient (Commodity cy) = Cell
  { _rows = Seq.singleton . Seq.singleton
      . sidedChunk env maySide
      $ cy
  , _horizontal = top
  , _vertical = vertOrient
  , _background = background (env ^. clatch) (env ^. colors)
  }
  where
    vertOrient
      | orient == CommodityOnLeft = right
      | otherwise = left


instance Colable (Maybe Pole) where
  column f = Columns $ \env ->
    Seq.singleton (sideCell env (f (_clatch env)))

instance Colable Pole where
  column f = Columns $ \env ->
    Seq.singleton (sideCell env (Just . f . _clatch $ env))

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
  = textCell getColor (env ^. clatch) (env ^. colors)
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
  = textCell getColor (env ^. clatch) (env ^. colors)
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

-- | Creates two columns: one for the side and one for the magnitude.

instance Colable RepAnyRadix where
  column f = Columns getCells
    where
      getCells env = sideCell env maySide
        <| qtyRepAnyRadixMagnitudeCell env (f . _clatch $ env)
        <| Seq.empty
        where
          maySide = either equatorial equatorial (f . _clatch $ env)

-- | Creates two columns: one for the side and one for the magnitude.
instance Colable Qty where
  column f = Columns getCells
    where
      getCells env = sideCell env (qty ^. _Wrapped . to equatorial)
        <| qtyMagnitudeCell env Nothing (qty ^. _Wrapped)
        <| Seq.empty
        where
          qty = f . _clatch $ env

instance Colable Commodity where
  column f = column ((^. _Wrapped) . f)

data TroikaCells = TroikaCells
  { _tmSide :: Maybe Pole
    -- ^ Always top left aligned, with standard background
  , _tmCyOnLeft :: Maybe (Chunk Text)
  -- ^ Always top right aligned.
  , _tmMagWithCy :: Chunk Text
  -- ^ Always top left aligned.
  , _tmCyOnRight :: Maybe (Chunk Text)
  -- ^ Always top left aligned.
  }

makeLenses ''TroikaCells

troimountCells :: Env -> Troika -> TroikaCells
troimountCells env troimount = TroikaCells side onLeft magWithCy onRight
  where
    cy = troimount ^. Penny.Troika.commodity
    side = troimount ^. troiquant . to equatorial
    hasSpace = spaceBetween (env ^. history) (Just cy)
    orient = orientation (env ^. history) (Just cy)
    cyChunk = sidedChunk env side (cy ^. _Wrapped)
    (onLeft, onRight)
      | not hasSpace = (Nothing, Nothing)
      | orient == CommodityOnLeft = (Just cyChunk, Nothing)
      | otherwise = (Nothing, Just cyChunk)
    grouper = either (Left . Just) (Right . Just)
      . selectGrouper
      . Penny.Popularity.groupers (env ^. history)
      . Just
      $ cy
    magWithCy
      | hasSpace = magnitude
      | orient == CommodityOnLeft = cyChunk <> magnitude
      | otherwise = magnitude <> cyChunk
      where
        magnitude = case troimount ^. troiquant of
          Left troiload -> case troiload of
            QC q _ -> qtyRepAnyRadixMagnitudeChunk env q
            Q q -> qtyRepAnyRadixMagnitudeChunk env q
            UC rnn _ _ -> brimScalarAnyRadixMagnitudeChunk env side rnn
            U rnn _ -> brimScalarAnyRadixMagnitudeChunk env side rnn
            _ -> qtyRepAnyRadixMagnitudeChunk env
              . repDecimal grouper . toDecimal $ troiload
          Right qty -> qtyRepAnyRadixMagnitudeChunk env
            . repDecimal grouper $ qty

troimountCellsToColumns
  :: Env
  -> Seq TroikaCells
  -> Seq Cell
troimountCellsToColumns env
  = tupleToSeq
  . foldl addRow emptyTup
  where
    tupleToSeq (c0, c1, c2, c3)
      = c0 <| c1 <| c2 <| c3 <| Seq.empty

    emptyTup =
      ( Cell Seq.empty top left bkgd              -- side
      , Cell Seq.empty top right bkgd             -- cy on left
      , Cell Seq.empty top left bkgd              -- magnitude
      , Cell Seq.empty top left bkgd              -- cy on right
      )

    bkgd = background (env ^. clatch) (env ^. colors)

    addRow (side, cyOnLeft, mag, cyOnRight) tc
      = ( addLine side side'
        , addLine cyOnLeft cyOnLeft'
        , addLine mag mag'
        , addLine cyOnRight cyOnRight'
        )
      where
        addLine old line = old & rows <>~ Seq.singleton line
        side' = Seq.singleton . sidedChunk env (tc ^. tmSide) $
          case tc ^. tmSide of
            Nothing -> "--"
            Just sd
              | sd == P.debit -> "<"
              | otherwise -> ">"
        cyOnLeft' = maybe Seq.empty Seq.singleton . _tmCyOnLeft $ tc
        mag' = Seq.singleton . _tmMagWithCy $ tc
        cyOnRight' = maybe Seq.empty Seq.singleton . _tmCyOnRight $ tc


-- | Creates four columns:
--
-- 0.  Side
-- 1.  Separate commodity on left
-- 2.  Magnitude (with commodity on left or right, if applicable)
-- 3.  Separate commodity on right

instance Colable Troika where
  column f = Columns getCells where
    getCells env = troimountCellsToColumns env
      . Seq.singleton
      . troimountCells env
      . f
      . Control.Lens.view clatch
      $ env

-- | Creates same columns as 'Troika'.
instance Colable Amount where
  column f = column (c'Troika'Amount . f)

-- | Creates the same columns as for 'Amount', but with one line
-- for each commodity in the balance.

instance Colable Balance where
  column f = Columns getCells where
    getCells env = troimountCellsToColumns env
      . fmap (troimountCells env . makeTroika)
      . Seq.fromList
      . M.assocs
      . Control.Lens.view _Wrapped
      . f
      . Control.Lens.view clatch
      $ env
      where
        makeTroika (cy, qty) = Troika cy (Right qty)

-- | Creates two columns, one for the forward serial and one for the
-- backward serial.

instance Colable Serset where
  column f = Columns getCells
    where
      getCells env =
        fwdCell
        <> revCell
        where
          srst = f . _clatch $ env
          fwdCell = singleCell env (srst ^. forward)
          revCell = singleCell env (srst ^. backward)

-- | Creates four columns: two for the file serset and two for the
-- global serset.
instance Colable Serpack where
  column f = Columns getCells
    where
      getCells env
        = fileCells
        <> glblCells
        where
          serpack = f . _clatch $ env
          fileCells = singleCell env (serpack ^. file)
          glblCells = singleCell env (serpack ^. global)

-- | Creates one column with a @U@ or an @S@.
instance Colable Realm where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (env ^. clatch) (env ^. colors) txt
        where
          txt = case f . _clatch $ env of
            User -> "U"
            System -> "S"

colableDisplayNonLinear :: Display a => (Clatch -> a) -> Columns
colableDisplayNonLinear f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (env ^. clatch) (env ^. colors) txt
        where
          txt = X.pack . ($ "") . display . f . _clatch $ env

-- | Creates a single column with the date in YYYY-MM-DD format.

instance Colable Date where
  column = colableDisplayNonLinear

instance Colable Time where
  column = colableDisplayNonLinear

instance Colable Zone where
  column = colableDisplayNonLinear

instance Colable DateTime where
  column = colableDisplayNonLinear

instance Colable Scalar where
  column = colableDisplayNonLinear

instance Colable (Maybe Scalar) where
  column f = Columns getCells
    where
      getCells env = case f . _clatch $ env of
        Nothing -> Seq.singleton
          $ textCell _nonLinear (env ^. clatch) (env ^. colors) "--"
        Just sc -> Seq.singleton
          $ textCell _nonLinear (env ^. clatch) (env ^. colors)
          . X.pack . ($ "") . display $ sc

-- | Shows the scalar.  Does not show the children; if there are
-- children, a ↓ is shown at the end.
instance Colable Tree where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (env ^. clatch) (env ^. colors) txt
        where
          txt = scalarTxt <> childrenTxt
            where
              scalarTxt = case _scalar . f . _clatch $ env of
                Nothing -> "--"
                Just sc -> X.pack . ($ "") . display $ sc
              childrenTxt
                | Seq.null . _children . f . _clatch $ env = mempty
                | otherwise = "↓"

-- | Shows each tree, separated by a •.
instance Colable (Seq Tree) where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (env ^. clatch) (env ^. colors) txt
        where
          txt = foldl (<>) mempty
            . intersperse "•" . fmap treeToTxt
            . f . _clatch $ env
            where
              treeToTxt tree = scalarTxt <> childrenTxt
                where
                  scalarTxt = case tree ^. scalar of
                    Nothing -> "--"
                    Just sc -> X.pack . ($ "") . display $ sc
                  childrenTxt
                    | tree ^. Penny.Tree.children . to Seq.null = mempty
                    | otherwise = "↓"
