{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Penny.Columns where

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Colors
import Penny.Columns.Env
import Penny.Commodity
import Penny.Decimal
import Penny.Display
import Penny.Grammar
  (BrimScalarAnyRadix)
import Penny.Grammar.Convert
import Penny.Natural
import Penny.Polar (Pole, equatorial)
import qualified Penny.Polar as P
import Penny.Popularity
import Penny.Qty
import Penny.Realm
import Penny.Rep
import Penny.Report
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

newtype Columns = Columns (Env -> Seq Cell)

makeWrapped ''Columns

instance Monoid Columns where
  mempty = Columns (const mempty)
  mappend (Columns cx) (Columns cy)
    = Columns (\a -> (cx a) <> (cy a))


background :: Clatch -> Colors -> Radiant
background clatch colors
  | odd i = view oddBackground colors
  | otherwise = view evenBackground colors
  where
    i = view (postFiltset.forward.to naturalToInteger) clatch

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
          cellIsEmpty cell = all cellRowIsEmpty (view rows cell)
            where
              cellRowIsEmpty cellRow = all chunkIsEmpty cellRow
                where
                  chunkIsEmpty chunk = X.null (view yarn chunk)

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

spacerColumn
  :: Colors
  -> Seq Clatch
  -> Seq Cell
spacerColumn clrs = fmap mkSpacerCell
  where
    mkSpacerCell clatch = textCell _nonLinear clatch clrs "  "

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
    mkDataRow clatch = ($ env) . (view _Wrapped) $ col
      where
        env = Env clatch hist clrs

instance Report Columns where
  printReport sq clrs hist cltchs
    = table hist clrs (foldl' mappend mempty sq) cltchs


-- | Things that can create columns.  Instances of this class must
-- always create a 'Seq' of 'Cell' that is the exact same length,
-- regardless of whether the cells contain anything.
class Colable a where
  column :: Getter Clatch a -> Columns


instance Colable Text where
  column f = Columns $ \env -> Seq.singleton $
    textCell _nonLinear (view clatch env) (view colors env)
             (view (clatch . f) env)

instance Colable (Seq Text) where
  column f = Columns $ \env -> Seq.singleton $
    textCell _nonLinear (view clatch env) (view colors env)
             (foldl (<>) mempty . intersperse "•" . view (clatch . f) $ env)

spaces :: Int -> Columns
spaces i = column (to (const ((X.replicate i . X.singleton $ ' '))))

spaceCell :: Int -> Env -> Cell
spaceCell i env = textCell _nonLinear (view clatch env) (view colors env)
  (X.replicate i . X.singleton $ ' ')

singleCell
  :: Colable a
  => Env
  -> a
  -> Seq Cell
singleCell env a = ($ env) . (view _Wrapped) $ column (to (const a))

instance Colable Bool where
  column f = Columns cell
    where
      cell env = Seq.singleton $ Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background (view clatch env) (view colors env)) . fore fg
            . chunk . X.singleton $ char
        , _horizontal = top
        , _vertical = left
        , _background = background (view clatch env) (view colors env)
        }
        where
          (char, fg)
            | view (clatch . f) env = ('T', green)
            | otherwise = ('F', red)

instance Colable Integer where
  column f = column (f . to (X.pack . show))

instance Colable Int where
  column f = column (f . to (X.pack . show))

instance Colable Unsigned where
  column f = column (f . to naturalToInteger)

instance Colable a => Colable (Maybe a) where
  column f = Columns g
    where
      g env = case view (clatch . f) env of
        Nothing -> singleCell env (X.empty)
        Just v -> singleCell env v

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


instance Colable (Maybe Pole) where
  column f = Columns $ \env ->
    Seq.singleton (sideCell env (view (clatch . f) env))

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

-- | Creates two columns: one for the side and one for the magnitude.

instance Colable RepAnyRadix where
  column f = Columns getCells
    where
      getCells env = sideCell env maySide
        <| qtyRepAnyRadixMagnitudeCell env (view (clatch . f) env)
        <| Seq.empty
        where
          maySide = either equatorial equatorial (view (clatch . f) env)

-- | Creates two columns: one for the side and one for the magnitude.
instance Colable Qty where
  column f = Columns getCells
    where
      getCells env = sideCell env (view (_Wrapped . to equatorial) qty)
        <| qtyMagnitudeCell env Nothing (view _Wrapped qty)
        <| Seq.empty
        where
          qty = view (clatch . f) env

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
    hasSpace = isSpaceBetween (env ^. history) (Just cy)
    orient = orientation (env ^. history) (Just cy)
    cyChunk = sidedChunk env side cy
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

    bkgd = background (view clatch env) (view colors env)

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
      . Control.Lens.view (clatch . f)
      $ env

-- | Creates same columns as 'Troika'.
instance Colable Amount where
  column f = column (f . to c'Troika'Amount)

-- | Creates the same columns as for 'Amount', but with one line
-- for each commodity in the balance.

instance Colable Balance where
  column f = Columns getCells where
    getCells env = troimountCellsToColumns env
      . fmap (troimountCells env . makeTroika)
      . Seq.fromList
      . M.assocs
      . Control.Lens.view _Wrapped
      . Control.Lens.view (clatch . f)
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
          srst = view (clatch . f) env
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
          serpack = view (clatch . f) env
          fileCells = singleCell env (serpack ^. file)
          glblCells = singleCell env (serpack ^. global)

-- | Creates one column with a @U@ or an @S@.
instance Colable Realm where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (view clatch env) (view colors env) txt
        where
          txt = case view (clatch . f) env of
            User -> "U"
            System -> "S"

colableDisplayNonLinear :: Display a => Getter Clatch a -> Columns
colableDisplayNonLinear f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (view clatch env) (view colors env) txt
        where
          txt = X.pack . ($ "") . display . view (clatch . f) $ env

-- | Creates a single column with the date in YYYY-MM-DD format.

instance Colable Time.Day where
  column = colableDisplayNonLinear

instance Colable Time.TimeOfDay where
  column = colableDisplayNonLinear

instance Colable Scalar where
  column = colableDisplayNonLinear

instance Colable (Maybe Scalar) where
  column f = Columns getCells
    where
      getCells env = case view (clatch . f) $ env of
        Nothing -> Seq.singleton
          $ textCell _nonLinear (view clatch env) (view colors env) "--"
        Just sc -> Seq.singleton
          $ textCell _nonLinear (view clatch env) (view colors env)
          . X.pack . ($ "") . display $ sc

-- | Shows the scalar.  Does not show the children; if there are
-- children, a ↓ is shown at the end.
instance Colable Tree where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (view clatch env) (view colors env) txt
        where
          txt = scalarTxt <> childrenTxt
            where
              scalarTxt = case view (clatch . f . scalar) env of
                Nothing -> "--"
                Just sc -> X.pack . ($ "") . display $ sc
              childrenTxt
                | view (clatch . f . children . to Seq.null) env = mempty
                | otherwise = "↓"

{-
-- | Shows each tree, separated by a •.
instance Colable (Seq Tree) where
  column f = Columns getCells
    where
      getCells env = Seq.singleton
        $ textCell _nonLinear (view clatch env) (view colors env) txt
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

-- | Shows each Scalar, each separated by a bullet.
instance Colable (Seq Scalar) where
  column f = Columns getCells
    where
      getCells env = Seq.singleton $ textCell _nonLinear (view clatch env)
        (view colors env) txt
        where
          txt = foldl (<>) mempty
            . intersperse "•" . fmap (X.pack . ($ "") . display)
            . f . _clatch $ env
-}
