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
import Penny.Cell
import Penny.Clatch
import Penny.Colors
import Penny.Decimal
import Penny.Display
import Penny.Natural
import Penny.Polar (Pole, equatorial)
import qualified Penny.Polar as P
import Penny.Popularity
import Penny.Qty
import Penny.Realm
import Penny.Report
import Penny.Rep
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

newtype Columns = Columns (Env -> Clatch -> Seq Cell)

makeWrapped ''Columns

instance Monoid Columns where
  mempty = Columns (const mempty)
  mappend (Columns cx) (Columns cy)
    = Columns (\a c -> (cx a c) <> (cy a c))


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

spacerColumn
  :: Colors
  -> Seq Clatch
  -> Seq Cell
spacerColumn clrs = fmap mkSpacerCell
  where
    mkSpacerCell clatch = textCell _nonLinear
      (background clatch clrs) clrs "  "

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
  . Seq.zipWith mkDataRow colorSeq
  $ clatches
  where
    mkDataRow bkgd clatch = ($ clatch) . ($ env) . (view _Wrapped) $ col
      where
        env = Env bkgd hist clrs
    colorSeq = Seq.fromList . take (Seq.length clatches)
      . concat . repeat $ [_evenBackground clrs, _oddBackground clrs]

instance Report Columns where
  printReport sq clrs hist cltchs
    = table hist clrs (foldl' mappend mempty sq) cltchs


-- | Things that can create columns.  Instances of this class must
-- always create a 'Seq' of 'Cell' that is the exact same length,
-- regardless of whether the cells contain anything.
class Colable a where
  column :: Getter Clatch a -> Columns


instance Colable Text where
  column f = Columns $ \env clatch -> Seq.singleton $
    textCell _nonLinear (view rowBackground env) (view colors env)
             (view f clatch)

instance Colable (Seq Text) where
  column f = Columns $ \env clatch -> Seq.singleton $
    textCell _nonLinear (view rowBackground env) (view colors env)
             (foldl (<>) mempty . intersperse "•" . view f $ clatch)

spaces :: Int -> Columns
spaces i = column (to (const ((X.replicate i . X.singleton $ ' '))))

singleCell
  :: Colable a
  => Env
  -> Clatch
  -> a
  -> Seq Cell
singleCell env cltch a
  = ($ cltch) . ($ env) . (view _Wrapped) $ column (to (const a))

instance Colable Bool where
  column f = Columns cell
    where
      cell env clatch = Seq.singleton $ Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (view rowBackground env) . fore fg
            . chunk . X.singleton $ char
        , _horizontal = top
        , _vertical = left
        , _background = view rowBackground env
        }
        where
          (char, fg)
            | view f clatch = ('T', green)
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
      g env clatch = case view f clatch of
        Nothing -> singleCell env clatch (X.empty)
        Just v -> singleCell env clatch v

instance Colable (Maybe Pole) where
  column f = Columns $ \env clatch ->
    Seq.singleton (sideCell env (view f clatch))

-- | Creates two columns: one for the side and one for the magnitude.

instance Colable RepAnyRadix where
  column f = Columns getCells
    where
      getCells env clatch = sideCell env maySide
        <| qtyRepAnyRadixMagnitudeCell env (view f clatch)
        <| Seq.empty
        where
          maySide = either equatorial equatorial (view f clatch)

-- | Creates two columns: one for the side and one for the magnitude.
instance Colable Qty where
  column f = Columns getCells
    where
      getCells env clatch = sideCell env (view (_Wrapped . to equatorial) qty)
        <| qtyMagnitudeCell env Nothing (view _Wrapped qty)
        <| Seq.empty
        where
          qty = view f clatch

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
            US rnn _ -> brimScalarAnyRadixMagnitudeChunk env side rnn
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

    bkgd = view rowBackground env

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
      . view f

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
      . view _Wrapped
      . view f
      where
        makeTroika (cy, qty) = Troika cy (Right qty)

-- | Creates two columns, one for the forward serial and one for the
-- backward serial.

instance Colable Serset where
  column f = Columns getCells
    where
      getCells env clatch = fwdCell <> revCell
        where
          srst = view f clatch
          fwdCell = singleCell env clatch (srst ^. forward)
          revCell = singleCell env clatch (srst ^. backward)

-- | Creates four columns: two for the file serset and two for the
-- global serset.
instance Colable Serpack where
  column f = Columns getCells
    where
      getCells env clatch = fileCells <> glblCells
        where
          serpack = view f clatch
          fileCells = singleCell env clatch (serpack ^. file)
          glblCells = singleCell env clatch (serpack ^. global)

-- | Creates one column with a @U@ or an @S@.
instance Colable Realm where
  column f = Columns getCells
    where
      getCells env clatch = Seq.singleton
        $ textCell _nonLinear (view rowBackground env) (view colors env) txt
        where
          txt = case view f clatch of
            User -> "U"
            System -> "S"

colableDisplayNonLinear :: Display a => Getter Clatch a -> Columns
colableDisplayNonLinear f = Columns getCells
    where
      getCells env clatch = Seq.singleton
        $ textCell _nonLinear (view rowBackground env) (view colors env) txt
        where
          txt = X.pack . ($ "") . display . view f $ clatch

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
      getCells env clatch = case view f clatch of
        Nothing -> Seq.singleton
          $ textCell _nonLinear (view rowBackground env) (view colors env) "--"
        Just sc -> Seq.singleton
          $ textCell _nonLinear (view rowBackground env) (view colors env)
          . X.pack . ($ "") . display $ sc

-- | Shows the scalar.  Does not show the children; if there are
-- children, a ↓ is shown at the end.
instance Colable Tree where
  column f = Columns getCells
    where
      getCells env clatch = Seq.singleton
        $ textCell _nonLinear (view rowBackground env) (view colors env) txt
        where
          txt = scalarTxt <> childrenTxt
            where
              scalarTxt = case view (f . scalar) clatch of
                Nothing -> "--"
                Just sc -> X.pack . ($ "") . display $ sc
              childrenTxt
                | view (f . children . to Seq.null) clatch = mempty
                | otherwise = "↓"


-- | Shows each tree, separated by a •.
instance Colable (Seq Tree) where
  column f = Columns getCells
    where
      getCells env clatch = Seq.singleton
        $ textCell _nonLinear (view rowBackground env) (view colors env) txt
        where
          txt = foldr (<>) mempty
            . intersperse "•" . fmap treeToTxt
            . view f $ clatch
            where
              treeToTxt tree = scalarTxt <> childrenTxt
                where
                  scalarTxt = case view scalar tree of
                    Nothing -> "--"
                    Just sc -> X.pack . ($ "") . display $ sc
                  childrenTxt
                    | view (Penny.Tree.children . to Seq.null) tree = mempty
                    | otherwise = "↓"


-- | Shows each Scalar, each separated by a bullet.
instance Colable (Seq Scalar) where
  column f = Columns getCells
    where
      getCells env clatch = Seq.singleton $ textCell _nonLinear
        (view rowBackground env) (view colors env) txt
        where
          txt = foldl (<>) mempty
            . intersperse "•" . fmap (X.pack . ($ "") . display)
            . view f $ clatch
