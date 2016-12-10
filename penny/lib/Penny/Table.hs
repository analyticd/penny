{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Penny.Table where

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Cell
import Penny.Clatch.Types
import qualified Penny.Clatch.Access.Balance as AB
import Penny.Clatch.Access.PostFiltset (postFiltset)
import qualified Penny.Clatch.Access.Posting as AP
import qualified Penny.Clatch.Access.TransactionX as AT
import Penny.Colors
import Penny.Copper.Copperize
import Penny.Decimal
import Penny.Cursor
import Penny.NonNegative
import Penny.Polar (Pole)
import qualified Penny.Polar as P
import Penny.Popularity
import Penny.Rep
import Penny.Report
import Penny.SeqUtil (intersperse)
import Penny.Serial (Serset, Serpack)
import qualified Penny.Serial as Serial
import Penny.Troika

import Control.Lens
  ( to, view, (^.), (|>), makeLenses,
    (<>~), _Wrapped )
import Control.Monad (join)
import Data.Foldable (foldl', toList)
import qualified Data.Map as M
import qualified Data.IntMap as IM
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
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

data Column
  = W1 (Env -> Clatch (Maybe Cursor) -> Cell)
  | W2 (Env -> Clatch (Maybe Cursor) -> (Cell, Cell))
  | W3 (Env -> Clatch (Maybe Cursor) -> (Cell, Cell, Cell))
  | W4 (Env -> Clatch (Maybe Cursor) -> (Cell, Cell, Cell, Cell))

type Stripe = Env -> Clatch (Maybe Cursor) -> Cell

type Columns = Seq Column

background :: Clatch a -> Colors -> Radiant
background clatch colors
  | odd i = view oddBackground colors
  | otherwise = view evenBackground colors
  where
    i = view (postFiltset . Serial.forward .to c'Integer'NonNegative) clatch

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
  -> Seq (Clatch a)
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
tableReport
  :: Columns
  -> Colors
  -> History
  -> Seq (Clatch (Maybe Cursor))
  -> Seq (Chunk Text)
tableReport col clrs hist clatches
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
    mkDataRow bkgd clatch = join . fmap mkCell $ col
      where
        mkCell mkr = case mkr of
          W1 f -> Seq.singleton $ f env clatch
          W2 f -> let (c0, c1) = f env clatch in [c0, c1]
          W3 f -> let (c0, c1, c2) = f env clatch in [c0, c1, c2]
          W4 f -> let (c0, c1, c2, c3) = f env clatch in [c0, c1, c2, c3]
          where
            env = Env bkgd hist clrs
    colorSeq = Seq.fromList . take (Seq.length clatches)
      . concat . repeat $ [_evenBackground clrs, _oddBackground clrs]

-- | Creates a columns report for use in the 'Penny.Clatcher.Clatcher'.
table :: Columns -> Report
table cols _ colors hist clatches
  = tableReport cols colors hist clatches

maybeCol
  :: ((Clatch (Maybe Cursor) -> a) -> Stripe)
  -> (Clatch (Maybe Cursor) -> Maybe a)
  -> Stripe
maybeCol getCol getMay = \env clatch -> case getMay clatch of
  Nothing -> textCell _nonLinear (view rowBackground env)
    (view colors env) ""
  Just x -> getCol (const x) env clatch

text
  :: (Clatch (Maybe Cursor) -> Text)
  -> Stripe
text f = \env clatch ->
  textCell _nonLinear (view rowBackground env) (view colors env)
            (f clatch)


seqText
  :: (Clatch (Maybe Cursor) -> Seq Text)
  -> Stripe
seqText f = \env clatch ->
  textCell _nonLinear (view rowBackground env) (view colors env)
            (foldl (<>) mempty . intersperse "•" . f $ clatch)

spaces :: Int -> Stripe
spaces i = text (const (X.replicate i . X.singleton $ ' '))

bool
  :: (Clatch (Maybe Cursor) -> Bool)
  -> Stripe
bool f = \env clatch ->
  let (char, fg)
        | f clatch = ('T', green)
        | otherwise = ('F', red)
  in Cell
      { _rows = Seq.singleton . Seq.singleton
          . back (view rowBackground env) . fore fg
          . chunk . X.singleton $ char
      , _horizontal = top
      , _vertical = left
      , _background = view rowBackground env
      }

integer
  :: (Clatch (Maybe Cursor) -> Integer)
  -> Stripe
integer f = text (X.pack . show . f)

int
  :: (Clatch (Maybe Cursor) -> Int)
  -> Stripe
int f = text (X.pack . show . f)

nonNegative
  :: (Clatch (Maybe Cursor) -> NonNegative)
  -> Stripe
nonNegative f = integer (c'Integer'NonNegative . f)

maybePole
  :: (Clatch (Maybe Cursor) -> Maybe Pole)
  -> Stripe
maybePole f = \env clatch -> sideCell env (f clatch)

-- | Creates two columns: one for the side and one for the magnitude.
repAnyRadix
  :: (Clatch (Maybe Cursor) -> RepAnyRadix)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell)
repAnyRadix f env clatch =
  ( sideCell env (pole'RepAnyRadix (f clatch))
  , qtyRepAnyRadixMagnitudeCell env (f clatch)
  )

-- | Creates two columns: one for the side and one for the magnitude.
qty
  :: (Clatch (Maybe Cursor) -> Decimal)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell)
qty f env clatch =
  ( sideCell env . pole'Decimal . f $ clatch
  , qtyMagnitudeCell env Nothing . f $ clatch
  )

-- | Each 'Troika' creates four columns.  A single posting might give
-- rise to multiple 'Troika'; for example, a balance can have multiple
-- 'Troika'.
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
    side = troimount ^. to pole'Troika
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
        magnitude = case troimount ^. troiload of
          QC q _ -> qtyRepAnyRadixMagnitudeChunk env q
          Q q -> qtyRepAnyRadixMagnitudeChunk env q
          UC rnn _ _ -> brimScalarAnyRadixMagnitudeChunk env side rnn
          US rnn _ -> brimScalarAnyRadixMagnitudeChunk env side rnn
          _ -> qtyRepAnyRadixMagnitudeChunk env
            . repDecimal grouper . c'Decimal'Troiload $ (troimount ^. troiload)

troimountCellsToColumns
  :: Env
  -> Seq TroikaCells
  -> (Cell, Cell, Cell, Cell)
troimountCellsToColumns env = foldl addRow emptyTup
  where
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
troika
  :: (Clatch (Maybe Cursor) -> Troika)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell, Cell, Cell)
troika f env
  = troimountCellsToColumns env
  . Seq.singleton
  . troimountCells env
  . f

-- | Creates same columns as 'troika'.
amount
  :: (Clatch (Maybe Cursor) -> Amount)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell, Cell, Cell)
amount f = troika (c'Troika'Amount . f)

-- | Creates the same columns as for 'Amount', but with one line
-- for each commodity in the balance.

balance
  :: (Clatch (Maybe Cursor) -> Balance)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell, Cell, Cell)
balance f env
  = troimountCellsToColumns env
  . fmap (troimountCells env . makeTroika)
  . Seq.fromList
  . M.assocs
  . view _Wrapped
  . f
  where
    makeTroika (cy, qty) = c'Troika'Amount (Amount cy qty)


-- | Creates two columns, one for the forward serial and one for the
-- backward serial.

serset
  :: (Clatch (Maybe Cursor) -> Serset)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell)
serset f env clatch = (fwdCell, revCell)
  where
    fwdCell = nonNegative (Serial._forward . f) env clatch
    revCell = nonNegative (Serial._backward . f) env clatch

-- | Creates four columns: two for the file serset and two for the
-- global serset.
serpack
  :: (Clatch (Maybe Cursor) -> Serpack)
  -> Env
  -> Clatch (Maybe Cursor)
  -> (Cell, Cell, Cell, Cell)
serpack f env clatch = (fileFwd, fileRev, glblFwd, glblRev)
  where
    (fileFwd, fileRev) = serset (Serial._file . f) env clatch
    (glblFwd, glblRev) = serset (Serial._global . f) env clatch

-- | Creates a single column using whatever 'Show' shows.
columnShow
  :: Show a
  => (Clatch (Maybe Cursor) -> a)
  -> Stripe
columnShow f = text (X.pack . show . f)

-- # Pre-made columns

day :: Column
day = W1 (text $ X.pack . show . view AT.day)

timeOfDay :: Column
timeOfDay = W1 (text $ X.pack . show . view AT.timeOfDay)

timeZone :: Column
timeZone = W1 (text $ X.pack . show . view AT.timeZone)

payee :: Column
payee = W1 (text (view AT.payee))

number :: Column
number = W1 (maybeCol integer (view AP.number))

flag :: Column
flag = W1 (text (view AP.flag))

account :: Column
account = W1 (seqText $ view AP.account)

fitid :: Column
fitid = W1 (text (view AP.fitid))

tags :: Column
tags = W1 (seqText $ view AP.tags)

reconciled :: Column
reconciled = W1 (text $ getTxt . AP.reconciled)
  where
    getTxt r | r = "R"
             | otherwise = ""

cleared :: Column
cleared = W1 (text $ getTxt . AP.cleared)
  where
    getTxt c | c = "C"
             | otherwise = ""

-- | The 'Troika' for this particular posting.
entry :: Column
entry = W4 (troika $ view AP.troika)

-- | All the 'Troika' for the balance for this particular posting.
runner :: Column
runner = W4 (balance $ view AB.balance)

-- | A report with a standard set of 'Column':
--
-- * 'day'
-- * 'number'
-- * 'flag'
-- * 'payee'
-- * 'account'
-- * 'entry'
-- * 'runner'

checkbook :: Columns
checkbook = [ day, number, flag, payee, account, entry, runner ]
