{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Penny.Column where

import Control.Lens
import Control.Monad (join)
import qualified Data.Map as M
import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Commodity
import Penny.DateTime
import Penny.Display
import Penny.Natural
import Penny.Popularity
import Penny.Qty
import Penny.Realm
import Penny.Representation
import Penny.Scalar
import Penny.SeqUtil (intersperse)
import Penny.Serial
import Penny.Side
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
  , tableByRows
  )
import Rainbow
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Penny.Troika

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

data Env = Env
  { _clatch :: Clatch
  , _history :: History
  , _colors :: Colors
  }

makeLenses ''Env

newtype Column = Column (Env -> Seq Cell)

makeWrapped ''Column

instance Monoid Column where
  mempty = Column (const mempty)
  mappend (Column cx) (Column cy)
    = Column (\a -> (cx a) <> (cy a))


table
  :: History
  -> Colors
  -> Seq Column
  -> Seq Clatch
  -> (Seq (Chunk Text))
table hist clrs cols cltchs = render . tableByRows $ dataRows
  where
    dataRows = fmap (mkDataRow $) cltchs
    mkDataRow clatch = join . fmap ($ env) . fmap (^. _Wrapped) $ cols
      where
        env = Env clatch hist clrs

background :: Env -> Radiant
background env
  | odd i = env ^. colors.oddBackground
  | otherwise = env ^. colors.evenBackground
  where
    i = env ^. clatch.to postFiltset.forward.to naturalToInteger

class Colable a where
  column :: (Clatch -> a) -> Column

-- | Makes a single cell with a Text.
textCell
  :: (Colors -> Radiant)
  -- ^ Selects the foreground color.
  -> Env
  -- ^ The environment
  -> Text
  -- ^ The text to display
  -> Cell
textCell fg env txt = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back (background env) . fore (env ^. colors.to fg)
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background env
  }

instance Colable Text where
  column f = Column $ \env -> Seq.singleton $
    textCell _nonLinear env (f (env ^. clatch))

spaces :: Int -> Column
spaces i = column (const ((X.replicate i . X.singleton $ ' ')))

spaceCell :: Int -> Env -> Cell
spaceCell i env = textCell _nonLinear env
  (X.replicate i . X.singleton $ ' ')

singleCell
  :: Colable a
  => Env
  -> a
  -> Seq Cell
singleCell env a = ($ env) . (^. _Wrapped) $ column (const a)

instance Colable Bool where
  column f = Column cell
    where
      cell env = Seq.singleton $ Cell
        { _rows = Seq.singleton . Seq.singleton
            . back (background env) . fore fg
            . chunk . X.singleton $ char
        , _horizontal = top
        , _vertical = left
        , _background = background env
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
  column f = Column g
    where
      g env = case f (env ^. clatch) of
        Nothing -> mempty
        Just v -> singleCell env v

sideCell
  :: Env
  -> Maybe Side
  -> Cell
sideCell env maySide = Cell
  { _rows = Seq.singleton . Seq.singleton
      . back (background env) . fore fgColor
      . chunk $ txt
  , _horizontal = top
  , _vertical = left
  , _background = background env
  }
  where
    (fgColor, txt) = case maySide of
      Nothing -> (env ^. colors.neutral, "--")
      Just Debit -> (env ^. colors.debit, "<")
      Just Credit -> (env ^. colors.credit, ">")

sidedChunk
  :: Env
  -> Maybe Side
  -> Text
  -> Chunk Text
sidedChunk env maySide
  = back (background env)
  . fore fgColor
  . chunk
  where
    fgColor = case maySide of
      Nothing -> env ^. colors.neutral
      Just Debit -> env ^. colors.debit
      Just Credit -> env ^. colors.credit

commodityCell
  :: Env
  -> Maybe Side
  -> Orient
  -> Commodity
  -> Cell
commodityCell env maySide orient (Commodity cy) = Cell
  { _rows = Seq.singleton . Seq.singleton
      . sidedChunk env maySide
      $ cy
  , _horizontal = top
  , _vertical = vertOrient
  , _background = background env
  }
  where
    vertOrient
      | orient == CommodityOnLeft = right
      | otherwise = left


instance Colable (Maybe Side) where
  column f = Column $ \env ->
    Seq.singleton (sideCell env (f (_clatch env)))

instance Colable Side where
  column f = Column $ \env ->
    Seq.singleton (sideCell env (Just . f . _clatch $ env))

qtyRepAnyRadixMagnitudeChunk
  :: Env
  -> QtyRepAnyRadix
  -> Chunk Text
qtyRepAnyRadixMagnitudeChunk env qr
  = sidedChunk env (sideOrNeutral qr)
  . X.pack
  . ($ "")
  . display
  . c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  $ qr

qtyRepAnyRadixMagnitudeCell
  :: Env
  -> QtyRepAnyRadix
  -> Cell
qtyRepAnyRadixMagnitudeCell env qr
  = textCell getColor env
  . X.pack
  . ($ "")
  . display
  . c'NilOrBrimScalarAnyRadix'QtyRepAnyRadix
  $ qr
  where
    getColor = case sideOrNeutral qr of
      Nothing -> _neutral
      Just Debit -> _debit
      Just Credit -> _credit

repNonNeutralNoSideMagnitudeChunk
  :: Env
  -> Maybe Side
  -> RepNonNeutralNoSide
  -> Chunk Text
repNonNeutralNoSideMagnitudeChunk env maySide
  = sidedChunk env maySide
  . X.pack
  . ($ "")
  . display
  . c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide

repNonNeutralNoSideMagnitudeCell
  :: Env
  -> Maybe Side
  -> RepNonNeutralNoSide
  -> Cell
repNonNeutralNoSideMagnitudeCell env maySide rnn
  = textCell getColor env
  . X.pack
  . ($ "")
  . display
  . c'NilOrBrimScalarAnyRadix'RepNonNeutralNoSide
  $ rnn
  where
    getColor = case maySide of
      Nothing -> _neutral
      Just Debit -> _debit
      Just Credit -> _credit

qtyMagnitudeCell
  :: Env
  -> Maybe Commodity
  -- ^ If a commodity is supplied, it is used to better determine how
  -- to render the Qty.
  -> Qty
  -> Cell
qtyMagnitudeCell env mayCy
  = qtyRepAnyRadixMagnitudeCell env
  . repQty ei
  where
    ei = either (Left . Just) (Right . Just)
      . selectGrouper
      . Penny.Popularity.groupers (env ^. history)
      $ mayCy

-- | Creates three columns: one for the side, one for the magnitude,
-- and a space in the middle.

instance Colable QtyRepAnyRadix where
  column f = Column getCells
    where
      getCells env = sideCell env maySide
        <| spaceCell 1 env
        <| qtyRepAnyRadixMagnitudeCell env (f . _clatch $ env)
        <| Seq.empty
        where
          maySide = sideOrNeutral (f . _clatch $ env)

-- | Creates three columns: one for the side, one for the magnitude,
-- and a space in the middle.
instance Colable Qty where
  column f = Column getCells
    where
      getCells env = sideCell env (sideOrNeutral qty)
        <| spaceCell 1 env
        <| qtyMagnitudeCell env Nothing qty
        <| Seq.empty
        where
          qty = f . _clatch $ env

instance Colable Commodity where
  column f = column ((^. _Wrapped) . f)

data TroimountCells = TroimountCells
  { _tmSide :: Maybe Side
    -- ^ Always top left aligned, with standard background
  , _tmCyOnLeft :: Maybe (Chunk Text)
  -- ^ Always top right aligned.  If this is Nothing, there is no
  -- following space cell.
  , _tmMagWithCy :: Chunk Text
  -- ^ Always top left aligned.
  , _tmCyOnRight :: Maybe (Chunk Text)
  -- ^ Always top left aligned.  If this is Nothing, there is no
  -- preceding space cell.
  }

makeLenses ''TroimountCells

troimountCells :: Env -> Troimount -> TroimountCells
troimountCells env troimount = TroimountCells side onLeft magWithCy onRight
  where
    cy = troimount ^. Penny.Troika.commodity
    side = troimount ^. troiquant . to sideOrNeutral
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
            UC rnn _ _ -> repNonNeutralNoSideMagnitudeChunk env side rnn
            U rnn _ -> repNonNeutralNoSideMagnitudeChunk env side rnn
            _ -> qtyRepAnyRadixMagnitudeChunk env
              . repQty grouper . toQty $ troiload
          Right qty -> qtyRepAnyRadixMagnitudeChunk env
            . repQty grouper $ qty

troimountCellsToColumns
  :: Env
  -> Seq TroimountCells
  -> Seq Cell
troimountCellsToColumns env
  = tupleToSeq
  . foldl addRow emptyTup
  where
    tupleToSeq (c0, c1, c2, c3, c4, c5, c6) =
      c0 <| c1 <| c2 <| c3 <| c4 <| c5 <| c6 <| Seq.empty

    emptyTup =
      ( Cell Seq.empty top left (background env)              -- side
      , Cell Seq.empty top left (background env)              -- spacer 2
      , Cell Seq.empty top right (background env)             -- cy on left
      , Cell Seq.empty top left (background env)              -- spacer 4
      , Cell Seq.empty top left (background env)              -- magnitude
      , Cell Seq.empty top left (background env)              -- spacer 6
      , Cell Seq.empty top left (background env)              -- cy on right
      )

    addRow (side, spc2, cyOnLeft, spc4, mag, spc6, cyOnRight) tc
      = ( addLine side side'
        , addLine spc2 spc2'
        , addLine cyOnLeft cyOnLeft'
        , addLine spc4 spc4'
        , addLine mag mag'
        , addLine spc6 spc6'
        , addLine cyOnRight cyOnRight'
        )
      where
        addLine old line = old & rows <>~ Seq.singleton line
        side' = Seq.singleton . sidedChunk env (tc ^. tmSide) $
          case tc ^. tmSide of
            Nothing -> "--"
            Just Debit -> "<"
            Just Credit -> ">"
        spc2' = spacer
        cyOnLeft' = maybe Seq.empty Seq.singleton . _tmCyOnLeft $ tc
        spacer = Seq.singleton . back (background env) . chunk $ " "
        spc4' = maybe Seq.empty (const spacer) . _tmCyOnLeft $ tc
        mag' = Seq.singleton . _tmMagWithCy $ tc
        spc6' = maybe Seq.empty (const spacer) . _tmCyOnRight $ tc
        cyOnRight' = maybe Seq.empty Seq.singleton . _tmCyOnRight $ tc


-- | Creates seven columns:
--
-- 1.  Side
-- 2.  Space
-- 3.  Separate commodity on left
-- 4.  Space (empty if 3 is empty)
-- 5.  Magnitude (with commodity on left or right, if applicable)
-- 6.  Space (empty if 7 is empty)
-- 7.  Separate commodity on right

instance Colable Troimount where
  column f = Column getCells where
    getCells env = troimountCellsToColumns env
      . Seq.singleton
      . troimountCells env
      . f
      . Control.Lens.view clatch
      $ env

-- | Creates seven columns:
--
-- 1.  Side
-- 2.  Space
-- 3.  Separate commodity on left
-- 4.  Space (empty if 3 is empty)
-- 5.  Magnitude (with commodity on left or right, if applicable)
-- 6.  Space (empty if 7 is empty)
-- 7.  Separate commodity on right

instance Colable Amount where
  column f = column (c'Troimount'Amount . f)

-- | Creates the same columns as for 'Amount', but with one line
-- for each commodity in the balance.

instance Colable Balance where
  column f = Column getCells where
    getCells env = troimountCellsToColumns env
      . fmap (troimountCells env . makeTroimount)
      . Seq.fromList
      . M.assocs
      . Control.Lens.view _Wrapped
      . f
      . Control.Lens.view clatch
      $ env
      where
        makeTroimount (cy, qty) = Troimount cy (Right qty)

-- | Creates three columns, one for the forward serial and one for the
-- backward serial, with a space in between.

instance Colable Serset where
  column f = Column getCells
    where
      getCells env =
        fwdCell
        <> Seq.singleton (spaceCell 1 env)
        <> revCell
        where
          srst = f . _clatch $ env
          fwdCell = singleCell env (srst ^. forward)
          revCell = singleCell env (srst ^. backward)

-- | Creates seven columns: three for the file serset, three for the
-- global serset, with one column in between.
instance Colable Serpack where
  column f = Column getCells
    where
      getCells env
        = fileCells
        <> Seq.singleton (spaceCell 1 env)
        <> glblCells
        where
          serpack = f . _clatch $ env
          fileCells = singleCell env (serpack ^. file)
          glblCells = singleCell env (serpack ^. global)

-- | Creates one column with a @U@ or an @S@.
instance Colable Realm where
  column f = Column getCells
    where
      getCells env = Seq.singleton $ textCell _nonLinear env txt
        where
          txt = case f . _clatch $ env of
            User -> "U"
            System -> "S"

colableDisplayNonLinear :: Display a => (Clatch -> a) -> Column
colableDisplayNonLinear f = Column getCells
    where
      getCells env = Seq.singleton $ textCell _nonLinear env txt
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
  column f = Column getCells
    where
      getCells env = case f . _clatch $ env of
        Nothing -> Seq.singleton $ textCell _nonLinear env "--"
        Just sc -> Seq.singleton . textCell _nonLinear env
          . X.pack . ($ "") . display $ sc

-- | Shows the scalar.  Does not show the children; if there are
-- children, a ↓ is shown at the end.
instance Colable Tree where
  column f = Column getCells
    where
      getCells env = Seq.singleton $ textCell _nonLinear env txt
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
  column f = Column getCells
    where
      getCells env = Seq.singleton $ textCell _nonLinear env txt
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
