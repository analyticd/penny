{-# LANGUAGE FlexibleContexts #-}
-- | Takes a grid of data as input; formats a balance report as output.
--
-- There will be three columns in the output.  The first two can be
-- colored for debit or credit.  These two might be, for example, the
-- debit/credit indicator and the quantity.  The third column is not
-- colored.  Typically it will be the sub-account.
--
-- Each single row of the third column can be accompanied by multiple
-- columns of the first two rows.

module Penny.Balgrid where

import Control.Lens ((<|), (&))
import qualified Control.Lens as Lens
import Control.Monad (join)
import qualified Control.Monad.Trans.State as St
import Data.Foldable (toList)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as X
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Rainbow (Chunk)
import qualified Rainbow as RB
import qualified Rainbox as RX

import Penny.Colors
import Penny.Polar

data LeftColsLine = LeftColsLine
  { _leftColsColor :: Maybe Pole
  , _leftCols1 :: Text
  , _leftCols2 :: Text
  } deriving Show

data Tranche = Tranche
  { _leftCols :: Seq LeftColsLine
  , _rightCol :: Text
  } deriving Show

data Balgrid = Balgrid
  { _balgridTop :: Tranche
  , _balgridChildren :: Seq Balgrid
  } deriving Show

-- There are three visible columns in the resulting report.  There are
-- also two spacer columns.

printBalgrid
  :: Colors
  -> Balgrid
  -> Seq (Chunk Text)
printBalgrid colors
  = RX.render
  . RX.tableByRows
  . balgridToCells colors

balgridToCells
  :: Colors
  -> Balgrid
  -> Seq (Seq RX.Cell)
balgridToCells colors balgrid = St.evalState k True
  where
    k = balgridToCellsState colors 0 balgrid

-- | State will be used for the current level, then incremented.
balgridToCellsState
  :: Colors
  -> Int
  -- ^ Level
  -> Balgrid
  -> St.State Bool (Seq (Seq RX.Cell))
balgridToCellsState colors lvl balgrid = do
  st <- St.get
  let thisRow = trancheToCells colors lvl st (_balgridTop balgrid)
  St.put (not st)
  rest <- fmap join
    . mapM (balgridToCellsState colors (succ lvl))
    . _balgridChildren
    $ balgrid
  return $ thisRow <| rest

trancheToCells
  :: Colors
  -> Int
  -- ^ Indentation level
  -> Bool
  -- ^ True if the row is even
  -> Tranche
  -> Seq RX.Cell
trancheToCells colors level isEven tranche
  = left
  <| spacer
  <| center
  <| spacer
  <| right
  <| Seq.empty
  where
    spacer = RX.separator background 1
    background
      | isEven = _evenBackground colors
      | otherwise = _oddBackground colors
    right = rightCell background colors level (_rightCol tranche)
    (left, center) = leftCells background colors (_leftCols tranche)

leftCols
  :: RB.Radiant
  -- ^ Background
  -> Colors
  -> LeftColsLine
  -> (Chunk Text, Chunk Text)
leftCols bg colors (LeftColsLine mayPole col1 col2)
  = (mkChunk col1, mkChunk col2)
  where
    mkChunk txt = RB.chunk txt & RB.fore fg & RB.back bg
      where
        fg = case mayPole of
          Nothing -> _neutral colors
          Just pole
            | pole == Penny.Polar.debit -> _debit colors
            | otherwise -> _credit colors

leftCells
  :: RB.Radiant
  -- ^ Background
  -> Colors
  -> Seq LeftColsLine
  -> (RX.Cell, RX.Cell)
leftCells bg colors sq = (leftCell, centerCell)
  where
    cellsChunks = fmap (leftCols bg colors) sq
    leftCell = RX.Cell
      { RX._rows = fmap Seq.singleton . fmap fst $ cellsChunks
      , RX._horizontal = RX.top
      , RX._vertical = RX.left
      , RX._background = bg
      }
    centerCell = RX.Cell
      { RX._rows = fmap Seq.singleton . fmap snd $ cellsChunks
      , RX._horizontal = RX.top
      , RX._vertical = RX.right
      , RX._background = bg
      }

rightCell
  :: RB.Radiant
  -- ^ Background
  -> Colors
  -> Int
  -- ^ Indentation level
  -> Text
  -- ^ Cell text
  -> RX.Cell
rightCell bg colors indent txt = RX.Cell
  { RX._rows = Seq.singleton . Seq.singleton $ chunk
  , RX._horizontal = RX.top
  , RX._vertical = RX.left
  , RX._background = bg
  }
  where
    chunk = RB.chunk (spaces <> txt) & RB.fore fg & RB.back bg
    indentSpaces = 2
    spaces = X.replicate (indentSpaces * indent) (X.singleton ' ')
    fg = _nonLinear colors
