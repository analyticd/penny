{-# LANGUAGE OverloadedStrings #-}
module Penny.BalanceReport where

import Rainbow
import Rainbox

import Data.Sequence (Seq)
import Data.Text (Text)
import Penny.BalanceMap
import Penny.Colors

formatTable
  :: Colors
  -> BalanceMap
  -> Box Vertical
formatTable clrs = formatTable' 0 "Total"
  where
    formatTable' lvl lbl balMap = undefined

formatTableRow
  :: Int
  -- ^ Indentation level
  -> Text
  -- ^ Label for this level
  -> Radiant
  -- ^ Background color
  -> Seq Cell
formatTableRow = undefined
