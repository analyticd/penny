module Penny.Cabin.Columns where

import Data.Monoid (Monoid)
import Data.Text (Text)

import Penny.Lincoln.Balance (Balance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)

import Penny.Cabin.Class (Context, Columns)

type Calculator =
  Context
  -> Balance
  -> PostingBox 
  -> [PriceBox]
  -> [Text]

type CalculatorWithWidth =
  Columns
  -> Context
  -> Balance
  -> PostingBox
  -> [PriceBox]
  -> [Text]

data Justify = LJustify | RJustify

data ColSize =
  GrowToFit Calculator
  | Fixed Columns CalculatorWithWidth
  | Variable Double CalculatorWithWidth

data ColSpec =
  ColSpec ColSize Justify

data TableSpec =
  TableSpec [ColSpec]

data Phase1ResultItem =
  P1Done [Text]
  | P1Variable Double (CalculatorWithWidth)

data Phase1ResultStatus =
  P1REmpty
  | P1RWidth Int
  | P1RVariable Double (CalculatorWithWidth)

data Phase1ResultRow =
  Phase1ResultRow [Phase1ResultItem]

data Phase1Table =
  Phase1Table [Phase1ResultRow]


