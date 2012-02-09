module Penny.Cabin.Columns where

import Data.Monoid (Monoid)
import Data.Text (Text)

import Penny.Lincoln.Balance (Balance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)

import Penny.Cabin.Class (Context, Columns)

type Calculator t p m =
  Context
  -> Balance
  -> PostingBox t p 
  -> [PriceBox m]
  -> [Text]

type CalculatorWithWidth t p m =
  Columns
  -> Context
  -> Balance
  -> PostingBox t p
  -> [PriceBox m]
  -> [Text]

data Justify = LJustify | RJustify

data ColSize t p m =
  GrowToFit (Calculator t p m)
  | Fixed Columns (CalculatorWithWidth t p m)
  | Variable Double (CalculatorWithWidth t p m)

data ColSpec t p m =
  ColSpec (ColSize t p m) Justify

data TableSpec t p m =
  TableSpec [ColSpec t p m]

data Phase1ResultItem t p m =
  P1Done [Text]
  | P1Variable Double (CalculatorWithWidth t p m)

data Phase1ResultStatus t p m =
  P1REmpty
  | P1RWidth Int
  | P1RVariable Double (CalculatorWithWidth t p m)

data Phase1ResultRow t p m =
  Phase1ResultRow [Phase1ResultItem t p m]

data Phase1Table t p m =
  Phase1Table [Phase1ResultRow t p m]


