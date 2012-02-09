module Penny.Cabin.Columns where

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

data ColSpec t p m =
  GrowToFit (Calculator t p m)
  | Fixed Int (Calculator t p m)
  | Variable Double (CalculatorWithWidth t p m)

