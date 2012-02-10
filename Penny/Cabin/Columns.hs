module Penny.Cabin.Columns where

import Data.List (maximum)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Word (Word)

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
  P1RWidth Word
  | P1RVariable

instance Monoid Phase1ResultStatus where
  mempty = P1RWidth . fromIntegral $ 0
  mappend x y = case (x, y) of
    (P1RWidth i1, P1RWidth i2) -> P1RWidth (max i1 i2)
    (P1RWidth i, P1RVariable) -> P1RWidth i
    (P1RVariable, P1RWidth i) -> P1RWidth i
    _ -> P1RVariable

status :: Phase1ResultItem -> Phase1ResultStatus
status i = case i of
  P1Done ls -> case ls of
    [] -> P1RWidth $ fromIntegral 0
    ts -> P1RWidth . maximum . map fromIntegral . map X.length $ ts
  P1Variable d c -> P1RVariable

data Phase1ResultRow =
  Phase1ResultRow [Phase1ResultItem]

data Phase1Table =
  Phase1Table [Phase1ResultRow]


