module Penny.Cabin.Columns where

import Data.Array (Array)
import qualified Data.Array as A
import Data.List (maximum, groupBy)
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.Monoid (Monoid, mempty, mappend)
import Data.Text (Text)
import qualified Data.Text as X
import Data.Word (Word)

import Penny.Lincoln.Balance (Balance)
import Penny.Lincoln.Boxes (PostingBox, PriceBox)

import Penny.Cabin.Class (Context)
import qualified Penny.Cabin.Class as C

type Calculator =
  Context
  -> Balance
  -> PostingBox 
  -> [PriceBox]
  -> [Text]

type CalculatorWithWidth =
  C.Columns
  -> Context
  -> Balance
  -> PostingBox
  -> [PriceBox]
  -> [Text]

data Justify = LJustify | RJustify

data ColSize =
  GrowToFit Calculator
  | Fixed C.Columns CalculatorWithWidth
  | Variable Double CalculatorWithWidth

data ColSpec =
  ColSpec ColSize Justify

data TableSpec =
  TableSpec [ColSpec]

tableA :: [x] -> [y] -> Array (Int, Int) (x, y)
tableA xs ys = A.array indexes ls where
  indexes = ((0,0), (length xs - 1, length ys - 1))
  is = A.range indexes
  vs = [(x, y) | x <- xs, y <- ys]
  ls = zip is vs

data Table x = Table [((Int, Int), x)]

instance Functor Table where
  fmap f (Table ls) = Table $ fmap f' ls where
    f' (p, x) = (p, f x)

table :: (x -> y -> z) -> [x] -> [y] -> Table z
table f xs ys = Table $ zip is vs' where
  is = A.range ((0,0), (length xs - 1, length ys - 1))
  vs = [(x, y) | x <- xs, y <- ys]
  vs' = map (uncurry f) vs

data Columns x = Columns (IntMap (IntMap x))

columns :: Table x -> Columns x
columns = undefined

data Rows x = Rows (IntMap (IntMap x))

rows :: Columns x -> Rows x
rows = undefined
