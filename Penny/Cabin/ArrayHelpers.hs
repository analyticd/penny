module Penny.Cabin.ArrayHelpers where

import Data.Array (Array, array, range, (!), Ix)
import qualified Data.Array as A
import Data.Word (Word)

-- | Given an index in a two-dimensional array, return all the values
-- in that column. This function is partial. It will call error if the
-- index given is not a member of the array.
column ::
  (Ix col, Ix row)
  => Array (col, row) e
  -> col
  -> Array row e
column a c = let
  ((colMin, rowMin), (colMax, rowMax)) = A.bounds a
  cellRange = range ((c, rowMin), (c, rowMax))
  ix' = range (rowMin, rowMax)
  pairs = zip ix' (map (a !) cellRange)
  result = array (rowMin, rowMax) pairs
  valid = A.inRange (A.bounds a) (c, rowMin) 
  in if valid then result else error "column: given column not valid"

row ::
  (Ix col, Ix row)
  => Array (col, row) e
  -> row
  -> Array col e
row a r = let
  ((colMin, rowMin), (colMax, rowMax)) = A.bounds a
  cellRange = range ((colMin, r), (colMax, r))
  ix' = range (colMin, colMax)
  pairs = zip ix' (map (a !) cellRange)
  result = array (colMin, colMax) pairs
  valid = A.inRange (A.bounds a) (colMin, r)
  in if valid then result else error "row: given row not valid"

label ::
  (Ix col, Ix row)
  => Array (col, row) e
  -> Array (col, row) ((col, row), e)
label a = array (A.bounds a) ls where
  ls = fmap f (A.assocs a)
  f (i, e) = (i, (i, e))

-- | Get all rows in an array.
rows ::
  (Ix col, Ix row)
  => Array (col, row) e
  -> Array row (Array col e)
rows a = array ob os where
  ((colMin, rowMin), (colMax, rowMax)) = A.bounds a
  ob = (rowMin, rowMax)
  os = zip (range ob) ias
  ias = map (array (colMin, colMax)) elss
  elss = map toElems indexLists
  indexLists = map f (A.range (rowMin, rowMax)) where
    f r = range ((colMin, r), (colMax, r))
  toElems indexes = zip (map fst indexes) (map (a !) indexes)

-- | Get all columns in an array.
columns ::
  (Ix col, Ix row)
  => Array (col, row) e
  -> Array col (Array row e)
columns a = array ob os where
  ((colMin, rowMin), (colMax, rowMax)) = A.bounds a
  ob = (colMin, colMax)
  os = zip (range ob) ias
  ias = map (array (rowMin, rowMax)) elss
  elss = map toElems indexLists
  indexLists = map f (A.range (colMin, colMax)) where
    f c = range ((c, rowMin), (c, rowMax))
  toElems indexes = zip (map snd indexes) (map (a !) indexes)

--
-- Testing
--
_twoDim :: Array (Char, Int) String
_twoDim = array (('a', 0), ('c', 2)) $ do
  cs <- ['a', 'b', 'c']
  is <- [0, 1, 2]
  return ((cs, is), [cs] ++ show is)
