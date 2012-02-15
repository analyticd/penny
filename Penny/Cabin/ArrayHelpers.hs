module Penny.Cabin.ArrayHelpers where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Array (Array, array, range, (!), Ix)
import qualified Data.Array as A
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Word (Word)

--data Table col row e = Table { unTable :: Array (col, row) e }
--                     deriving Show


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

newtype OneDim i e =
  OneDim
  { unOneDim :: Array i e }
  deriving Show

instance Ix i => Functor (OneDim i) where
  fmap f (OneDim a) = OneDim $ fmap f a

instance Ix i => F.Foldable (OneDim i) where
  foldr f z (OneDim a) = foldr f z . A.elems $ a

instance Ix i => T.Traversable (OneDim i) where
  -- :: Applicative f
  -- => OneDim (f a) -> f (OneDim a)
  sequenceA (OneDim a) = let
    mkT ls = OneDim arr where
      arr = A.array (A.bounds a) es
      es = zip (A.range . A.bounds $ a) ls
    in mkT <$> T.sequenceA (A.elems a)

newtype TRows col row e =
  TRows { unTRows :: Array (col, row) e }
  deriving Show

instance (Ix col, Ix row) => Functor (TRows col row) where
  fmap f (TRows a) = TRows $ fmap f a

tRowsElems ::
  (Ix col, Ix row)
  => TRows col row e
  -> [e]
tRowsElems (TRows a) = map (a !) is where
    ((minCol, minRow), (maxCol, maxRow)) = A.bounds a
    is = flip (,)
         <$> A.range (minRow, maxRow)
         <*> A.range (minCol, maxCol)

instance (Ix col, Ix row) => F.Foldable (TRows col row) where
  foldr f z = foldr f z . tRowsElems

instance (Ix col, Ix row) => T.Traversable (TRows col row) where
  -- :: Applicative f
  -- => TRows (f a) -> f (TRows a)
  sequenceA trw@(TRows a) = let
    es = tRowsElems trw
    mkA ls = TRows arr where
      arr = A.array (A.bounds a) els
      els = zip (A.indices a) ls
    in mkA <$> T.sequenceA es

newtype TCols col row e =
  TCols { unTCols :: Array (col, row) e }
  deriving Show

instance (Ix col, Ix row) => Functor (TCols col row) where
  fmap f (TCols a) = TCols $ fmap f a

instance (Ix col, Ix row) => F.Foldable (TCols col row) where
  foldr f z = foldr f z . A.elems . unTCols

instance (Ix col, Ix row) => T.Traversable (TCols col row) where
  sequenceA (TCols a) = let
    mkT ls = TCols $ A.array b es where
      b = A.bounds a
      es = zip (A.indices a) ls
    in mkT <$> T.sequenceA (A.elems a)

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
