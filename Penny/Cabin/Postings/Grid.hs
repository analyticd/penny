module Penny.Cabin.Postings.Grid where

import qualified Data.Array as A

import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Boxes as B

fmapArray ::
  A.Ix i
  => (A.Array i y -> i -> y -> z)
  -> A.Array i y
  -> A.Array i z
fmapArray f a = A.array b ls' where
  b = A.bounds a
  ls' = map f' . A.assocs $ a
  f' (i, e) = (i, f a i e)

-- Step 1 - Compute balances
