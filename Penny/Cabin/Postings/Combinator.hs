module Penny.Cabin.Postings.Combinator where

import Data.Map as Map
import qualified Data.Map as M
import qualified Data.Table as T
import Data.Text (Text)
import qualified Data.Text as X

import Penny.Cabin.Postings.Combinator.Allocate (allocate)
import qualified Penny.Cabin.Postings.Base as B

-- | Calculate the widest cell in a column.
widest :: Map T.RowNum B.Queried -> B.ColumnWidth
widest = M.fold f (B.ColumnWidth 0) where
  f queried cw = case queried of
    B.EAllocate _ _ -> cw
    B.EGrowToFit (cw', _) -> max cw' cw

-- | Insert padding characters on the right.
justifyLeft :: Map T.RowNum B.Queried -> Text -> Text
justifyLeft rm = X.justifyLeft n ' ' where
  n = fromIntegral . B.unColumnWidth . widest $ rm

-- | Insert padding characters on the left.
justifyRight :: Map T.RowNum B.Queried -> Text -> Text
justifyRight rm = X.justifyRight n ' ' where
  n = fromIntegral . B.unColumnWidth . widest $ rm


