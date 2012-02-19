module Penny.Cabin.Postings.Types where

import qualified Data.Array as A

newtype PostingNum = PostingNum { unPostingNum :: Int }
                     deriving (Show, Eq, Ord)

newtype RevPostingNum =
  RevPostingNum { unRevPostingNum :: Int }
  deriving (Show, Eq, Ord)

newtype VisibleNum = VisibleNum { unVisibleNum :: Int }
                     deriving (Show, Eq, Ord)

newtype TrancheRow = TrancheRow { unTrancheRow :: Int }
                     deriving (Show, Eq, Ord)

