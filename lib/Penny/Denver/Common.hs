-- | Types that are shared in common between TopLines and Postings.

module Penny.Denver.Common where

data Cleared = Cleared | NotCleared
             deriving (Eq, Show)
