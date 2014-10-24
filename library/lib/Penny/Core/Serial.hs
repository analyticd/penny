{-# LANGUAGE BangPatterns #-}
module Penny.Core.Serial where

import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

data T = T
  { forward :: Int
  , backward :: Int
  } deriving (Eq, Ord, Show)

-- | Creates a particular number of serials.  Returns an empty Seq if
-- the argument is less than or equal to zero.
serials :: Int -> Seq T
serials = go 0
  where
    go !l !h
      | h <= 0 = Seq.empty
      | otherwise = T l (h - 1) <| go (succ l) (pred h)

