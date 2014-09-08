{-# LANGUAGE BangPatterns #-}
module Penny.Decems where

import Deka.Native.Abstract
import Data.Sequence
import qualified Penny.NonNeg as N

newtype T = T { toSeq :: Seq Decem }
  deriving (Eq, Ord, Show)

toNonNeg :: T -> N.T
toNonNeg = go N.zero N.zero . toSeq
  where
    go !places !acc sq = case viewr sq of
      EmptyR -> acc
      xs :> x -> go (N.succ places) acc' xs
        where
          acc' =
            N.add acc $
            N.mult (N.fromDecem x) (N.exp N.ten places)
