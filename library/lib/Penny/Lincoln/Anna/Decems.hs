{-# LANGUAGE BangPatterns #-}
module Penny.Lincoln.Anna.Decems where

import Deka.Native.Abstract
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Natural.Unsigned as N
import qualified Data.Foldable as F

newtype T = T { toSeq :: Seq Decem }
  deriving (Eq, Ord, Show)

toUnsigned :: T -> N.T
toUnsigned = go N.zero N.zero . toSeq
  where
    go !places !acc sq = case viewr sq of
      EmptyR -> acc
      xs :> x -> go (N.succ places) acc' xs
        where
          acc' =
            N.add acc $
            N.mult (N.fromDecem x) (N.exp N.ten places)

toList :: T -> [Decem]
toList = F.toList . toSeq

fromList :: [Decem] -> T
fromList = T . S.fromList
