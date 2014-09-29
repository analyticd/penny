{-# LANGUAGE BangPatterns #-}
module Penny.Core.Decems where

import Deka.Native.Abstract
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Natural.Unsigned as N
import qualified Data.Foldable as F
import qualified Penny.Tree.Parsec as P

newtype T = T { toSeq :: Seq Decem }
  deriving (Eq, Ord, Show)

parser :: P.Parser T
parser = fmap T $ P.seq (P.decem)

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

empty :: T
empty = T S.empty
