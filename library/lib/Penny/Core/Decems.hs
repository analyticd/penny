{-# LANGUAGE BangPatterns #-}
module Penny.Core.Decems where

import qualified Penny.Natural.Decem as Decem
import Data.Sequence
import qualified Data.Sequence as S
import qualified Penny.Natural.Unsigned as N
import qualified Data.Foldable as F
import qualified Penny.Tree.Parsec as P
import Data.Monoid
import qualified Penny.Natural.Unsigned as Unsigned

newtype T = T { toSeq :: Seq Decem.T }
  deriving (Eq, Ord, Show)

instance Monoid T where
  mempty = T S.empty
  mappend (T a) (T b) = T $ a <> b

cons :: Decem.T -> T -> T
cons d (T s) = T (d <| s)

snoc :: T -> Decem.T -> T
snoc (T s) d = T (s |> d)

parser :: P.Parser T
parser = fmap T $ P.seq Decem.parser

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

toList :: T -> [Decem.T]
toList = F.toList . toSeq

fromList :: [Decem.T] -> T
fromList = T . S.fromList

empty :: T
empty = T S.empty

numDigits :: T -> Unsigned.T
numDigits = Unsigned.length . toSeq
