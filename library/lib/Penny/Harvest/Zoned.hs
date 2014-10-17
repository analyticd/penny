{-# LANGUAGE BangPatterns #-}
module Penny.Harvest.Zoned where

import qualified Penny.Core.Location as Location
import qualified Penny.Harvest.Zoned.Located as Located
import Data.Sequence (Seq, ViewL(..), (<|))
import qualified Data.Sequence as S
import qualified Penny.Tree.Package as Tree.Package
import qualified Penny.Tree.File as File
import qualified Penny.Tree.Line as Line
import qualified Penny.Harvest.Zoned.Item as Item

newtype T = T
  { items :: Seq (Located.T Item.T)
  } deriving (Eq, Ord, Show)

harvest :: Tree.Package.T -> T
harvest (Tree.Package.T _ (File.T lns _)) = T locs
  where
    locs = go 0 lns
    go !lineNum sq = case S.viewl sq of
      EmptyL -> S.empty
      x :< xs -> case x of
        Line.T3 memoT -> loc (Item.T0 memoT) <| go (succ lineNum) xs
        Line.T4 topLine -> loc (Item.T1 topLine) <| go (succ lineNum) xs
        Line.T5 pstg -> loc (Item.T2 pstg) <| go (succ lineNum) xs
        Line.T6 memoP -> loc (Item.T3 memoP) <| go (succ lineNum) xs
        _ -> go (succ lineNum) xs
        where
          loc = Located.T (Location.T lineNum)

