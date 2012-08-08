module Penny.Cabin.Posts.Chunk (makeChunk) where

import qualified Data.Foldable as Fdbl
import Data.List (transpose)
import Data.Maybe (isNothing, catMaybes)
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.BottomRows as B
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Colors as PC
import Penny.Cabin.Posts.Meta (Box)


makeChunk ::
  S.Spacers Int
  -> PC.BaseColors
  -> G.GrowOpts
  -> (G.Fields (Maybe Int) -> A.AllocatedOpts)
  -> (G.Fields (Maybe Int)
      -> A.Fields (Maybe Int)
      -> B.BottomOpts)
  -> [Box]
  -> [C.Chunk]
makeChunk ss bc go ao bo bs =
  let fmapSnd = fmap (fmap snd)
      fmapFst = fmap (fmap fst)
      gFldW = fmap (fmap snd) gFlds
      aFldW = fmapSnd aFlds
      gFlds = G.growCells go bs
      aFlds = A.payeeAndAcct (ao gFldW) bs
      bFlds = B.bottomRows (bo gFldW aFldW) bs
      topCells = B.topRowCells (fmapFst gFlds) (fmap (fmap fst) aFlds)
      withSpacers = B.mergeWithSpacers topCells ss
      topRows = makeTopRows bc withSpacers
      bottomRows = makeBottomRows bFlds
  in makeAllRows topRows bottomRows


topRowsCells ::
  PC.BaseColors
  -> B.TopRowCells (Maybe [R.ColumnSpec], Maybe Int)
  -> [[(R.ColumnSpec, Maybe R.ColumnSpec)]]
topRowsCells bc t = let
  toWithSpc (mayCs, maySp) = case mayCs of
    Nothing -> Nothing
    Just cs -> Just (makeSpacers bc cs maySp)
  f mayPairList acc = case mayPairList of
    Nothing -> acc
    (Just pairList) -> pairList : acc
  in transpose $ Fdbl.foldr f [] (fmap toWithSpc t)

makeRow :: [(R.ColumnSpec, Maybe R.ColumnSpec)] -> [C.Chunk]
makeRow = R.row . foldr f [] where
  f (c, mayC) acc = case mayC of
    Nothing -> c:acc
    Just spcr -> c:spcr:acc


makeSpacers ::
  PC.BaseColors
  -> [R.ColumnSpec]
  -> Maybe Int
  -> [(R.ColumnSpec, Maybe R.ColumnSpec)]
makeSpacers bc cs mayI = case mayI of
  Nothing -> map (\c -> (c, Nothing)) cs
  Just i -> makeEvenOddSpacers bc cs i

makeEvenOddSpacers ::
  PC.BaseColors
  -> [R.ColumnSpec]
  -> Int
  -> [(R.ColumnSpec, Maybe R.ColumnSpec)]
makeEvenOddSpacers bc cs i = let absI = abs i in
  if absI == 0
  then map (\c -> (c, Nothing)) cs
  else let
    spcrs = cycle [Just $ mkSpcr evenTs, Just $ mkSpcr oddTs]
    mkSpcr ts = R.ColumnSpec R.LeftJustify (C.Width absI) ts []
    evenTs = PC.evenColors bc
    oddTs = PC.oddColors bc
    in zip cs spcrs

makeTopRows ::
  PC.BaseColors
  -> B.TopRowCells (Maybe [R.ColumnSpec], Maybe Int)
  -> Maybe [[C.Chunk]]
makeTopRows bc trc =
  if Fdbl.all (isNothing . fst) trc
  then Nothing
  else Just $ map makeRow . topRowsCells bc $ trc


makeBottomRows ::
  B.Fields (Maybe [[C.Chunk]])
  -> Maybe [[[C.Chunk]]]
makeBottomRows flds =
  if Fdbl.all isNothing flds
  then Nothing
  else Just . transpose . catMaybes . Fdbl.toList $ flds

makeAllRows :: Maybe [[C.Chunk]] -> Maybe [[[C.Chunk]]] -> [C.Chunk]
makeAllRows mayrs mayrrs = case (mayrs, mayrrs) of
  (Nothing, Nothing) -> []
  (Just rs, Nothing) -> concat rs
  (Nothing, Just rrs) -> concat . concat $ rrs
  (Just rs, Just rrs) -> concat $ zipWith f rs rrs where
    f topRow botRows = concat [topRow, concat botRows]


