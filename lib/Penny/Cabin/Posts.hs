module Penny.Cabin.Posts where

import qualified Data.Foldable as Fdbl
import Data.List (transpose)
import Data.Maybe (isNothing, catMaybes)
import qualified Data.Sequence as Seq
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.BottomRows as B
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Posts.Colors as PC
import qualified Penny.Liberty.Types as LT

report ::
  (LT.PostingInfo -> Bool) -- ^ Main predicate
  -> ([Info.T] -> [Info.T])
  -> [LT.PostingInfo]
  -> Maybe C.Chunk
report = undefined

makeCells :: Options.T a -> [Info.T] -> C.Chunk
makeCells os is = let
  fmapSnd flds = fmap (fmap snd) flds
  fmapFst flds = fmap (fmap fst) flds
  gFldW = fmapSnd gFlds
  aFldW = fmapSnd aFlds
  gFlds = G.growCells os is
  aFlds = A.payeeAndAcct gFldW os is
  bFlds = B.bottomRows gFldW aFldW os is
  topCells = B.topRowCells (fmapFst gFlds) (fmapFst aFlds)
  withSpacers = B.mergeWithSpacers topCells (Options.spacers os)
  topRows = makeTopRows (Options.baseColors os) withSpacers
  bottomRows = makeBottomRows bFlds
  rws = makeAllRows topRows bottomRows
  in R.chunk rws


topRowsCells ::
  PC.BaseColors
  -> B.TopRowCells (Maybe [R.Cell], Maybe Int)
  -> [[(R.Cell, Maybe R.Cell)]]
topRowsCells bc t = let
  toWithSpc (mayCs, maySp) = case mayCs of
    Nothing -> Nothing
    Just cs -> Just (makeSpacers bc cs maySp)
  f mayPairList acc = case mayPairList of
    Nothing -> acc
    (Just pairList) -> pairList : acc
  in transpose $ Fdbl.foldr f [] (fmap toWithSpc t)

makeRow :: [(R.Cell, Maybe R.Cell)] -> R.Row
makeRow ls = let
  cells = foldr f [] ls where
    f (c, mayC) acc = case mayC of
      Nothing -> c:acc
      Just spcr -> c:spcr:acc
  in Fdbl.foldl' R.appendCell R.emptyRow cells

makeSpacers ::
  PC.BaseColors
  -> [R.Cell]
  -> Maybe Int
  -> [(R.Cell, Maybe R.Cell)]
makeSpacers bc cs mayI = case mayI of
  Nothing -> map (\c -> (c, Nothing)) cs
  Just i -> makeEvenOddSpacers bc cs i

makeEvenOddSpacers ::
  PC.BaseColors
  -> [R.Cell]
  -> Int
  -> [(R.Cell, Maybe R.Cell)]
makeEvenOddSpacers bc cs i = let absI = abs i in
  if absI == 0
  then map (\c -> (c, Nothing)) cs
  else let
    spcrs = cycle [Just $ mkSpcr evenTs, Just $ mkSpcr oddTs]
    mkSpcr ts = R.Cell R.LeftJustify (C.Width absI) ts Seq.empty
    evenTs = PC.evenColors bc
    oddTs = PC.oddColors bc
    in zip cs spcrs

makeTopRows ::
  PC.BaseColors
  -> B.TopRowCells (Maybe [R.Cell], Maybe Int)
  -> Maybe [R.Row]
makeTopRows bc trc =
  if Fdbl.all (isNothing . fst) trc
  then Nothing
  else Just $ map makeRow . topRowsCells bc $ trc


makeBottomRows ::
  B.Fields (Maybe [R.Row])
  -> Maybe [[R.Row]]
makeBottomRows flds =
  if Fdbl.all isNothing flds
  then Nothing
  else Just . transpose . catMaybes . Fdbl.toList $ flds

makeAllRows :: Maybe [R.Row] -> Maybe [[R.Row]] -> R.Rows
makeAllRows mayrs mayrrs = case (mayrs, mayrrs) of
  (Nothing, Nothing) -> R.emptyRows
  (Just rs, Nothing) -> Fdbl.foldl' R.appendRow R.emptyRows rs
  (Nothing, Just rrs) ->
    Fdbl.foldl' R.appendRow R.emptyRows (concat rrs)
  (Just rs, Just rrs) ->
    Fdbl.foldl' addRows  R.emptyRows (zip rs rrs) where 
      addRows rws (tr, brs) = let
        withTopRow = rws `R.appendRow` tr
        in Fdbl.foldl' R.appendRow withTopRow brs

