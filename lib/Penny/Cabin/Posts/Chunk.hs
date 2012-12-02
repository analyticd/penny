module Penny.Cabin.Posts.Chunk (ChunkOpts(..), makeChunk) where

import qualified Data.Foldable as Fdbl
import Data.List (transpose)
import Data.Maybe (isNothing, catMaybes)
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.BottomRows as B
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Chunk as C
import qualified Penny.Cabin.Colors as PC
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Penny.Cabin.Posts.Types as Ty

data ChunkOpts = ChunkOpts {
  baseColors :: PC.BaseColors
  , drCrColors :: PC.DrCrColors
  , dateFormat :: Box -> X.Text
  , qtyFormat :: Box -> X.Text
  , balanceFormat :: L.Commodity -> L.BottomLine -> X.Text
  , fields :: F.Fields Bool
  , subAccountLength :: A.SubAccountLength
  , payeeAllocation :: A.Alloc
  , accountAllocation :: A.Alloc
  , spacers :: S.Spacers Int
  , reportWidth :: Ty.ReportWidth
  }

growOpts :: ChunkOpts -> G.GrowOpts
growOpts c = G.GrowOpts {
  G.baseColors = baseColors c
  , G.drCrColors = drCrColors c
  , G.dateFormat = dateFormat c
  , G.qtyFormat = qtyFormat c
  , G.balanceFormat = balanceFormat c
  , G.fields = fields c
  }

allocatedOpts :: ChunkOpts -> G.Fields (Maybe Int) -> A.AllocatedOpts
allocatedOpts c g = A.AllocatedOpts {
  A.fields = let f = fields c
             in A.Fields { A.payee = F.payee f
                         , A.account = F.account f }
  , A.subAccountLength = subAccountLength c
  , A.baseColors = baseColors c
  , A.allocations = A.Fields { A.payee = payeeAllocation c
                             , A.account = accountAllocation c }
  , A.spacers = spacers c
  , A.growerWidths = g
  , A.reportWidth = reportWidth c
  }

bottomOpts ::
  ChunkOpts
  -> G.Fields (Maybe Int)
  -> A.Fields (Maybe Int)
  -> B.BottomOpts
bottomOpts c g a = B.BottomOpts {
  B.growingWidths = g
  , B.allocatedWidths = a
  , B.fields = fields c
  , B.baseColors = baseColors c
  , B.reportWidth = reportWidth c
  , B.spacers = spacers c
  }

makeChunk ::
  ChunkOpts
  -> [Box]
  -> [C.Chunk]
makeChunk c bs =
  let fmapSnd = fmap (fmap snd)
      fmapFst = fmap (fmap fst)
      gFldW = fmap (fmap snd) gFlds
      aFldW = fmapSnd aFlds
      gFlds = G.growCells (growOpts c) bs
      aFlds = A.payeeAndAcct (allocatedOpts c gFldW) bs
      bFlds = B.bottomRows (bottomOpts c gFldW aFldW) bs
      topCells = B.topRowCells (fmapFst gFlds) (fmap (fmap fst) aFlds)
      withSpacers = B.mergeWithSpacers topCells (spacers c)
      topRows = makeTopRows (baseColors c) withSpacers
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


