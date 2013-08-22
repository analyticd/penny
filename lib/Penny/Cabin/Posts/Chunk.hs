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
import qualified Penny.Cabin.Scheme as E
import qualified System.Console.Rainbow as Rb
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Lincoln as L
import qualified Data.Text as X
import qualified Penny.Cabin.Posts.Types as Ty

data ChunkOpts = ChunkOpts
  { dateFormat :: (M.PostMeta, L.Posting) -> X.Text
  , qtyFormat :: L.Amount L.Qty -> X.Text
  , fields :: F.Fields Bool
  , subAccountLength :: A.SubAccountLength
  , payeeAllocation :: A.Alloc
  , accountAllocation :: A.Alloc
  , spacers :: S.Spacers Int
  , reportWidth :: Ty.ReportWidth
  }

growOpts :: ChunkOpts -> G.GrowOpts
growOpts c = G.GrowOpts
  { G.dateFormat = dateFormat c
  , G.qtyFormat = qtyFormat c
  , G.fields = fields c
  }

allocatedOpts :: ChunkOpts -> G.Fields (Maybe Int) -> A.AllocatedOpts
allocatedOpts c g = A.AllocatedOpts
  { A.fields = let f = fields c
               in A.Fields { A.payee = F.payee f
                           , A.account = F.account f }
  , A.subAccountLength = subAccountLength c
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
  , B.reportWidth = reportWidth c
  , B.spacers = spacers c
  }

makeChunk
  :: E.Changers
  -> ChunkOpts
  -> [(M.PostMeta, L.Posting)]
  -> [Rb.Chunk]
makeChunk ch c bs =
  let fmapSnd = fmap (fmap snd)
      fmapFst = fmap (fmap fst)
      gFldW = fmap (fmap snd) gFlds
      aFldW = fmapSnd aFlds
      gFlds = G.growCells ch (growOpts c) bs
      aFlds = A.payeeAndAcct ch (allocatedOpts c gFldW) bs
      bFlds = B.bottomRows ch (bottomOpts c gFldW aFldW) bs
      topCells = B.topRowCells (fmapFst gFlds) (fmap (fmap fst) aFlds)
      withSpacers = B.mergeWithSpacers topCells (spacers c)
      topRows = makeTopRows ch withSpacers
      bottomRows = makeBottomRows bFlds
  in makeAllRows topRows bottomRows


topRowsCells
  :: B.TopRowCells (Maybe [R.ColumnSpec], Maybe Int)
  -> [[(R.ColumnSpec, Maybe R.ColumnSpec)]]
topRowsCells t = let
  toWithSpc (mayCs, maySp) = case mayCs of
    Nothing -> Nothing
    Just cs -> Just (makeSpacers cs maySp)
  f mayPairList acc = case mayPairList of
    Nothing -> acc
    (Just pairList) -> pairList : acc
  in transpose $ Fdbl.foldr f [] (fmap toWithSpc t)

makeRow :: E.Changers -> [(R.ColumnSpec, Maybe R.ColumnSpec)] -> [Rb.Chunk]
makeRow ch = R.row ch . foldr f [] where
  f (c, mayC) acc = case mayC of
    Nothing -> c:acc
    Just spcr -> c:spcr:acc


makeSpacers
  :: [R.ColumnSpec]
  -> Maybe Int
  -> [(R.ColumnSpec, Maybe R.ColumnSpec)]
makeSpacers cs mayI = case mayI of
  Nothing -> map (\c -> (c, Nothing)) cs
  Just i -> makeEvenOddSpacers cs i

makeEvenOddSpacers
  :: [R.ColumnSpec]
  -> Int
  -> [(R.ColumnSpec, Maybe R.ColumnSpec)]
makeEvenOddSpacers cs i = let absI = abs i in
  if absI == 0
  then map (\c -> (c, Nothing)) cs
  else let
    spcrs = cycle [Just $ mkSpcr evenTs, Just $ mkSpcr oddTs]
    mkSpcr ts = R.ColumnSpec R.LeftJustify (R.Width absI) ts []
    evenTs = (E.Other, E.Even)
    oddTs = (E.Other, E.Odd)
    in zip cs spcrs

makeTopRows
  :: E.Changers
  -> B.TopRowCells (Maybe [R.ColumnSpec], Maybe Int)
  -> Maybe [[Rb.Chunk]]
makeTopRows ch trc =
  if Fdbl.all (isNothing . fst) trc
  then Nothing
  else Just $ map (makeRow ch) . topRowsCells $ trc


makeBottomRows ::
  B.Fields (Maybe [[Rb.Chunk]])
  -> Maybe [[[Rb.Chunk]]]
makeBottomRows flds =
  if Fdbl.all isNothing flds
  then Nothing
  else Just . transpose . catMaybes . Fdbl.toList $ flds

makeAllRows :: Maybe [[Rb.Chunk]] -> Maybe [[[Rb.Chunk]]] -> [Rb.Chunk]
makeAllRows mayrs mayrrs = case (mayrs, mayrrs) of
  (Nothing, Nothing) -> []
  (Just rs, Nothing) -> concat rs
  (Nothing, Just rrs) -> concat . concat $ rrs
  (Just rs, Just rrs) -> concat $ zipWith f rs rrs where
    f topRow botRows = concat [topRow, concat botRows]


