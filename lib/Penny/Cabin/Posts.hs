module Penny.Cabin.Posts where

import qualified Penny.Cabin.Posts.Growers as G
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.BottomRows as B
import qualified Penny.Cabin.Posts.Options as Options
import qualified Penny.Cabin.Posts.Info as Info
import qualified Penny.Cabin.Row as R
import qualified Penny.Cabin.Colors as C

makeCells :: Options.T a -> [Info.T] -> C.Chunk
makeCells os is = let
  fmapSnd flds = fmap (fmap snd) flds
  gFldW = fmapSnd gFlds
  aFldW = fmapSnd aFlds
  gFlds = G.growCells os is
  aFlds = A.payeeAndAcct gFldW os is
  bFlds = B.bottomRows gFldW aFldW os is
  topCells = B.topRowCells gFlds aFlds
  spcrs = Options.spacers os
  withSpcrs = B.mergeWithSpacers topCells spcrs
  in undefined
