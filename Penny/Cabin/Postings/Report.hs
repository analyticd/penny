module Penny.Cabin.Postings.Report where

import Penny.Cabin.Postings.Claimer (claimer)
import Penny.Cabin.Postings.Grower (grower)
import Penny.Cabin.Postings.Allocator (allocator)
import Penny.Cabin.Postings.Finalizer (finalizer)

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Options as O
import qualified Penny.Cabin.Postings.Types as T

import qualified Penny.Lincoln.Boxes as B

report ::
  F.Fields Bool
  -> O.Options
  -> (T.PostingInfo -> Bool)
  -> [B.PostingBox]
  -> Maybe C.Chunk
report flds o =
  G.report (f claimer) (f grower) (f allocator) (f finalizer) where
    f fn = fn flds o
