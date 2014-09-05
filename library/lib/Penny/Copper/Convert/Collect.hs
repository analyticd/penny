module Penny.Copper.Convert.Collect where

import qualified Penny.Copper.Tree.Memo.Transaction as TM
import qualified Penny.Copper.Convert.Locate as L
import qualified Penny.Copper.Tree.TopLine as T

data State
  = Clear
  -- ^ Not currently in posting or transaction; awaiting next input
  | TopLineMemos (Seq (L.Located TM.Memo))
  -- ^ Currently in a TopLineMemo
  | Posting (Seq (L.Located TM.Memo))
            (L.Located T.TopLine)
  -- ^ Just saw a TopLine, waiting for a Posting
  | AfterPosting (Seq (L.Located TM.Memo))
                 (L.Located T.TopLine)
                 (Seq 
