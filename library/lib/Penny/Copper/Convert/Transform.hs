module Penny.Copper.Convert.Transform where

import qualified Penny.Copper.Convert.Collect as O
import qualified Penny.Copper.Convert.TopLine as T
import qualified Penny.TopLine as TL
import qualified Penny.Common as C
import qualified Penny.Copper.Convert.Locate as L
import qualified Penny.Copper.Convert.Fields as F

afterTopLine
  :: O.AfterTopLine
  -> Either T.Error (C.Clxn -> TL.TopLine)
afterTopLine atl = fmap f (T.topLine tl)
  where
    (L.Located loc tl) = O.atlTopLine atl
    f mkTop = \clxn -> mkTop clxn (O.atlTopLineGlobal atl)
      (O.atlTopLineClxn atl) loc (transactionMemo atl)

transactionMemo :: O.AfterTopLine -> C.Memo
transactionMemo =
  F.transactionMemo . fmap (\(L.Located _ m) -> m)
  . O.unTopLineMemos . O.atlMemos
