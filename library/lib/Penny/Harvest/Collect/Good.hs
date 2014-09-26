module Penny.Harvest.Collect.Good where

import qualified Penny.Harvest.Collect.AfterTopLine as AfterTopLine
import qualified Penny.Harvest.Collect.AfterPosting as AfterPosting
import Data.Sequence (Seq, (|>))

data T
  = NoOutput
  | TopLine AfterTopLine.T
  | Postings AfterPosting.T
  deriving (Eq, Ord, Show)

appendToSeq
  :: Seq (Either AfterTopLine.T AfterPosting.T)
  -> T
  -> Seq (Either AfterTopLine.T AfterPosting.T)
appendToSeq sq g = case g of
  NoOutput -> sq
  TopLine atl -> sq |> (Left atl)
  Postings afp -> sq |> (Right afp)
