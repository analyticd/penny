module Penny.Report where

import Data.Sequence (Seq)
import Data.Text (Text)
import Rainbow (Chunk)

import Penny.Clatch.Types
import Penny.Colors
import Penny.Cursor
import Penny.Popularity
import Penny.Price


-- | Type for user-facing reports.
type Report
  = Seq (Price (Maybe Cursor))
  -> Colors
  -> History
  -> Seq (Clatch (Maybe Cursor))
  -> Seq (Chunk Text)
