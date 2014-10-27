{-# LANGUAGE BangPatterns #-}
-- after Timothy Thomas Fortune

module Penny.Core.Fortune where

import qualified Penny.Core.Ent as Ent
import qualified Penny.Core.View as View
import qualified Penny.Core.Bundle as Bundle
import qualified Penny.Core.Balances as Balances
import qualified Penny.Core.Serial as Serial
import qualified Penny.Core.TopLine as TopLine
import qualified Penny.Core.DateTime as DateTime

-- | A 'Penny.Core.Bundle.T', combined with an additional serial
-- indicating the order in which this posting appears after filtering.
-- Also contains balance information.

data T = T
  { bundle :: Bundle.T
  , postMainFilter :: Serial.T
  -- ^ After postings are filtered to determine which postings will be
  -- part of a report, they are assigned this serial.
  , balances :: Balances.T
  } deriving (Eq, Ord, Show)

topLine :: T -> TopLine.T
topLine = Bundle.topLine . bundle

dateTime :: T -> DateTime.T
dateTime = TopLine.dateTime . topLine

-- | Creates a list of 'T' from a list of 'Bundle.T'.  Presumably the
-- 'Bundle.T' have already been filtered.  The list must be finite.
fromBundleList :: [Bundle.T] -> [T]
fromBundleList items = go 0 (length items - 1)
  Balances.empty items
  where
    go !l !h bal is = case is of
      [] -> []
      x : xs -> T x (Serial.T l h) bal'
        : go (succ l) (pred h) bal' xs
        where
          bal' = Balances.addEntry
            (Ent.commodity . View.current . Bundle.postings $ x)
            (Ent.qty . View.current . Bundle.postings $ x) bal

