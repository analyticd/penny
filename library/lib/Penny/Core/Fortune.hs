{-# LANGUAGE BangPatterns #-}
-- after Timothy Thomas Fortune

module Penny.Core.Fortune where

import Data.Sequence (Seq, viewl, ViewL(..), (<|))
import qualified Penny.Core.Ent as Ent
import qualified Penny.Core.View as View
import qualified Penny.Core.Bundle as Bundle
import qualified Data.Sequence as Seq
import qualified Penny.Core.Balances as Balances
import qualified Penny.Core.Serial as Serial

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

filterBundles
  :: (Bundle.T -> Bool)
  -> Seq Bundle.T
  -> Seq T
filterBundles p sq
  = makeItems
  . Seq.filter p
  $ sq
  where
    makeItems items = go 0 (Seq.length items - 1) Balances.empty items
      where
        go !l !h bal is = case viewl is of
          EmptyL -> Seq.empty
          x :< xs -> T x (Serial.T l h) bal' <| go (succ l) (pred h) bal' xs
            where
              bal' = Balances.addEntry
                (Ent.commodity . View.current . Bundle.postings $ x)
                (Ent.qty . View.current . Bundle.postings $ x) bal
