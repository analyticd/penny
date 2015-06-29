{-# LANGUAGE TemplateHaskell #-}
module Penny.Clatch where

import Control.Lens
import Penny.Amount
import Penny.Tree
import Penny.Trio
import Penny.Qty
import Penny.Commodity
import Penny.Serial
import Data.Sequence (Seq)

data Posting = Posting
  { _pstgTrees :: Seq Tree
  , _trio :: Trio
  , _amount :: Amount
  , _pstgSerpack :: Serpack
  , _index :: Serset
  } deriving (Eq, Ord, Show)

makeLenses ''Posting
