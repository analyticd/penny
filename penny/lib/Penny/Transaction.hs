{-# LANGUAGE TemplateHaskell #-}
module Penny.Transaction where

import Penny.Ents
import Penny.Fields
import Penny.Tree

import qualified Control.Lens as Lens
import Data.Sequence (Seq)

data Transaction = Transaction
  { _topLine :: (TopLineFields, Seq Tree)
  , _postings :: Balanced (PostingFields, Seq Tree)
  }

Lens.makeLenses ''Transaction
