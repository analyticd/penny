{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Penny.Clatch.Shortcut where

import Control.Lens
import Control.Monad
import Penny.Matcher
import Penny.Ledger (Ledger, TreeL, PostingL)
import qualified Penny.Ledger as L
import Penny.Clatch
import Data.Sequence (Seq)
import Penny.Ledger.Matcher
import Penny.SeqUtil


payee
  :: (Ledger m, MonadPlus m)
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> m (Seq (TreeL m))
payee = undefined
