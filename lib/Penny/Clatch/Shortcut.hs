{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Penny.Clatch.Shortcut where

import Control.Lens
import Control.Monad
import Penny.Matcher
import Penny.Ledger (Ledger, TreeL, PostingL)
import qualified Penny.Ledger
import Penny.Ledger.Matcher
import qualified Penny.Ledger as L
import Penny.Clatch
import Data.Sequence (Seq)
import Penny.Ledger.Matcher
import Penny.SeqUtil
import Penny.Transbox
import Penny.Viewpost
import qualified Data.Traversable as Tr
import Data.Text (Text)

standard
  :: (Ledger m, MonadPlus m)
  => Text
  -> (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> m (Seq (TreeL m))
standard nm getter
  = namedTree nm
  <=< return . join
  <=< Tr.mapM Penny.Ledger.pstgMeta
  . getter
  . view (transboxee.viewpost)

payee
  :: (Ledger m, MonadPlus m)
  => (View (PostingL m) -> Seq (PostingL m))
  -> Clatch m
  -> m (Seq (TreeL m))
payee getter clch = standard "payee" getter clch `mplus` shortcut
  where
    shortcut = undefined
