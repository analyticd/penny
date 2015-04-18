{-# LANGUAGE MultiParamTypeClasses #-}
-- | This module is intended to form the basis of a REPL session.
--
-- Please see "Penny.Docs" for full documentation; the Haddocks in
-- this module are intended as a quick reference.
module Penny where

import Data.Monoid
import Data.Sequence (Seq)
import Penny.Amount
import Penny.Clatch
import Penny.Matcher
import Penny.Ledger
import Penny.Representation
import Penny.SeqUtil
import Rainbow
import qualified Data.Foldable as F
import Pipes
import Data.ByteString (ByteString)
import Control.Monad.State

type PreFilter l a = Matcher (TransactionL l, View (Converted (PostingL l))) l a
type Filtereds l = Seq (Filtered (TransactionL l, View (Converted (PostingL l))))
type PostFilter l a
  = Matcher (RunningBalance
    (Sorted (Filtered (TransactionL l, View (Converted (PostingL l)))))) l a

type Reporter l
  = (Amount -> RepNonNeutralNoSide)
  -> Seq (Clatch l)
  -> [Chunk]

data ClatchOptions l ldr pre post rpt res = ClatchOptions
  { converter :: Maybe (Amount -> Maybe Amount)
  , preFilter :: Maybe (PreFilter l pre)
  , sorter :: Maybe (Filtereds l -> l (Filtereds l))
  , postFilter :: Maybe (PostFilter l post)
  , streamPreFilter :: Maybe (Consumer ByteString IO ())
  , streamPostFilter :: Maybe (Consumer ByteString IO ())
  , streamMain :: Maybe (Consumer ByteString IO ())
  , report :: Maybe (rpt, rpt -> Reporter l)
  , runLedger :: Maybe (ldr, ldr -> l res -> IO res)
  }

data Loader
  = LoadFromFile String
  -- ^ Gets transactions from the given filename

type Clatcher l ldr pre post rpt res
  = StateT (ClatchOptions l ldr pre post rpt res)

data ClatchDefaults = ClatchDefaults

clatcher :: ClatchOptions l ldr pre post rpt res -> IO ()
clatcher = undefined
