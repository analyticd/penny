-- | The Penny Postings report
--
-- The Postings report displays postings in a tabular format designed
-- to be read by humans. Some terminology used in the Postings report:
--
-- [@row@] The smallest unit that spans from left to right. A row,
-- however, might consist of more than one screen line. For example,
-- the running balance is shown on the far right side of the Postings
-- report. The running balance might consist of more than one
-- commodity. Each commodity is displayed on its own screen
-- line. However, all these lines put together are displayed in a
-- single row.
--
-- [@column@] The smallest unit that spans from top to bottom.
--
-- [@tranche@] Each posting is displayed in several rows. The group of
-- rows that is displayed for a single posting is called a tranche.
--
-- [@tranche row@] Each tranche has a particular number of rows
-- (currently four); each of these rows is known as a tranche row.
--
-- [@field@] Corresponds to a particular element of the posting, such
-- as whether it is a debit or credit or its payee. The user can
-- select which fields to see.
--
-- [@allocation@] The width of the Payee and Account fields is
-- variable. Generally their width will adjust to fill the entire
-- width of the screen. The allocations of the Payee and Account
-- fields determine how much of the remaining space each field will
-- receive.
--
-- The Postings report is easily customized from the command line to
-- show various fields. However, the order of the fields is not
-- configurable without editing the source code (sorry).

module Penny.Cabin.Postings where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X

import Penny.Cabin.Postings.Claimer (claimer)
import Penny.Cabin.Postings.Grower (grower)
import Penny.Cabin.Postings.Allocator (allocator)
import Penny.Cabin.Postings.Finalizer (finalizer)

import qualified Penny.Cabin.Colors as C
import qualified Penny.Cabin.Postings.Fields as F
import qualified Penny.Cabin.Postings.Grid as G
import qualified Penny.Cabin.Postings.Options as O
import qualified Penny.Cabin.Postings.Parser as P
import qualified Penny.Cabin.Postings.Types as PT
import qualified Penny.Cabin.Types as CT

import Penny.Liberty.Expressions (evaluate)
import Penny.Liberty.Operators (getPredicate)
import qualified Penny.Liberty.Types as LT

report ::
  F.Fields Bool
  -> O.Options
  -> (LT.PostingInfo -> Bool)
  -> [LT.PostingInfo]
  -> Maybe C.Chunk
report flds o =
  G.report (f claimer) (f grower) (f allocator) (f finalizer) where
    f fn = fn flds o

makeReportFunc ::
  F.Fields Bool
  -> O.Options
  -> P.State
  -> [LT.PostingInfo]
  -> a
  -> Ex.Exceptional X.Text C.Chunk
makeReportFunc f o s ps _ = case getPredicate (P.tokens s) of
  Nothing -> Ex.Exception (X.pack "postings: bad expression")
  Just p -> Ex.Success $ case report f o p ps of
    Nothing -> C.emptyChunk
    Just c -> c

