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

module Penny.Cabin.Posts (
  -- * Defaults
  defaultPostsReport

  -- * Options
  , O.T(..)
  , O.ReportWidth(..)
  , O.ymd, O.qtyAsIs, O.balanceAsIs, O.defaultWidth
  , O.columnsVarToWidth, O.defaultOptions, O.widthFromRuntime
  , O.defaultFields
  ) where

import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Copper as C
import qualified Penny.Shield as S

-- | When applied to a DefaultTimeZone and a RadGroup, returns a
-- report with the default options.
defaultPostsReport ::
  C.DefaultTimeZone
  -> C.RadGroup
  -> I.Report
defaultPostsReport dtz rg = undefined

-- | Makes a Posts report with customizable options.
parsePostsReport ::
  (S.Runtime -> O.T)
  -- ^ Function that, when applied to a Runtime, returns the report
  -- options

  -> I.ParseReportOpts
parsePostsReport frt rt =
  let opts = frt rt
  in undefined
