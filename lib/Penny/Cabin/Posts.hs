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
  
  -- * Custom report
  , customPostsReport

  -- * Options
  , O.T(..)
  , O.ReportWidth(..)
  , O.ymd, O.qtyAsIs, O.balanceAsIs, O.defaultWidth
  , O.columnsVarToWidth, O.defaultOptions, O.widthFromRuntime
  , O.defaultFields
  ) where

import Control.Applicative ((<$>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import qualified Penny.Cabin.Chunk as Chk
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Posts.Options as O
import qualified Penny.Cabin.Posts.Meta as M
import Penny.Cabin.Posts.Chunk (makeChunk)
import Penny.Cabin.Posts.Parser (parseOptions)
import Penny.Cabin.Posts.Help (help)
import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import qualified Penny.Shield as S
import System.Console.MultiArg.Prim (Parser)
import Text.Matchers.Text (CaseSensitive)

-- | When applied to a DefaultTimeZone and a RadGroup, returns a
-- report with the default options.
defaultPostsReport ::
  C.DefaultTimeZone
  -> C.RadGroup
  -> I.Report
defaultPostsReport dtz rg = makeReport f where
  f = parseReportOpts (\rt -> O.defaultOptions dtz rg rt)

-- | Generate a custom Posts report.
customPostsReport ::
  (S.Runtime -> O.T)
  -- ^ Function that, when applied to a Runtime, returns the default
  -- options for the posts report. The options will be overridden by
  -- any options on the command line.

  -> I.Report
customPostsReport = makeReport . parseReportOpts

-- | When passed a ParseReportOpts, makes a Report.
makeReport ::
  I.ParseReportOpts
  -> I.Report
makeReport f =
  I.Report { I.help = help
           , I.name = "posts"
           , I.parseReport = f }

type Factory = CaseSensitive -> X.Text
                 -> Ex.Exceptional X.Text (X.Text -> Bool)

parseReportOpts ::
  (S.Runtime -> O.T)
  -> S.Runtime
  -> Parser (CaseSensitive
             -> Factory
             -> [L.Box Ly.LibertyMeta]
             -> a
             -> Ex.Exceptional X.Text XL.Text)
parseReportOpts frt rt = do
  getOpts <- parseOptions rt
  let optsDefault = frt rt
      f cs fty bs _ = do
        let optsInit = optsDefault { O.sensitive = cs
                                   , O.factory = fty }
        optsParsed <- case getOpts optsInit of
          Ex.Exception e -> Ex.throw . X.pack . show $ e
          Ex.Success g -> return g
        makeReportTxt optsParsed bs
  return f
            

-- | Using the options parsed from the command line, print out the
-- report.
makeReportTxt ::
  O.T
  -> [L.Box Ly.LibertyMeta]
  -> Ex.Exceptional X.Text XL.Text
makeReportTxt op bs = fmap mkText postMetaBoxes
  where
    e = X.pack "posts report: filter expression parse failure"
    postMetaBoxes = Ex.fromMaybe e (filterAndAssignMeta op bs)
    mkText = Chk.bitsToText (O.colorPref op) . makeChunk op
    

-- | Takes a list of Box with LibertyMeta and options as processed
-- from the command line.  Filters the list of Box and processes the
-- post filter, then assigns the post metadata. Fails if the
-- expression fails to produce a result.
filterAndAssignMeta ::
  O.T
  -> [L.Box Ly.LibertyMeta]
  -> Maybe [L.Box M.PostMeta]
filterAndAssignMeta op bs =
  M.addMetadata (O.showZeroBalances op)
  <$> M.filterBoxes (O.tokens op) (O.postFilter op) bs


