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
  postsReport
  , parseReport
  , makeReport
  , defaultOptions
  , ZincOpts(..)
  , ymd
  , qtyAsIs
  , balanceAsIs
  , defaultWidth
  , columnsVarToWidth
  , widthFromRuntime
  , defaultFields
  , defaultSpacerWidth
  ) where

import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import qualified Penny.Cabin.Chunk as CC
import qualified Penny.Cabin.Colors as PC
import qualified Penny.Cabin.Colors.DarkBackground as Dark
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.Allocate as Alc
import qualified Penny.Cabin.Posts.Chunk as C
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Help as H
import qualified Penny.Cabin.Posts.Meta as M
import Penny.Cabin.Posts.Meta (Box)
import qualified Penny.Cabin.Posts.Parser as P
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Types as T

import qualified Penny.Copper as Cop
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Balance as Bal
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty as Ly
import qualified Penny.Shield as Sh

import Data.Time as Time
import System.Console.MultiArg.Prim (Parser)
import System.Locale (defaultTimeLocale)
import Text.Matchers.Text (CaseSensitive)

-- | All information needed to make a Posts report. This function
-- never fails.
postsReport ::
  CC.Colors
  -- ^ How many colors to show.
  -> CO.ShowZeroBalances
  -> (L.Box Ly.LibertyMeta -> Bool)
  -- ^ Removes posts from the report if applying this function to the
  -- post returns False. Posts removed still affect the running
  -- balance.
  
  -> [Ly.PostFilterFn]
  -- ^ Applies these post-filters to the list of posts that results
  -- from applying the predicate above. Might remove more
  -- postings. Postings removed still affect the running balance.
    
  -> C.ChunkOpts
  -> [L.Box Ly.LibertyMeta]
  -> XL.Text

postsReport col szb pdct pff co =
  CC.chunksToText col
  . C.makeChunk co
  . M.toBoxList szb pdct pff


parseReport ::
  (Sh.Runtime -> ZincOpts)
  -> Parser I.ReportFunc
parseReport frt = do
  getState <- P.parseOptions
  let rf rt cs fty ps _ = do
        let zo = frt rt
            maySt' = getState rt dtz rg st
              where
                dtz = defaultTimeZone zo
                rg = radGroup zo
                st = newParseState cs fty zo
        st' <- Ex.mapException showParserError maySt'
        pdct <- getPredicate . P.tokens $ st'
        return $ postsReport (P.colorPref st')
          (P.showZeroBalances st') pdct
          (P.postFilter st') (chunkOpts st' zo) ps
  return rf
                 
            
makeReport ::
  (Sh.Runtime -> ZincOpts)
  -> I.Report
makeReport frt = I.Report {
  I.help = H.help
  , I.name = "postings"
  , I.parseReport = parseReport frt }


defaultOptions ::
  Cop.DefaultTimeZone
  -> Cop.RadGroup
  -> Sh.Runtime
  -> ZincOpts
defaultOptions dtz rg rt = ZincOpts {
  defaultTimeZone = dtz
  , radGroup = rg
  , fields = defaultFields
  , colorPref = CO.maxCapableColors rt
  , drCrColors = Dark.drCrColors
  , baseColors = Dark.baseColors
  , width = defaultWidth
  , showZeroBalances = CO.ShowZeroBalances True
  , dateFormat = ymd
  , qtyFormat = qtyAsIs
  , balanceFormat = balanceAsIs
  , subAccountLength = A.SubAccountLength 2
  , payeeAllocation = Alc.allocation 60
  , accountAllocation = Alc.allocation 40
  , spacers = defaultSpacerWidth }


showParserError :: P.Error -> X.Text
showParserError = X.pack . show

getPredicate ::
  [Ly.Token (L.Box Ly.LibertyMeta -> Bool)]
  -> Ex.Exceptional X.Text (L.Box Ly.LibertyMeta -> Bool)
getPredicate ts =
  case ts of
    [] -> return $ const True
    ls ->
      Ex.fromMaybe
        (X.pack "posts report: bad posting filter expression")
        (Ly.parseTokenList ls)


-- | All the information to configure the postings report if the
-- options will be parsed in from the command line.
data ZincOpts = ZincOpts {
  defaultTimeZone :: Cop.DefaultTimeZone
  -- ^ The postings report takes options to determine which posts will
  -- be visible. This determines the time zone to use when parsing
  -- these options. Does not affect how the resulting report is
  -- formatted. For that, see the dateFormat field.
  
  , radGroup :: Cop.RadGroup
    -- ^ The postings report takes options to determine which posts
    -- will be visible. This determines the radix point and grouping
    -- character to use when parsing these options. Does not affect
    -- how the resulting report is formatted. For that, see the
    -- qtyFormat field.

  , fields :: F.Fields Bool
    -- ^ Default fields to show in the report.
      
  , colorPref :: CC.Colors
    -- ^ How many colors you want to see, or do it
    -- automatically.

  , drCrColors :: PC.DrCrColors
    -- ^ Colors to use when displaying debits, credits, and
    -- when displaying balance totals

  , baseColors :: PC.BaseColors
    -- ^ Colors to use when displaying everything else

  , width :: T.ReportWidth
    -- ^ Gives the default report width. This can be
    -- overridden on the command line. You can use the
    -- information from the Runtime to make this as wide as
    -- the current terminal.

  , showZeroBalances :: CO.ShowZeroBalances
    -- ^ Are commodities that have no balance shown in the Total fields
    -- of the report?

  , dateFormat :: Box -> X.Text
    -- ^ How to display dates. This function is applied to the
    -- a PostingInfo so it has lots of information, but it
    -- should return a date for use in the Date field.

  , qtyFormat :: Box -> X.Text
    -- ^ How to display the quantity of the posting. This
    -- function is applied to a Box so it has lots of
    -- information, but it should return a formatted string of
    -- the quantity. Allows you to format digit grouping,
    -- radix points, perform rounding, etc.

  , balanceFormat :: L.Commodity -> L.BottomLine -> X.Text
    -- ^ How to display balance totals. Similar to
    -- balanceFormat.

  , subAccountLength :: A.SubAccountLength
    -- ^ When shortening the names of sub accounts to make
    -- them fit, they will be this long.

  , payeeAllocation :: Alc.Allocation
    -- ^ This and accountAllocation determine how much space
    -- payees and accounts receive. They divide up the
    -- remaining space after everything else is displayed. For
    -- instance if payeeAllocation is 60 and accountAllocation
    -- is 40, the payee takes about 60 percent of the
    -- remaining space and the account takes about 40 percent.

  , accountAllocation :: Alc.Allocation
    -- ^ See payeeAllocation above

  , spacers :: S.Spacers Int
    -- ^ Default width for spacer fields. If any of these Ints are
    -- less than or equal to zero, there will be no spacer. There is
    -- never a spacer for fields that do not appear in the report.
      
  }

chunkOpts ::
  P.State 
  -> ZincOpts
  -> C.ChunkOpts
chunkOpts s z = C.ChunkOpts {
  C.baseColors = P.baseColors s
  , C.drCrColors = P.drCrColors s
  , C.dateFormat = dateFormat z
  , C.qtyFormat = qtyFormat z
  , C.balanceFormat = balanceFormat z
  , C.fields = P.fields s
  , C.subAccountLength = subAccountLength z
  , C.payeeAllocation = payeeAllocation z
  , C.accountAllocation = accountAllocation z
  , C.spacers = spacers z
  , C.reportWidth = P.width s
  }


newParseState ::
  CaseSensitive
  -> L.Factory
  -> ZincOpts
  -> P.State
newParseState cs fty o = P.State {
  P.sensitive = cs
  , P.factory = fty
  , P.tokens = []
  , P.postFilter = []
  , P.fields = fields o
  , P.colorPref = colorPref o
  , P.drCrColors = drCrColors o
  , P.baseColors = baseColors o
  , P.width = width o
  , P.showZeroBalances = showZeroBalances o
  }

-- | Shows the date of a posting in YYYY-MM-DD format.
ymd :: Box -> X.Text
ymd p = X.pack (Time.formatTime defaultTimeLocale fmt d) where
  d = Time.localDay
      . L.localTime
      . Q.dateTime
      . L.boxPostFam
      $ p
  fmt = "%Y-%m-%d"

-- | Shows the quantity of a posting. Does no rounding or
-- prettification; simply uses show on the underlying Decimal.
qtyAsIs :: Box -> X.Text
qtyAsIs p = X.pack . show . L.unQty . Q.qty . L.boxPostFam $ p

-- | Shows the quantity of a balance. If there is no quantity, shows
-- two dashes.
balanceAsIs :: a -> L.BottomLine -> X.Text
balanceAsIs _ n = case n of
  L.Zero -> X.pack "--"
  L.NonZero c -> X.pack . show . L.unQty . Bal.qty $ c

-- | The default width for the report.
defaultWidth :: T.ReportWidth
defaultWidth = T.ReportWidth 80

-- | Applied to the value of the COLUMNS environment variable, returns
-- an appropriate ReportWidth.
columnsVarToWidth :: Maybe String -> T.ReportWidth
columnsVarToWidth ms = case ms of
  Nothing -> defaultWidth
  Just str -> case reads str of
    [] -> defaultWidth
    (i, []):[] -> if i > 0 then T.ReportWidth i else defaultWidth
    _ -> defaultWidth

-- | Given the Runtime, use the defaultWidth given above to calculate
-- the report's width if COLUMNS does not yield a value. Otherwise,
-- use what is in COLUMNS.
widthFromRuntime :: Sh.Runtime -> T.ReportWidth
widthFromRuntime rt = case Sh.screenWidth rt of
  Nothing -> defaultWidth
  Just w -> T.ReportWidth . Sh.unScreenWidth $ w

-- | Default fields to show in the Postings report.
defaultFields :: F.Fields Bool
defaultFields =
  F.Fields { F.globalTransaction    = False
           , F.revGlobalTransaction = False
           , F.globalPosting        = False
           , F.revGlobalPosting     = False
           , F.fileTransaction      = False
           , F.revFileTransaction   = False
           , F.filePosting          = False
           , F.revFilePosting       = False
           , F.filtered             = False
           , F.revFiltered          = False
           , F.sorted               = False
           , F.revSorted            = False
           , F.visible              = False
           , F.revVisible           = False
           , F.lineNum              = False
           , F.date                 = True
           , F.flag                 = False
           , F.number               = False
           , F.payee                = True
           , F.account              = True
           , F.postingDrCr          = True
           , F.postingCmdty         = True
           , F.postingQty           = True
           , F.totalDrCr            = True
           , F.totalCmdty           = True
           , F.totalQty             = True
           , F.tags                 = False
           , F.memo                 = False
           , F.filename             = False }

-- | Default width of spacers; most are one character wide, but the
-- spacer after payee is 4 characters wide.
defaultSpacerWidth :: S.Spacers Int
defaultSpacerWidth =
  S.Spacers { S.globalTransaction    = 1
            , S.revGlobalTransaction = 1
            , S.globalPosting        = 1
            , S.revGlobalPosting     = 1
            , S.fileTransaction      = 1
            , S.revFileTransaction   = 1
            , S.filePosting          = 1
            , S.revFilePosting       = 1
            , S.filtered             = 1
            , S.revFiltered          = 1
            , S.sorted               = 1
            , S.revSorted            = 1
            , S.visible              = 1
            , S.revVisible           = 1
            , S.lineNum              = 1
            , S.date                 = 1
            , S.flag                 = 1
            , S.number               = 1
            , S.payee                = 4
            , S.account              = 1
            , S.postingDrCr          = 1
            , S.postingCmdty         = 1
            , S.postingQty           = 1
            , S.totalDrCr            = 1
            , S.totalCmdty           = 1 }
