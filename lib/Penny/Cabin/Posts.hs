{-# LANGUAGE OverloadedStrings #-}

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

module Penny.Cabin.Posts
  ( postsReport
  , zincReport
  , defaultOptions
  , ZincOpts(..)
  , A.Alloc
  , A.SubAccountLength(..)
  , A.alloc
  , yearMonthDay
  , defaultWidth
  , columnsVarToWidth
  , widthFromRuntime
  , defaultFields
  , defaultSpacerWidth
  , T.ReportWidth(..)
  ) where

import Control.Applicative ((<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import Data.List.Split (chunksOf)
import qualified Data.Either as Ei
import qualified Data.Text as X
import qualified Penny.Cabin.Interface as I
import qualified Penny.Cabin.Options as CO
import qualified Penny.Cabin.Posts.Allocated as A
import qualified Penny.Cabin.Posts.Chunk as C
import qualified Penny.Cabin.Posts.Fields as F
import qualified Penny.Cabin.Posts.Meta as M
import qualified Penny.Cabin.Posts.Parser as P
import qualified Penny.Cabin.Posts.Spacers as S
import qualified Penny.Cabin.Posts.Types as T
import qualified Penny.Cabin.Scheme as E

import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Queries as Q
import qualified Penny.Liberty as Ly
import qualified Penny.Shield as Sh
import qualified Data.Prednote.Expressions as Exp
import qualified Data.Prednote.Pdct as Pe
import qualified System.Console.Rainbow as Rb

import Data.List (intersperse)
import Data.Maybe (catMaybes)
import qualified Data.Foldable as Fdbl
import Data.Time as Time
import qualified System.Console.MultiArg as MA
import System.Locale (defaultTimeLocale)
import Text.Matchers (CaseSensitive)

-- | All information needed to make a Posts report. This function
-- never fails.
postsReport
  :: E.Changers
  -> CO.ShowZeroBalances
  -> (Pe.Pdct (Ly.LibertyMeta, L.Posting))
  -- ^ Removes posts from the report if applying this function to the
  -- post returns False. Posts removed still affect the running
  -- balance.

  -> [Ly.PostFilterFn]
  -- ^ Applies these post-filters to the list of posts that results
  -- from applying the predicate above. Might remove more
  -- postings. Postings removed still affect the running balance.

  -> C.ChunkOpts
  -> [(Ly.LibertyMeta, L.Posting)]
  -> [Rb.Chunk]

postsReport ch szb pdct pff co =
  C.makeChunk ch co
  . M.toBoxList szb pdct pff


zincReport :: ZincOpts -> I.Report
zincReport opts rt = (helpStr opts, md)
  where
    md cs fty ch expr fsf = MA.modeHelp
      "postings"
      (const (helpStr opts))
      (process opts cs fty ch expr fsf)
      (specs rt)
      MA.Intersperse
      (return . Left)

specs
  :: Sh.Runtime
  -> [MA.OptSpec (Either String (P.State -> Ex.Exceptional X.Text P.State))]
specs = map (fmap Right) . P.allSpecs


process
  :: ZincOpts
  -> CaseSensitive
  -> L.Factory
  -> E.Changers
  -> Exp.ExprDesc
  -> ([L.Transaction] -> [(Ly.LibertyMeta, L.Posting)])
  -> [Either String (P.State -> Ex.Exceptional X.Text P.State)]
  -> Ex.Exceptional X.Text I.ArgsAndReport
process os cs fty ch expr fsf ls =
  let (posArgs, clOpts) = Ei.partitionEithers ls
      pState = newParseState cs fty expr os
      exState' = foldl (>>=) (return pState) clOpts
  in fmap (mkPrintReport posArgs os ch fsf) exState'

mkPrintReport
  :: [String]
  -> ZincOpts
  -> E.Changers
  -> ([L.Transaction] -> [(Ly.LibertyMeta, L.Posting)])
  -> P.State
  -> I.ArgsAndReport
mkPrintReport posArgs zo ch fsf st = (posArgs, f)
  where
    f fmt txns _ = do
      pdct <- getPredicate (P.exprDesc st) (P.tokens st)
      let boxes = fsf txns
          rptChks = postsReport ch (P.showZeroBalances st) pdct
                    (P.postFilter st) (chunkOpts fmt st zo) boxes
          expChks = showExpression (P.showExpression st) pdct
          verbChks = showVerboseFilter fmt (P.verboseFilter st)
                                       pdct boxes
          chks = expChks
                 ++ verbChks
                 ++ rptChks
      return chks

indentAmt :: Pe.IndentAmt
indentAmt = 4

blankLine :: Rb.Chunk
blankLine = "\n"

showExpression
  :: P.ShowExpression
  -> Pe.Pdct ((Ly.LibertyMeta, L.Posting))
  -> [Rb.Chunk]
showExpression (P.ShowExpression b) pdct =
  if not b then [] else info : blankLine : (chks ++ [blankLine])
  where
    info = "Postings filter expression:\n"
    chks = Pe.showPdct indentAmt 0 pdct

showVerboseFilter
  :: (L.Amount L.Qty -> X.Text)
  -> P.VerboseFilter
  -> Pe.Pdct (Ly.LibertyMeta, L.Posting)
  -> [(Ly.LibertyMeta, L.Posting)]
  -> [Rb.Chunk]
showVerboseFilter fmt (P.VerboseFilter b) pdct bs =
  if not b then [] else info : blankLine : (chks ++ [blankLine])
  where
    chks =
      fst
      $ Pe.verboseFilter ((L.display fmt) . snd) indentAmt False pdct bs
    info = "Postings report filter:\n"

defaultOptions
  :: Sh.Runtime
  -> ZincOpts
defaultOptions rt = ZincOpts
  { fields = defaultFields
  , width = widthFromRuntime rt
  , showZeroBalances = CO.ShowZeroBalances False
  , dateFormat = yearMonthDay
  , subAccountLength = A.SubAccountLength 2
  , payeeAllocation = A.alloc 60
  , accountAllocation = A.alloc 40
  , spacers = defaultSpacerWidth
  }


type Error = X.Text

getPredicate
  :: Exp.ExprDesc
  -> [Exp.Token ((Ly.LibertyMeta, L.Posting))]
  -> Ex.Exceptional Error (Pe.Pdct ((Ly.LibertyMeta, L.Posting)))
getPredicate d ts =
  case ts of
    [] -> return $ Pe.always
    _ -> Exp.parseExpression d ts


-- | All the information to configure the postings report if the
-- options will be parsed in from the command line.
data ZincOpts = ZincOpts
  { fields :: F.Fields Bool
    -- ^ Default fields to show in the report.

  , width :: T.ReportWidth
    -- ^ Gives the default report width. This can be
    -- overridden on the command line. You can use the
    -- information from the Runtime to make this as wide as
    -- the current terminal.

  , showZeroBalances :: CO.ShowZeroBalances
    -- ^ Are commodities that have no balance shown in the Total fields
    -- of the report?

  , dateFormat :: (M.PostMeta, L.Posting) -> X.Text
    -- ^ How to display dates. This function is applied to the
    -- a PostingInfo so it has lots of information, but it
    -- should return a date for use in the Date field.

  , subAccountLength :: A.SubAccountLength
    -- ^ When shortening the names of sub accounts to make
    -- them fit, they will be this long.

  , payeeAllocation :: A.Alloc
    -- ^ This and accountAllocation determine how much space
    -- payees and accounts receive. They divide up the
    -- remaining space after everything else is displayed. For
    -- instance if payeeAllocation is 60 and accountAllocation
    -- is 40, the payee takes about 60 percent of the
    -- remaining space and the account takes about 40 percent.

  , accountAllocation :: A.Alloc
    -- ^ See payeeAllocation above

  , spacers :: S.Spacers Int
    -- ^ Default width for spacer fields. If any of these Ints are
    -- less than or equal to zero, there will be no spacer. There is
    -- never a spacer for fields that do not appear in the report.

  }

chunkOpts
  :: (L.Amount L.Qty -> X.Text)
  -> P.State
  -> ZincOpts
  -> C.ChunkOpts
chunkOpts fmt s z = C.ChunkOpts
  { C.dateFormat = dateFormat z
  , C.qtyFormat = fmt
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
  -> Exp.ExprDesc
  -> ZincOpts
  -> P.State
newParseState cs fty expr o = P.State
  { P.sensitive = cs
  , P.factory = fty
  , P.tokens = []
  , P.postFilter = []
  , P.fields = fields o
  , P.width = width o
  , P.showZeroBalances = showZeroBalances o
  , P.exprDesc = expr
  , P.verboseFilter = P.VerboseFilter False
  , P.showExpression = P.ShowExpression False
  }

-- | Shows the date of a posting in YYYY-MM-DD format.
yearMonthDay :: (M.PostMeta, L.Posting) -> X.Text
yearMonthDay p = X.pack (Time.formatTime defaultTimeLocale fmt d)
  where
    d = L.day
        . Q.dateTime
        . snd
        $ p
    fmt = "%Y-%m-%d"

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

------------------------------------------------------------
-- ## Help
------------------------------------------------------------

ifDefault :: Bool -> String
ifDefault b = if b then " (default)" else ""

helpStr :: ZincOpts -> String
helpStr o = unlines $
  [ "postings"
  , "  Show postings in order with a running balance."
  , "  Accepts the following options:"
  , ""
  , "Posting filters"
  , "==============="
  , "These options affect which postings are shown in the report."
  , "Postings not shown still affect the running balance."
  , ""
  , "Dates"
  , "-----"
  , ""
  , "--date cmp timespec, -d cmp timespec"
  , "  Date must be within the time frame given. timespec"
  , "  is a day or a day and a time. Valid values for cmp:"
  , "     <, >, <=, >=, ==, /=, !="
  , "--current"
  , "  Same as \"--date <= (right now) \""
  , ""
  , "Serials"
  , "-------"
  , "These options take the form --option cmp num; the given"
  , "sequence number must fall within the given range. \"rev\""
  , "in the option name indicates numbering is from end to beginning."
  , ""
  , "--globalTransaction, --revGlobalTransaction"
  , "  All transactions, after reading the ledger files"
  , "--globalPosting, --revGlobalPosting"
  , "  All postings, after reading the leder files"
  , "--fileTransaction, --revFileTransaction"
  , "  Transactions in each ledger file, after reading the files"
  , "  (numbering restarts with each file)"
  , "--filePosting, --revFilePosting"
  , "  Postings in each ledger file, after reading the files"
  , "  (numbering restarts with each file)"
  , "--filtered, --revFiltered"
  , "  All postings, after filters given in the filter"
  , "  specification portion of the command line are"
  , "  applied"
  , "--sorted, --revSorted"
  , "  All postings remaining after filtering and after"
  , "  postings have been sorted"
  , ""
  , "Pattern matching"
  , "----------------"
  , ""
  , "-a pattern, --account pattern"
  , "  Pattern must match colon-separated account name"
  , "--account-level num pat"
  , "  Pattern must match sub account at given level"
  , "--account-any pat"
  , "  Pattern must match sub account at any level"
  , "-p pattern, --payee pattern"
  , "  Payee must match pattern"
  , "-t pattern, --tag pattern"
  , "  Tag must match pattern"
  , "--number pattern"
  , "  Number must match pattern"
  , "--flag pattern"
  , "  Flag must match pattern"
  , "--commodity pattern"
  , "  Pattern must match colon-separated commodity name"
  , "--posting-memo pattern"
  , "  Posting memo must match pattern"
  , "--transaction-memo pattern"
  , "  Transaction memo must match pattern"
  , ""
  , "Other posting characteristics"
  , "-----------------------------"
  , "--debit"
  , "  Entry must be a debit"
  , "--credit"
  , "  Entry must be a credit"
  , "--qty cmp number"
  , "  Entry quantity must fall within given range"
  , ""
  , "Infix or RPN selection"
  , "----------------------"
  , "--infix - use infix notation"
  , "--rpn - use reverse polish notation"
  , "  (default: use what was used in the filtering options)"
  , ""
  , "Infix Operators - from highest to lowest precedence"
  , "(all are left associative)"
  , "--------------------------"
  , "--open expr --close"
  , "  Force precedence (as in \"open\" and \"close\" parentheses)"
  , "--not expr"
  , "  True if expr is false"
  , "expr1 --and expr2 "
  , "  True if expr and expr2 are both true"
  , "expr1 --or expr2"
  , "  True if either expr1 or expr2 is true"
  , ""
  , "RPN Operators"
  , "-------------"
  , "expr --not"
  , "  True if expr is false"
  , "expr1 expr2 --and"
  , "  True if expr and expr2 are both true"
  , "expr1 expr2 --or"
  , "  True if either expr1 or expr2 is true"
  , ""
  , "Options affecting patterns"
  , "=========================="
  , ""
  , "-i, --case-insensitive"
  , "  Be case insensitive"
  , "-I, --case-sensitive"
  , "  Be case sensitive"
  , ""
  , "--within"
  , "  Use \"within\" matcher"
  , "--pcre"
  , "  Use \"pcre\" matcher"
  , "--posix"
  , "  Use \"posix\" matcher"
  , "--exact"
  , "  Use \"exact\" matcher"
  , ""
  , "Removing postings after sorting and filtering"
  , "============================================="
  , "--head n"
  , "  Keep only the first n postings"
  , "--tail n"
  , "  Keep only the last n postings"
  , ""
  , "Other options"
  , "============="
  , "--width num"
  , "  Hint for roughly how wide the report should be in columns"
  , "  (currently: " ++ (show . T.unReportWidth . width $ o) ++ ")"
  , "--show field, --hide field"
  , "  show or hide this field, where field is one of:"
  , "    globalTransaction, revGlobalTransaction,"
  , "    globalPosting, revGlobalPosting,"
  , "    fileTransaction, revFileTransaction,"
  , "    filePosting, revFilePosting,"
  , "    filtered, revFiltered,"
  , "    sorted, revSorted,"
  , "    visible, revVisible,"
  , "    lineNum,"
  , "    date, flag, number, payee, account,"
  , "    postingDrCr, postingCommodity, postingQty,"
  , "    totalDrCr, totalCommodity, totalQty,"
  , "    tags, memo, filename"
  , "--show-all"
  , "  Show all fields"
  , "--hide-all"
  , "  Hide all fields"
  , ""
  ] ++ showDefaultFields (fields o) ++
  [ ""
  , "--show-zero-balances"
  , "  Show balances that are zero"
    ++ ifDefault (CO.unShowZeroBalances . showZeroBalances $ o)
  , "--hide-zero-balances"
  , "  Hide balances that are zero"
    ++ ifDefault (not . CO.unShowZeroBalances . showZeroBalances $ o)
  , ""
  , "--help, -h"
  , "  Show this help and exit"
  ]

-- | Shows which fields are on by default.
showDefaultFields :: F.Fields Bool -> [String]
showDefaultFields i = hdr : rest
  where
    hdr = "Fields shown by default:"
      ++ if null rest then " (none)" else ""
    rest =
      map ("  " ++)
      . map concat
      . map (intersperse ", ")
      . chunksOf 3
      . catMaybes
      . Fdbl.toList
      . toMaybes
      $ i
    toMaybes flds = f <$> flds <*> F.fieldNames
    f b n = if b then Just n else Nothing
