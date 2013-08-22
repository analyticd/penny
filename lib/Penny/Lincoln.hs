-- | Lincoln - the Penny core
--
-- Penny's core types and classes are here. This module re-exports the
-- most useful things. For more details you will want to look at the
-- sub-modules. Also, not all types and functions are re-exported due
-- to naming conflicts. In particular, neither
-- "Penny.Lincoln.Predicates" nor "Penny.Lincoln.Queries" is exported
-- from here due to the blizzard of name conflicts that would result.
module Penny.Lincoln
  ( module Penny.Lincoln.Balance
  , module Penny.Lincoln.Bits
  , module Penny.Lincoln.Builders
  , module Penny.Lincoln.Ents
  , module Penny.Lincoln.Equivalent
  , module Penny.Lincoln.HasText
  , module Penny.Lincoln.Matchers
  , module Penny.Lincoln.PriceDb
  , module Penny.Lincoln.Serial
  , display
  ) where

import Penny.Lincoln.Bits
import Penny.Lincoln.Ents
import Penny.Lincoln.Balance
import Penny.Lincoln.Builders
import Penny.Lincoln.Equivalent
import Penny.Lincoln.HasText
import Penny.Lincoln.Matchers
import Penny.Lincoln.PriceDb
import Penny.Lincoln.Serial

import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Penny.Lincoln.Queries as Q
import qualified Data.Time as Time
import System.Locale (defaultTimeLocale)

--
-- Display
--

-- | Displays a PostFam in a one line format.
--
-- Format:
--
-- File LineNo Date Payee Acct DrCr Cmdty Qty
display
  :: (Amount Qty -> X.Text)
  -- ^ How to format Qty that do not have a QtyRep
  -> Posting
  -> Text
display fmt p = X.pack $ concat (intersperse " " ls)
  where
    ls = [file, lineNo, dt, pye, acct, dc, cmdty, qt]
    file = maybe (labelNo "filename") (X.unpack . unFilename)
           (fmap tFilename . tlFileMeta . fst . unPosting $ p)
    lineNo = maybe (labelNo "line number")
             (show . unPostingLine)
             (Q.postingLine p)
    dateFormat = "%Y-%m-%d %T %z"
    dt = Time.formatTime defaultTimeLocale dateFormat
         . Time.utctDay
         . toUTC
         . Q.dateTime
         $ p
    pye = maybe (labelNo "payee")
            (X.unpack . text) (Q.payee p)
    acct = X.unpack . X.intercalate (X.singleton ':')
           . map unSubAccount . unAccount . Q.account $ p
    dc = case Q.drCr p of
      Debit -> "Dr"
      Credit -> "Cr"
    cmdty = X.unpack . unCommodity . Q.commodity $ p
    getFmt q = fmt $ Amount q (Q.commodity p)
    qt = X.unpack . either showQtyRep getFmt . Q.eiQty $ p
    labelNo s = "(no " ++ s ++ ")"
