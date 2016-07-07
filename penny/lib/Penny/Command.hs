{-# LANGUAGE RankNTypes #-}

-- | Assembles everything needed for an interactive session in the
-- REPL using the 'Clatcher'.
--
-- The idea is that you will take different values of 'Clatcher' and
-- combine them using '<>', and then run a computation using 'penny',
-- 'light', or 'dark'.
module Penny.Command where

import Penny.Account
import Penny.Amount
import Penny.BalanceReport
import Penny.Commodity
import qualified Penny.Clatch.Access.Posting as AP
import qualified Penny.Clatch.Access.Transaction as AT
import Penny.Clatch.Types
import Penny.Clatcher (Clatcher, Report)
import qualified Penny.Clatcher as Clatcher
import Penny.Colors (Colors)
import qualified Penny.Columns as Columns
import Penny.Converter
import Penny.Cursor
import Penny.Decimal
import qualified Penny.Dump as Dump
import Penny.NonNegative
import Penny.Price
import qualified Penny.Scheme as Scheme
import Penny.Serial
import Penny.Stream
import Penny.TransactionBare
import Penny.Troika

import Control.Lens (set, Getter, view, to)
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Time as Time

-- # Parsers

-- | Parses an unsigned decimal.  Applies 'error' if a value cannot be parsed.
unsigned :: Text -> DecUnsigned
unsigned = undefined

-- # Commands

-- | Converts one commodity to another, using a particular conversion
-- factor.
convert
  :: Commodity
  -- ^ Convert from this commodity
  -> Commodity
  -- ^ Convert to this commodity
  -> Text
  -- ^ One unit of the from commodity equals this many of the to
  -- commodity.  Enter here a value that can be parsed into a zero or
  -- positive quantity.  If the value does not parse, 'error' will be
  -- applied with a short but slightly helpful error message.
  -> Clatcher
convert fromCy toCy factorTxt = set Clatcher.converter cv mempty
  where
    cv = Converter fn
    fn (Amount oldCy oldQty)
      | oldCy /= fromCy = Nothing
      | otherwise = Just $ Amount toCy (oldQty * factor)
    factor = fmap c'Integer'NonNegative . unsigned $ factorTxt

-- | The 'sieve' performs pre-filtering.  That is, it filters
-- 'Converted' after they have been converted using 'convert', but
-- before they have been sorted.  This can be useful for selecting
-- only the postings you want to be included in the running balance.
-- For instance, if you are interested in the running balance in your
-- checking account, you would pass an appropriate filter for the
-- 'sieve'.
--
-- When combining multiple 'Clatcher' using '<>', every 'sieve' must
-- be 'True' for a 'Converted' to be included in the running balance
-- and report.  Therefore, if you want to combine different 'sieve'
-- predicates using '||', you must do so (using '|||' if you wish) and
-- pass the resulting single predicate to 'sieve'.
sieve
  :: (Converted (Maybe Cursor) () -> Bool)
  -> Clatcher
sieve f = set Clatcher.sieve f mempty

-- | Sorts the 'Prefilt'.  This is done after pre-filtering but before
-- post-filtering.  If combining multiple 'Clatcher' using '<>', an
-- appropriate mulitple-key sort is performed.
sort :: Ord a => (Prefilt (Maybe Cursor) () -> a) -> Clatcher
sort f = set Clatcher.sort (comparing f) mempty

-- | Controls post-filtering.  This filtering is performed after
-- sorting and after the running balance is added.  So for example,
-- you might use 'sieve' to filter for postings that are from your
-- checking account so that the running balance includes all checking
-- postings.  In such a case, you would probably also want to use
-- 'sort' to make sure the postings are in chronological order before
-- the running balance is computed.  Then, you can use 'screen' to
-- only see particular postings you are interested in, such as
-- postings after a certain date or postings to a particular payee.
--
-- As with 'sieve', when combining multiple 'Clatcher' using '<>',
-- every 'screen' must be 'True' for a 'Totaled' to be included in the
-- report.  Therefore, if you want to combine different 'screen'
-- predicates using '||', you must do so (using '|||' if you wish) and
-- pass the resulting single predicate to 'screen'.
screen
  :: (Totaled (Maybe Cursor) () -> Bool)
  -> Clatcher
screen f = set Clatcher.screen f mempty


-- # Output

-- | Determines where your output goes.  When combining multiple
-- 'Clatcher', every 'output' will be used.
output :: Stream -> Clatcher
output s = set Clatcher.output (Seq.singleton s) mempty

-- | Sends output to the @less@ program.
less :: Clatcher
less = output $ stream toLess

-- | Writes output to the given file.  If you use multiple 'saveAs'
-- option, all the named files will be written to.
saveAs
  :: String
  -- ^ Filename to which to send the output.
  -> Clatcher
saveAs = output . stream . toFile

-- | Choose a color scheme.  If you use multiple 'colors' options, the
-- color schemes will be combined using '<>'.
colors :: Colors -> Clatcher
colors c = set Clatcher.colors c mempty

-- | Choose which report to run.  If you use multiple 'report'
-- options, the reports will be shown one after the other.
report :: Report -> Clatcher
report r = set Clatcher.report r mempty

-- # Load

-- | Specify a file from which to load transactions and prices.
open :: String -> Clatcher
open str = set Clatcher.load
  (Seq.singleton (Clatcher.loadCopper str)) mempty

-- | You can preload prices and postings so that you do not have to
-- load them repeatedly; then, specify the preloaded items using
-- 'preload'.
preload
  :: (Seq Price, Seq (TransactionBare (Maybe Cursor)))
  -> Clatcher
preload pair = set Clatcher.load (Seq.singleton (return pair)) mempty

-- # Standard reports

-- | Creates a columns report with the specified 'Columns.Stripe'.
columns :: Columns.Columns -> Clatcher
columns cols = report rpt
  where
    rpt _ colors hist clatches
      = Columns.columnsReport hist colors cols clatches

-- | A report with a standard set of 'Columns.Stripe':
--
-- * 'Columns.day'
-- * 'Columns.number'
-- * 'Columns.flag'
-- * 'Columns.payee'
-- * 'Columns.account'
-- * 'Columns.entry'
-- * 'Columns.runner'
checkbook :: Clatcher
checkbook = columns Columns.checkbook

-- | Prints the @dump@ report, which is a pretty printer showing every
-- aspect of every clatch.
clatchDump :: Clatcher
clatchDump = report rpt
  where
    rpt _ _ _ clatches = Seq.singleton $ Dump.printReport clatches

-- | Prints the @acctree@ report, which shows every account and its balance.
acctree :: Clatcher
acctree = report rpt
  where
    rpt _ colors hist clatches = balanceReport colors hist clatches

-- | Some sensible defaults for a light background terminal:
--
-- * uses 'light'
-- * sends output to @less@
lightDefaults :: Clatcher
lightDefaults = colors Scheme.light <> output (stream toLess)

-- | Some sensible defaults for a dark background terminal:
--
-- * uses 'dark'
-- * sends output to @less@
darkDefaults :: Clatcher
darkDefaults = colors Scheme.dark <> output (stream toLess)

-- # Running

-- | Runs the given 'Clatcher'; does not add any additional settings
-- outside of the ones specified in the 'Clatcher'.  Thus, if your
-- 'Clatcher' does not specify a destination for output, it will
-- appear that nothing happens, because no output will be sent out.
penny :: Clatcher -> IO ()
penny = fmap (const ()) . Clatcher.runClatcher

-- | Runs Penny with the 'lightDefaults' in addition to the specified
-- 'Clatcher'.
light :: Clatcher -> IO ()
light c = penny (lightDefaults <> c)

-- | Runs Penny with the 'darkDefaults' in addition to the specified
-- 'Clatcher'.
dark :: Clatcher -> IO ()
dark c = penny (darkDefaults <> c)

-- # Helpers

-- | A point-free version of '&&'.

(&&&) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
l &&& r = \a -> l a && r a
infixr 3 &&&

-- | A point-free version of '||'.
(|||) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
l ||| r = \a -> l a || r a
infixr 2 |||

-- # Accessing fields

-- ## Transaction fields

zonedTime :: Sliced l a -> Time.ZonedTime
zonedTime = view AT.zonedTime

day :: Sliced l a -> Time.Day
day = view AT.day

timeOfDay :: Sliced l a -> Time.TimeOfDay
timeOfDay = view AT.timeOfDay

timeZone :: Sliced l a -> Time.TimeZone
timeZone = view AT.timeZone

timeZoneMinutes :: Sliced l a -> Int
timeZoneMinutes = view AT.timeZoneMinutes

payee :: Sliced l a -> Maybe Text
payee = view AT.payee

entry :: Sliced l a -> Troika
entry = view AP.troika

birth :: Sliced l a -> Serset
birth = view AP.birth

number :: Sliced l a -> Maybe Integer
number = view AP.number

flag :: Sliced l a -> Maybe Text
flag = view AP.flag

account :: Sliced l a -> Account
account = view AP.account

fitid :: Sliced l a -> Maybe Text
fitid = view AP.fitid

tags :: Sliced l a -> Seq Text
tags = view AP.tags

reconciled :: Sliced l a -> Bool
reconciled = AP.reconciled

cleared :: Sliced l a -> Bool
cleared = AP.cleared
