{-# LANGUAGE OverloadedStrings, OverloadedLists #-}
-- | For use in a REPL.
--
-- Merely by doing an
--
-- > :set -XOverloadedStrings -XOverloadedLists
-- > import Penny.Repl
--
-- the user should be able to do common tasks in the REPL.  This
-- module will do whatever re-exports are necessary to make this
-- happen.
--
-- Also, the Haddocks for this module must be readable, so keep this
-- in mind when re-exporting other functions and modules.

module Penny.Repl
  (
  -- * Clatcher types
    Loader
  , Report

  -- * Quasi quoters
  , qDay
  , qTime
  , qUnsigned

  -- * Loading files
  , copopen
  , load

  -- * Converting commodities
  , convert
  , converter

  -- * Filtering
  , sieve
  , screen

  -- * Sorting
  , sort

  -- * Reports
  -- ** Dump
  , Dump.dump

  -- ** Columns
  , Column
  , Columns
  , checkbook
  , table

  -- ** Acctree
  , acctree

  -- * Output
  , output
  , colors
  , light
  , dark
  , report

  -- * Running the clatcher
  , clatcher

  -- * Accessing fields
  -- ** Transaction fields
  , zonedTime
  , day
  , timeOfDay
  , timeZone
  , timeZoneMinutes
  , payee

  -- ** Posting fields
  , entry
  , birth
  , number
  , flag
  , account
  , fitid
  , tags
  , AP.reconciled
  , AP.cleared
  , AP.side
  , AP.qty
  , AP.magnitude
  , AP.isDebit
  , AP.isCredit
  , AP.isZero

  -- * Comparison helpers
  , cmpUnsigned
  , (&&&)
  , (|||)

  -- * Lens operators
  , (&)
  , (.~)
  , (^.)

  ) where

import Penny.Account
import Penny.Acctree
import Penny.Amount
import Penny.Commodity
import qualified Penny.Clatch.Access.Posting as AP
import qualified Penny.Clatch.Access.TransactionX as AT
import Penny.Clatch.Types
import Penny.Clatcher
import qualified Penny.Clatcher as Clatcher
import Penny.Colorize
import Penny.Colors (Colors)
import Penny.Table (Column, Columns, checkbook, table)
import Penny.Converter
import Penny.Copper (copopen)
import Penny.Cursor
import Penny.Decimal
import qualified Penny.Dump as Dump
import Penny.NonNegative
import Penny.Price
import Penny.Quasi
import Penny.Scheme
import Penny.Serial
import Penny.Stream
import Penny.Transaction
import Penny.Troika

import Control.Lens (view, (&), (.~), (^.))
import Data.Monoid ((<>))
import Data.Ord (comparing)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text)
import qualified Data.Time as Time

-- | Sends output to @less@, using the maximum colors.
less :: (ChooseColors, Stream)
less = (autoColors, streamToStdin toLess)

-- | Writes output to the given file.  If you use multiple 'saveAs'
-- option, all the named files will be written to.
saveAs
  :: String
  -- ^ Filename to which to send the output.
  -> (ChooseColors, Stream)
saveAs fn = (alwaysNoColors, (streamToFile False fn))

-- | Runs the clatcher with the specified settings.
clatcher :: Clatcher -> IO ()
clatcher = fmap (const ()) . Clatcher.runClatcher

-- | A set of reasonable presets:
--
-- * Colorful output is sent to @less@
--
-- * the light color scheme is used
--
-- * the checkbook column report is used
presets :: Clatcher
presets = mempty
  & output .~ [less]
  & colors .~ light
  & report .~ table checkbook

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

payee :: Sliced l a -> Text
payee = view AT.payee

-- ## Posting fields

entry :: Sliced l a -> Troika
entry = view AP.troika

birth :: Sliced l a -> Serset
birth = view AP.birth

number :: Sliced l a -> Maybe Integer
number = view AP.number

flag :: Sliced l a -> Text
flag = view AP.flag

account :: Sliced l a -> Account
account = view AP.account

fitid :: Sliced l a -> Text
fitid = view AP.fitid

tags :: Sliced l a -> Seq Text
tags = view AP.tags
