-- | For use in a REPL.
--
-- Merely by doing an
--
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
  , Table.Column
  , Table.Columns
  , Table.checkbook
  , Table.table

  -- * Output
  , output
  , colors
  , report

  ) where

import Penny.Account
import Penny.Amount
import Penny.BalanceReport
import Penny.Commodity
import qualified Penny.Clatch.Access.Posting as AP
import qualified Penny.Clatch.Access.TransactionX as AT
import Penny.Clatch.Types
import Penny.Clatcher
import qualified Penny.Clatcher as Clatcher
import Penny.Colorize
import Penny.Colors (Colors)
import qualified Penny.Table as Table
import Penny.Converter
import Penny.Copper (copopen)
import Penny.Cursor
import Penny.Decimal
import qualified Penny.Dump as Dump
import Penny.NonNegative
import Penny.Price
import Penny.Quasi
import qualified Penny.Scheme as Scheme
import Penny.Serial
import Penny.Stream
import Penny.Transaction
import Penny.Troika

import Control.Lens (set, view)
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

-- | A set of reasonable presets:
--
-- * Colorful output is sent to @less@
--
-- * the light color scheme is used
--
-- * the checkbook column report is used
