-- | An interface for other Penny components to use. A report is
-- anything that is a 'Report'.
module Penny.Cabin.Interface where

import Control.Monad.Exception.Synchronous (Exceptional)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Text.Matchers.Text (CaseSensitive)
import qualified System.Console.MultiArg as MA

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import Penny.Shield (Runtime)

-- | The function that will print the report, and the positional
-- arguments.
type ParseResult = ([String], PrintReport)

type PrintReport
  = [L.Transaction]
  -- ^ All transactions; the report must sort and filter them

  -> [L.PricePoint]
  -- ^ PricePoints to be included in the report


  -> Exceptional X.Text XL.Text
  -- ^ The exception type is a strict Text, containing the error
  -- message. The success type is a lazy Text, containing the
  -- resulting report.


-- | The strict Text containing a help message, and a function to make
-- the mode
type Report = (X.Text, MakeMode)

type MakeMode
  = Runtime
  -- ^ Information only known at runtime, such as the
  -- environment. Does not include any information that is derived
  -- from parsing the command line.

  -> CaseSensitive
  -- ^ Result from previous parses indicating whether the user desires
  -- case sensitivity (this may have been changed in the filtering
  -- options)

  -> (CaseSensitive -> X.Text -> Exceptional X.Text (X.Text -> Bool))
  -- ^ Result from previous parsers indicating the matcher factory the
  -- user wishes to use

  -> ([L.Transaction] -> [L.Box Ly.LibertyMeta])
  -- ^ Result from previous parsers that will sort and filter incoming
  -- transactions

  -> MA.Mode () ParseResult
  -- ^ Strict Text containing a help message, and a Mode that will
  -- print a report.

