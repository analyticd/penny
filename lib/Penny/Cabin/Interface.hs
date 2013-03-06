-- | An interface for other Penny components to use. A report is
-- anything that is a 'Report'.
module Penny.Cabin.Interface where

import qualified Penny.Steel.Expressions as Exp
import qualified Penny.Cabin.Scheme as S
import Control.Monad.Exception.Synchronous (Exceptional)
import qualified Data.Text as X
import Text.Matchers (CaseSensitive)
import qualified Text.Matchers as TM
import qualified System.Console.MultiArg as MA

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import Penny.Shield (Runtime)

-- | The function that will print the report, and the positional
-- arguments. If there was a problem parsing the command line options,
-- return an Exception with an error message.

-- | Parsing the filter options can have one of two results: a help
-- string, or a list of positional arguments and a function that
-- prints a report. Or, the parse might fail.

type PosArg = String
type HelpStr = String
type ArgsAndReport = ([PosArg], PrintReport)
type ParseResult = Exceptional String ArgsAndReport

type PrintReport
  = [L.Transaction]
  -- ^ All transactions; the report must sort and filter them

  -> [L.PricePoint]
  -- ^ PricePoints to be included in the report


  -> Exceptional X.Text [S.PreChunk]
  -- ^ The exception type is a strict Text, containing the error
  -- message. The success type is a list of PreChunks containing the
  -- resulting report. This allows for errors after the list of
  -- transactions has been seen.


type Report = Runtime -> (HelpStr, MkReport)
type MkReport
  = CaseSensitive
  -- ^ Result from previous parses indicating whether the user desires
  -- case sensitivity (this may have been changed in the filtering
  -- options)

  -> (CaseSensitive -> X.Text -> Exceptional X.Text TM.Matcher)
  -- ^ Result from previous parsers indicating the matcher factory the
  -- user wishes to use

  -> Exp.ExprDesc
  -- ^ Result from previous parsers indicating whether the user wants
  -- RPN or infix

  -> ([L.Transaction] -> [L.Box Ly.LibertyMeta])
  -- ^ Result from previous parsers that will sort and filter incoming
  -- transactions

  -> MA.Mode ParseResult
