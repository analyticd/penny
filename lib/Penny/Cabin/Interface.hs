-- | An interface for other Penny components to use. A report is
-- anything that is a 'Report'.
module Penny.Cabin.Interface where

import Control.Monad.Exception.Synchronous (Exceptional)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Text.Matchers.Text (CaseSensitive)
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Copper as C
import qualified Penny.Lincoln as L
import Penny.Liberty (Error)
import Penny.Shield (Runtime)

type ReportFunc =
  [L.PostingChild C.TopLineMeta C.PostingMeta]
  -> [L.PricePoint C.PriceMeta]
  
  -> Exceptional X.Text XL.Text
  -- ^ The exception type is a strict Text, containing the error
  -- message. The success type is a lazy Text, containing the
  -- resulting report.

type ParserFunc =
  CaseSensitive
  -- ^ Result from previous parses indicating whether the user desires
  -- case sensitivity (this may have been changed in the filtering
  -- options)
  
  -> (CaseSensitive -> X.Text -> Exceptional X.Text (X.Text -> Bool))
  -- ^ Result from previous parsers indicating the matcher factory the
  -- user wishes to use
  
  -> ReportFunc
  -- ^ The resulting function that will be applied to postings and
  -- prices to produce a report.

-- | The parser must parse everything beginning with its command name
-- (parser must fail without consuming any input if the next word is
-- not its command name) up until, but not including, the first
-- non-option word.
type ParseReportOpts =
  Runtime
  -- ^ Information only known at runtime, such as the
  -- environment. Does not include any information that is derived
  -- from parsing the command line.
  -> Parser ParserFunc

data Report =
  Report { help :: X.Text
           -- ^ A strict Text containing a help message.
           
         , parseReport :: ParseReportOpts }
