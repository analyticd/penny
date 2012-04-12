-- | An interface for other Penny components to use. A report is
-- anything that is a 'Report'.
module Penny.Cabin.Interface where

import Control.Monad.Exception.Synchronous (Exceptional)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Text.Matchers.Text (CaseSensitive)
import System.Console.MultiArg.Prim (ParserE)

import Penny.Liberty.Types (PostingInfo)
import Penny.Lincoln.Boxes (PriceBox)
import Penny.Liberty.Error (Error)
import Penny.Shield (Runtime)

type ReportFunc =
  [PostingInfo]
  -> [PriceBox]
  -> Exceptional X.Text XL.Text

-- | The parser must parse everything beginning with its command name
-- (parser must fail without consuming any input if the next word is
-- not its command name) up until, but not including, the first
-- non-option word.
type ParseReportOpts =
  Runtime
  -> CaseSensitive
  -> (CaseSensitive -> X.Text -> Exceptional X.Text (X.Text -> Bool))
  -> ParserE Error ReportFunc

data Report =
  Report { help :: X.Text
         , parseReport :: ParseReportOpts }
