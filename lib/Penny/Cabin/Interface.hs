-- | An interface for other Penny components to use. A report is
-- anything that is a 'Report'.
module Penny.Cabin.Interface where

import Control.Monad.Exception.Synchronous (Exceptional)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import Text.Matchers.Text (CaseSensitive)
import System.Console.MultiArg.Prim (Parser)

import qualified Penny.Lincoln as L
import qualified Penny.Liberty as Ly
import Penny.Shield (Runtime)

type ReportFunc =
  Runtime
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
  
  -> [L.Box Ly.LibertyMeta]
  -- ^ Postings that will be included in the report
  
  -> [L.PricePoint]
  -- ^ PricePoints to be included in the report
  
  
  -> Exceptional X.Text XL.Text
  -- ^ The exception type is a strict Text, containing the error
  -- message. The success type is a lazy Text, containing the
  -- resulting report.


data Report =
  Report { help :: X.Text
           -- ^ A strict Text containing a help message.
           
         , name :: String
           -- ^ The name of the report
           
         , parseReport :: Parser ReportFunc
           -- ^ The parser must parse everything beginning with the
           -- first word after the name of the report (the parser does
           -- not parse the name of the report) up until, but not
           -- including, the first non-option word.
         }
