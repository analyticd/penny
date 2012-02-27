-- | Types used throughout Cabin. Also provides an interface for other
-- Penny components to use.
module Penny.Cabin.Types where

import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Text (Text)
import Text.Matchers.Text (CaseSensitive)
import System.Console.MultiArg.Prim (ParserE)

import Penny.Cabin.Colors (Chunk, ColorPref)
import Penny.Liberty.Types (PostingInfo)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (PriceBox)
import Penny.Liberty.Error (Error)

data ScreenLines = ScreenLines { unLines :: Int }
                 deriving Show

newtype ScreenWidth = ScreenWidth { unScreenWidth :: Int }
                      deriving Show

data Output = IsTTY | NotTTY

newtype Term = Term { unTerm :: String } deriving Show

-- | Information about the runtime environment.
data Runtime =
  Runtime { screenLines :: Maybe ScreenLines
          , screenWidth :: Maybe ScreenWidth
          , currentTime :: DateTime 
          , output :: Output
          , term :: Maybe Term }

type ReportFunc =
  Runtime
  -> [PostingInfo]
  -> [PriceBox]
  -> Exceptional Text Chunk

-- | The parser must parse everything beginning with its command name
-- (parser must fail without consuming any input if the next word is
-- not its command name) up until, but not including, the first
-- non-option word.
type ParseReportOpts =
  CaseSensitive
  -> (Text -> Exceptional Text (Text -> Bool))
  -> ParserE Error (ReportFunc, ColorPref)

data Report =
  Report { help :: Text
         , printReport :: ParseReportOpts }
