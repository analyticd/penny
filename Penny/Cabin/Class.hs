module Penny.Cabin.Class where

import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Text (Text)
import System.Console.MultiArg.Prim (ParserE)

import Penny.Cabin.Colors (Chunk, ColorPref, Width)
import Penny.Liberty.Error (Error)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (  PostingBox, PriceBox )

import Text.Matchers.Text (CaseSensitive)

type ReportFunc =
  Context
  -> [PostingBox]
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

data Context =
  Context { screenLines :: Maybe ScreenLines
          , screenWidth :: Maybe ScreenWidth
          , currentTime :: DateTime }

data ScreenLines = ScreenLines { unLines :: Int }
                 deriving Show

newtype ScreenWidth = ScreenWidth { unScreenWidth :: Int }
                      deriving Show
