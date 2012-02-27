module Penny.Cabin.Class where

import Control.Monad.Exception.Synchronous (Exceptional)
import Data.Text (Text)
import System.Console.MultiArg.Prim (ParserE)

import Penny.Cabin.Colors (Chunk, ColorPref, Width)
import Penny.Liberty.Error (Error)
import Penny.Liberty.Types (PostingInfo)
import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (  PostingBox, PriceBox )

import Text.Matchers.Text (CaseSensitive)

type ReportFunc =
  Context
  -> [PostingInfo]
  -> [PriceBox]
  -> Exceptional Text Chunk

