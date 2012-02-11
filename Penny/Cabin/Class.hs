module Penny.Cabin.Class where

import Control.Monad.Exception.Synchronous (Exceptional)
import Data.ByteString as BS
import Data.Sequence (Seq)
import Data.Text (Text)
import System.Console.MultiArg.Prim (ParserE)

import Penny.Lincoln.Bits (DateTime)
import Penny.Lincoln.Boxes (  PostingBox, PriceBox )

import Text.Matchers.Text (CaseSensitive)

type ReportFunc =
  Context
  -> [PostingBox]
  -> [PriceBox]
  -> Exceptional Text (Seq Chunk)

type ParseReportOpts =
  CaseSensitive
  -> (Text -> Exceptional Text (Text -> Bool))
  -> ParserE Text ReportFunc

data Report =
  Report { help :: Text
         , printReport :: ParseReportOpts }

data OutputDesc = IsTTY | NotTTY
                deriving Show

data Context =
  Context { radix :: Radix
          , separator :: Separator
          , colors :: Maybe Colors
          , outputDesc :: OutputDesc
          , lines :: Maybe Lines
          , columns :: Maybe Columns
          , grouping :: Grouping
          , currentTime :: DateTime }

newtype Radix = Radix { unRadix :: Char }
                deriving Show

newtype Separator = Separator { unSeparator :: Char }
                    deriving Show

data LastGroup = NoMoreGroups | UseLastGrouping
               deriving Show

data Columns = Columns { unColumns :: Int }
               deriving Show

data Lines = Lines { unLines :: Int }
             deriving Show

data Grouping =
  Grouping { groups :: [Int]
           , lastGroup :: LastGroup }
