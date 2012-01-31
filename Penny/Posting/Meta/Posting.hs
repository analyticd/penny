module Penny.Posting.Meta.Posting where

import qualified Penny.Bits.Commodity as C
import qualified Penny.Reports as R

newtype Line = Line { unLine :: Int }
               deriving Show

newtype Column = Column { unColumn :: Int }
                 deriving Show

data Meta = Meta { column :: Column
                 , line :: Line
                 , format :: Maybe (C.Commodity, R.CommodityFmt) }
            deriving Show
