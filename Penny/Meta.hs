module Penny.Meta where

import Data.Text ( Text )
import qualified Penny.Bits.Commodity as C
import qualified Penny.Reports as R

data Line = Line { unLine :: Int }
            deriving Show

newtype Column = Column { unColumn :: Int }
                 deriving Show

data Filename = Filename { unFilename :: Text }
                deriving Show

data Meta = Meta { column :: Column
                 , line :: Line
                 , format :: Maybe (C.Commodity, R.CommodityFmt) }
            deriving Show
