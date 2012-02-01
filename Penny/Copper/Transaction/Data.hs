module Penny.Parser.Transaction.Data where

import Penny.Parser.Posting ( PostingLine )
import Penny.Posting ( Transaction )
import Penny.Groups.AtLeast2 ( AtLeast2 )
import Penny.Bits.Commodity ( Commodity )
import Penny.Reports ( CommodityFmt )

data Data =
  Data { transaction :: Transaction
       , postingLines :: AtLeast2 PostingLine
       , formats :: [(Commodity, CommodityFmt)] }
  deriving Show

