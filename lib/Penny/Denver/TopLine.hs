module Penny.Denver.TopLine where

import qualified Data.Time as T
import qualified Penny.Denver.Common as C
import qualified Penny.Lincoln.Bits as B

data TopLine = TopLine {
  day :: T.Day
  , cleared :: C.Cleared
  , number :: Maybe B.Number
  , payee :: Maybe B.Payee
  } deriving (Eq, Show)
