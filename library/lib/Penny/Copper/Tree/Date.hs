module Penny.Copper.Tree.Date where

import Penny.Copper.Tree.Tokens
import Penny.Copper.Tree.Digits
import Data.Sums

newtype DateSep = DateSep (S2 Hyphen Solidus)
  deriving (Eq, Ord, Show)

data Date = Date Digits4 DateSep Digits1or2 DateSep Digits1or2
  deriving (Eq, Ord, Show)

data Time = Time OpenCurly Digits1or2 Colon Digits1or2 Time2
  deriving (Eq, Ord, Show)

data Time2
  = Time2End CloseCurly
  | Time2Space Space Time3
  | Time2Next Time3
  deriving (Eq, Ord, Show)

data Time3
  = Time3Seconds Colon Digits1or2 Time4
  | Time3Zone TimeZone
  deriving (Eq, Ord, Show)

data Time4
  = Time4Zone TimeZone
  | Time4Space Space TimeZone
  | Time4End CloseCurly
  deriving (Eq, Ord, Show)

data TimeZone
  = TimeZone (S2 Hyphen Plus) Digits4 CloseCurly
  deriving (Eq, Ord, Show)
