module Penny.Family where

import qualified Penny.Family.Family as F
import qualified Penny.Family.Child as C
import qualified Penny.Groups.AtLeast2 as A2

children :: F.Family p c -> A2.AtLeast2 (C.Child p c)
children (F.Family p c1 c2 cRest) = A2.AtLeast2 fc sc rc where
  fc = C.Child c1 c2 cRest p
  sc = C.Child c2 c1 cRest p
  rc = map toChild rest
  rest = A2.others cRest
  toChild (c, cs) = C.Child c c1 (c2:cs) p

orphans :: F.Family p c -> A2.AtLeast2 c
orphans (F.Family _ c1 c2 cs) = A2.AtLeast2 c1 c2 cs
