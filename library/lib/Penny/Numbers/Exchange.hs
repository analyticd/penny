module Penny.Numbers.Exchange where

import Penny.Numbers.Babel
import Penny.Numbers.Concrete
import Penny.Numbers.Abstract.Signed
import Deka.Dec (Sign(..))
import Penny.Numbers.Abstract.Grouping
import Penny.Numbers.Abstract.RadGroup

newtype Exch = Exch { unExch :: Concrete }
  deriving (Eq, Ord, Show)

data PluMin = Plus | Minus
  deriving (Eq, Ord, Show)

polarToExch :: Signed r PluMin -> Exch
polarToExch = Exch . concrete . ungroupedToParams pluMinToSign
  . ungroupSigned

exchToSigned :: Radix r -> Exch -> SignedUngrouped r PluMin
exchToSigned rdx = paramsToUngrouped signToPluMin rdx . params . unExch

pluMinToSign :: PluMin -> Sign
pluMinToSign p = case p of
  Plus -> Sign0
  Minus -> Sign1

signToPluMin :: Sign -> PluMin
signToPluMin s = case s of
  Sign0 -> Plus
  Sign1 -> Minus
