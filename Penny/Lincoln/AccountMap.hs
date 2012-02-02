module Penny.Lincoln.AccountMap where

import qualified Penny.Lincoln.Bits as B
import qualified Data.NestedMap as N

newtype AccountMap =
  AccountMap (N.NestedMap B.SubAccountName DL.D
