module Penny.Tree.Ingot.Comma where

import qualified Penny.Tree.Lewis as Lewis
import qualified Penny.Core.Anna.RadCom as RadCom
import qualified Penny.Tree.Currency as Currency
import qualified Penny.Tree.Apostrophe as Apostrophe

data T
  = T Apostrophe.T
      (Either (Currency.T, Lewis.T RadCom.T)
              (Lewis.T RadCom.T, Maybe Currency.T))
      Apostrophe.T
  deriving (Eq, Ord, Show)
