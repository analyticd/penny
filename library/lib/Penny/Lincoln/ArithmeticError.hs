{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Lincoln.ArithmeticError where

import Control.Exception
import Data.Typeable

-- | A 'Penny.Concrete.T' wraps a 'Deka.Dec.Dec'.  It is possible for
-- arithmetic operations to exceed the available limits of the Deka
-- library; in this case, 'T' is thrown.
newtype T =
  T { toString :: String }
  deriving (Eq, Ord, Show, Typeable)

instance Exception T
