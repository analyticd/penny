{-# LANGUAGE DeriveDataTypeable #-}
module Penny.Positive.Internal where

import Data.Data (Data)

-- Do not try to make 'Positive' an instance of 'Wrapped' in Lens.
-- That would allow the user to make a 'Positive' with any 'Integer',
-- which would break type safety.

newtype Positive = Positive { c'Integer'Positive :: Integer }
  deriving (Eq, Ord, Show, Data)

