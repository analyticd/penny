module Builders where

import Test.QuickCheck

-- | Takes a single value, x.  Applies a function to that value,
-- and then applies a second function to the result of the
-- application of the first function.  Passes if the result of the
-- second function equals the original value.

inversion
  :: (Eq a, Show a)
  => (a -> b)
  -- ^ Apply this function to the original value
  -> (b -> a)
  -- ^ Apply this function to the result of the first function
  -> a
  -> Property
inversion f1 f2 a = f2 (f1 a) === a
