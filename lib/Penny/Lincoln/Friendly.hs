module Penny.Lincoln.Friendly where

-- | Friendly display of error messages.

class Friendly a where
  friendly :: a -> [String]
  -- ^ Displays an error message in a friendly way.  Each 'String' is
  -- a single line.
