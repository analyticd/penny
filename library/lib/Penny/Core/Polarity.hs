module Penny.Core.Polarity where

-- | Objects that can be neutral or, alterntively, can be polar; for
-- example, numbers on a number line that includes zero.
data T n o p
  = Center n
  -- ^ This object is neutral.
  | OffCenter o p
  -- ^ This object is not neutral.  The second field indicates the
  -- polarity of the object, while the first is the object itself.
  deriving (Eq, Ord, Show)

polarize :: p' -> T n o p -> Maybe (T n o p')
polarize _ (Center _) = Nothing
polarize p' (OffCenter o _) = Just $ OffCenter o p'
