module Penny.Numbers.Abstract.Polar where

import Penny.Numbers.Abstract.Unpolar

-- # Polarity

-- | Holds polar types, along with their polarity.  The first type
-- variable is the type for neutral types.  The second type variable
-- is for opposing types.  The third type variable is the polarity
-- itself, e.g. Debit or Credit.
data Polarity n o p
  = Center n
  | OffCenter o p
  deriving (Eq, Ord, Show)

casePolarity :: (n -> r) -> (o -> p -> r) -> Polarity n o p -> r
casePolarity fn fo a = case a of
  Center n -> fn n
  OffCenter p o -> fo p o

mapPolarity
  :: (n -> n')
  -> (o -> o')
  -> Polarity n o p
  -> Polarity n' o' p
mapPolarity fn fo py = case py of
  Center n -> Center (fn n)
  OffCenter o p -> OffCenter (fo o) p

newtype Polar r p = Polar { unPolar :: Polarity (Nil r) (Brim r) p }
  deriving (Eq, Ord, Show)

newtype Ungrouped r p = Unpolar
  { unUngrouped :: Polarity (NilUngrouped r) (BrimUngrouped r) p }
  deriving (Eq, Ord, Show)

newtype Grouped r p = Grouped
  { unGrouped :: Polarity (NilGrouped r) (BrimGrouped r) p }
  deriving (Eq, Ord, Show)

ungroup :: Grouped r p -> Ungrouped r p
ungroup = undefined
