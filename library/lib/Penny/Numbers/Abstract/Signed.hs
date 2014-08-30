module Penny.Numbers.Abstract.Signed where

import Penny.Numbers.Abstract.Unsigned

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

newtype Signed r p = Signed { unSigned :: Polarity (Nil r) (Brim r) p }
  deriving (Eq, Ord, Show)

newtype SignedUngrouped r p = SignedUngrouped
  { unSignedUngrouped :: Polarity (NilUngrouped r) (BrimUngrouped r) p }
  deriving (Eq, Ord, Show)

newtype SignedGrouped r p = SignedGrouped
  { unSignedGrouped :: Polarity (NilGrouped r) (BrimGrouped r) p }
  deriving (Eq, Ord, Show)

signNeutral :: Unsigned r -> Maybe (Signed r p)
signNeutral u = case u of
  Nil n -> Just . Signed $ Center n
  _ -> Nothing

signOffCenter :: p -> Unsigned r -> Maybe (Signed r p)
signOffCenter s u = case u of
  Brim b -> Just . Signed $ OffCenter b s
  _ -> Nothing

brimToSigned :: p -> Brim r -> Signed r p
brimToSigned p b = Signed $ OffCenter b p
