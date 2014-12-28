-- | Number representations.
module Penny.Lincoln.Rep where

import Data.Sequence (Seq)
import Data.Monoid
import Penny.Lincoln.Natural
import Penny.Lincoln.Rep.Digits

-- | A radix point.  The type is parameterized on a type that
-- represents the character used for the radix point.

data Radix a = Radix
  deriving (Eq, Ord, Show)

-- | A radix point of a comma.
data RadCom = RadCom
  deriving (Eq, Ord, Show)

period :: RadCom
period = RadCom

-- | A radix point of a period.
data RadPer = RadPer
  deriving (Eq, Ord, Show)

comma :: RadPer
comma = RadPer

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

-- | One or more zeroes.
newtype Zeroes
  = Zeroes Positive
  -- ^ @Zeroes a@, where @a@ is the number of zeroes.
  deriving (Eq, Ord, Show)

newtype Decems = Decems (Seq Decem)
  deriving (Eq, Ord, Show)

instance Monoid Decems where
  mempty = Decems mempty
  mappend (Decems x) (Decems y) = Decems (x <> y)

data DecDecs = DecDecs Decem Decems
  deriving (Eq, Ord, Show)

-- | Equal to the old DecsGroup
data DecDecsGrouped g = DecDecsGrouped g DecDecs
  deriving (Eq, Ord, Show)

-- | Equal to the old SeqDecs
newtype SeqDecDecsGrouped g
  = SeqDecDecsGrouped (Seq (DecDecsGrouped g))
  deriving (Eq, Ord, Show)

data DecDecsMayGroups g
  = DecDecsMayGroups DecDecs (SeqDecDecsGrouped g)
  deriving (Eq, Ord, Show)

data BrimGrouped2 r
  = BrimGrouped2 (Radix r) (Maybe (DecDecsMayGroups r))
  deriving (Eq, Ord, Show)

data BrimGrouped1 r
  = BG1GroupOnLeft r (DecDecsMayGroups r) (Maybe (BrimGrouped2 r))
  | BG1GroupOnRight (Radix r) DecDecs (DecDecsGrouped r)
                    (SeqDecDecsGrouped r)
  deriving (Eq, Ord, Show)

data NovDecs = NovDecs Novem Decems
  deriving (Eq, Ord, Show)

-- | Equal to the old Nodecs3

data NovDecsSeqDecDecsGrouped g
  = NovDecsSeqDecDecsGrouped NovDecs (SeqDecDecsGrouped g)
  deriving (Eq, Ord, Show)

{-
data BrimGrouped7 r
  = BG7LeadZeroes Zeroes (Either (r, BrimGrouped7 r) (
-}
