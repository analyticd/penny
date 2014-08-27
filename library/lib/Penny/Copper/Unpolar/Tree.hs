-- | Tree to parse unpolar numbers.
module Penny.Copper.Unpolar.Tree where

import Data.Sequence (Seq)
import Data.Sums
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Concrete (NovDecs)
import Penny.Numbers.Abstract.Unpolar hiding (Zeroes)
import Deka.Native.Abstract

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

-- | One or more zeroes.
data Zeroes = Zeroes Zero (Seq Zero)
  deriving (Eq, Ord, Show)

-- | A non-empty set that starts with something of one type and
-- concludes with a list of items of a different type.
data NE a b = NE a (Seq b)
  deriving (Eq, Ord, Show)

data Start r
  = Masuno (NE Novem Decem) (Masuno1 r)
  | LeadZero Zero (LZ1 r)
  | LeadRadix (Radix r) (LR1 r)
  deriving (Eq, Ord, Show)

-- # Values greater than 1

data Masuno1 r
  = Masuno1End
  | Masuno1Radix (Radix r) (Masuno1Radix1 r)
  deriving (Eq, Ord, Show)

data Masuno1Radix1 r
  = Masuno1Radix1End
  | Masuno1Radix1Digs (NE Decem Decem) (Seq (Group r (NE Decem Decem)))
  deriving (Eq, Ord, Show)

-- # Values leading with 0

data LZ1 r
  = LZ1End
  -- ^ End here.  The result is an ungrouped, zero value (a single zero.)
  | LZ1Radix (Radix r) (LZ2 r)
  -- ^ A radix point occurs.
  deriving (Eq, Ord, Show)

-- | Inside of an LZ1Radix.  This is after the radix point.
data LZ2 r
  = LZ2End
  -- ^ End here.  The result is an ungrouped, zero value (a single
  -- zero, with a trailing radix point.)
  | LZ2Zeroes (NE Zero Zero) (LZ3 r)
  -- ^ Zeroes occur immediately after the radix point.  No groups yet.
  | LZ2NonZero (NE Novem Decem) (LZ4 r)
  -- ^ A non-zero value occurs immediately after the radix point.  No
  -- groups yet.
  deriving (Eq, Ord, Show)

-- | Inside of an LZ2.  This is after the radix point, but so
-- far all that has been seen are zeroes.
data LZ3 r
  = LZ3End
  -- ^ End here.  The result is a zero value.
  | LZ3NovDecs (NE Novem Decem) (LZ5 r)
  -- ^ A 'Novem' is seen immediately after the sequence of zeroes.
  -- The result is a non-zero value.  However, no groups have been
  -- seen yet.
  | LZ3Group (Group r (LZ6 r))
  -- ^ A group occurs next.  It's not yet known whether this group is
  -- a zero or non-zero group.
  deriving (Eq, Ord, Show)

-- | Inside of an LZ2NonZero.  This is after the radix point, but
-- there have been no groups yet.
data LZ4 r
  = LZ4End
  -- ^ End here.  The result is a non-zero, ungrouped value.
  | LZ4Groups (Group r (NE Decem Decem)) (Seq (Group r (NE Decem Decem)))
  -- ^ Groups occur.  The result is a non-zero, grouped value.
  deriving (Eq, Ord, Show)

-- | Inside of a LZ3NovDecs.  This is after the radix point, and a
-- non-zero digit has already been seen, but no groups have been seen
-- yet.
data LZ5 r
  = LZ5End
  -- ^ End here.  The result is a non-zero, ungrouped value.
  | LZ5Groups (Group r (NE Decem Decem)) (Seq (Group r (NE Decem Decem)))
  -- ^ Groups occur.  The result is a non-zero, grouped value.
  deriving (Eq, Ord, Show)

-- | Inside of an LZ3Group.  At this point, we know that the string
-- has groups.  However, it might still contain all zeroes.
data LZ6 r
  = LZ6NovDecs (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  -- ^ This group starts off with a non-zero value.  There might be
  -- additional groups.  The result is a grouped, non-zero value.
  | LZ6Zeroes (NE Zero Zero) (LZ7 r)
  -- ^ This group starts off with zeroes.
  deriving (Eq, Ord, Show)

-- | Inside of an 'LZ6Zeroes'.  The string has groups; however, it
-- might still contain all zeroes.
data LZ7 r
  = LZ7End
  -- ^ End here.  The result is a grouped, non-zero value.
  | LZ7NovDecs (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  -- ^ A 'Novem' appears.  There might be additional groups.  The
  -- result is a grouped, non-zero value.
  | LZ7Group (Group r (LZ6 r))
  -- ^ An additional group appears.
  deriving (Eq, Ord, Show)

-- # Values leading with a radix point.  Reuses some of the LZ types.

data LR1 r
  = LR1Zero (NE Zero Zero) (LZ3 r)
  | LR1NonZero (NE Novem Decem) (LZ4 r)
  deriving (Eq, Ord, Show)
