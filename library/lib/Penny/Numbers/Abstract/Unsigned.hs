{-# LANGUAGE EmptyDataDecls, BangPatterns #-}
-- | Unsigned abstract numbers.
module Penny.Numbers.Abstract.Unsigned where

import Data.Sequence (Seq)
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Natural
import Deka.Native.Abstract

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

data Zeroes = Zeroes { unZeroes :: Pos }
  deriving (Eq, Ord, Show)

data Unsigned r
  = Nil (Nil r)
  | Brim (Brim r)
  deriving (Eq, Ord, Show)

data Nil r
  = NilUngrouped (NilUngrouped r)
  | NilGrouped (NilGrouped r)
  deriving (Eq, Ord, Show)

newtype LeadingZero = LeadingZero { unLeadingZero :: Bool }
  deriving (Eq, Ord, Show)

data NZeroesNonNeg = NZeroesNonNeg { unNZeroesNonNeg :: NonNeg }
  deriving (Eq, Ord, Show)

data NilUngrouped r
  = NULeadingZero Zero (NU1 r)
  | NUNoLeadingZero (Radix r) Zeroes
  deriving (Eq, Ord, Show)

data NU1 r
  = NU1End
  | NU1Radix (Radix r) NU2
  deriving (Eq, Ord, Show)

data NU2
  = NU2End
  | NU2Zeroes Zeroes
  deriving (Eq, Ord, Show)

-- # NilGrouped

data NilGrouped r
  = NGLeadingZero Zero (NG1 r)
  | NGNoLeadingZero (NG1 r)
  deriving (Eq, Ord, Show)

data NG1 r = NG1 (Radix r) r Zeroes (Seq (r, Zeroes))
  deriving (Eq, Ord, Show)

-- # Brim

data Brim r
  = BrimUngrouped (BrimUngrouped r)
  | BrimGrouped (BrimGrouped r)
  deriving (Eq, Ord, Show)

-- ## BrimUngrouped

data BrimUngrouped r
  = BUMasuno (NE Novem Decem) (BU1 r)
  | BUFracuno (BU2 r)
  deriving (Eq, Ord, Show)

data BU1 r
  = BU1End
  | BU1Radix (Radix r) (Seq Decem)
  deriving (Eq, Ord, Show)

data BU2 r
  = BU2LeadingZero Zero (Radix r) BU3
  | BU2NoLeadingZero (Radix r) BU3
  deriving (Eq, Ord, Show)

data BU3
  = BU3Zeroes Zeroes (NE Novem Decem)
  | BU3NoZeroes (NE Novem Decem)
  deriving (Eq, Ord, Show)

-- ## BrimGrouped

data BrimGrouped r
  = BGMasuno (NE Novem Decem) (BG1 r)
  | BGFracuno (BG4 r)
  deriving (Eq, Ord, Show)

-- | Inside a 'BGMasuno'.  At this point, the first group of digits on
-- the left side of the radix point has been seen.  No radix point or
-- additional groups have yet been seen.
data BG1 r
  = BG1GroupOnLeft r (NE Decem Decem)
                   (Seq (r, (NE Decem Decem)))
                   (BG2 r)
  | BG1GroupOnRight (Radix r) (NE Decem Decem)
                    (Seq (r, (NE Decem Decem)))
  deriving (Eq, Ord, Show)

-- | Inside a 'BG1GroupOnLeft'.  At this point, everything on the left
-- side of the radix point has been seen; 'BG2' determines whether
-- there is a radix point or not.
data BG2 r
  = BG2End
  | BG2Radix (Radix r) (BG3 r)
  deriving (Eq, Ord, Show)

data BG3 r
  = BG3End
  | BG3AfterRad (NE Decem Decem) (Seq (r, (NE Decem Decem)))
  deriving (Eq, Ord, Show)

-- | Inside a 'BGFracuno'.
data BG4 r = BG4 (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

-- | Inside a 'BG4'.  This is a fractional value of less than one, but
-- greater than zero.  So far an optional leading zero has been seen,
-- as well as a mandatory radix point.
data BG5 r
  = BG5Novem (NE Novem Decem) (Seq (r, (NE Decem Decem)))
  | BG5Zeroes Zeroes (BG6 r)
  deriving (Eq, Ord, Show)

-- | Inside a 'BG5Zeroes'.  This is a fractional value of less than
-- one, but greater than zero.  So far seen is an optional leading
-- zero, mandatory radix point, and a run of zeroes.
--
-- This also may be inside of a 'BG7Zeroes'.
data BG6 r
  = BG6Novem (NE Novem Decem) (Seq (r, (NE Decem Decem)))
  | BG6Group r (BG7 r)
  deriving (Eq, Ord, Show)

-- |  Inside a 'BG6Group'.
data BG7 r
  = BG7Zeroes Zeroes (BG6 r)
  | BG7Novem (NE Novem Decem) (Seq (r, (NE Decem Decem)))
  deriving (Eq, Ord, Show)
