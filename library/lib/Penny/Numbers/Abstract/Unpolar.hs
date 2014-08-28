{-# LANGUAGE EmptyDataDecls, BangPatterns #-}
-- | Unpolar abstract numbers.
module Penny.Numbers.Abstract.Unpolar where

import Control.Monad (join)
import Data.Sequence (Seq, (<|))
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Natural
import Deka.Native.Abstract
import qualified Data.Foldable as F
import Data.Monoid
import Penny.Numbers.Concrete

-- | A non-empty set that starts with something of one type and
-- concludes with a list of items of a different type.
data NE a b = NE a (Seq b)
  deriving (Eq, Ord, Show)

-- | A single zero.
data Zero = Zero
  deriving (Eq, Ord, Show)

data Zeroes = Zeroes { unZeroes :: Pos }
  deriving (Eq, Ord, Show)

data Unpolar r
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

data NG1 r = NG1 (Radix r) (Group r Zeroes) (Seq (Group r Zeroes))
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

data BG1 r
  = BG1GroupOnLeft (Group r (NE Decem Decem))
                   (Seq (Group r (NE Decem Decem)))
                   (BG2 r)
  | BG1GroupOnRight (Radix r) (NE Decem Decem)
                    (Seq (Group r (NE Decem Decem)))
  deriving (Eq, Ord, Show)

data BG2 r
  = BG2End
  | BG2Radix (Radix r) (BG3 r)
  deriving (Eq, Ord, Show)

data BG3 r
  = BG3End
  | BG3AfterRad (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  deriving (Eq, Ord, Show)

-- | Inside a 'BGFracuno'.
data BG4 r = BG4 (Maybe Zero) (Radix r) (BG5 r)
  deriving (Eq, Ord, Show)

-- | Inside a 'BG4'.  This is a fractional value of less than one, but
-- greater than zero.  So far an optional leading zero has been seen,
-- as well as a mandatory radix point.
data BG5 r
  = BG5Novem (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  | BG5Zeroes Zeroes (BG6 r)
  deriving (Eq, Ord, Show)

-- | Inside a 'BG5Zeroes'.  This is a fractional value of less than
-- one, but greater than zero.  So far seen is an optional leading
-- zero, mandatory radix point, and a run of zeroes.
--
-- This also may be inside of a 'BG7Zeroes'.
data BG6 r
  = BG6Novem (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  | BG6Group (Group r (BG7 r))
  deriving (Eq, Ord, Show)

-- |  Inside a 'BG6Group'.
data BG7 r
  = BG7Zeroes Zeroes (BG6 r)
  | BG7Novem (NE Novem Decem) (Seq (Group r (NE Decem Decem)))
  deriving (Eq, Ord, Show)
