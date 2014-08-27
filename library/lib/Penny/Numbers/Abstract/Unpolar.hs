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

data Unpolar r
  = Nil (Nil r)
  | Brim (Brim r)
  deriving (Eq, Ord, Show)

data Nil r
  = NilUngrouped
  | NilGrouped
  deriving (Eq, Ord, Show)

newtype LeadingZero = LeadingZero { unLeadingZero :: Bool }
  deriving (Eq, Ord, Show)

data NZeroesNonNeg = NZeroesNonNeg { unNZeroesNonNeg :: NonNeg }
  deriving (Eq, Ord, Show)

data NilUngrouped r
  = NULeadingZeroOnly
  | NUWithRad LeadingZero (Radix r)

data Brim r
  = BrimUngrouped
  | BrimGrouped
  deriving (Eq, Ord, Show)
