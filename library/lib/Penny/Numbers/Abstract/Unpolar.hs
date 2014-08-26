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

data ZeroesNovDecs = ZeroesNovDecs
  { zndZeroes :: NonNeg
  , zndNovDecs :: NovDecs
  } deriving (Eq, Ord, Show)

data DecDecs = DecDecs Decem (Seq Decem)
  deriving (Eq, Ord, Show)

flattenDecDecs :: DecDecs -> Seq Decem
flattenDecDecs (DecDecs d1 ds) = d1 <| ds

flattenGroupedDecDecs
  :: Seq (Group r DecDecs)
  -> Seq Decem
flattenGroupedDecDecs
  = join
  . fmap (flattenDecDecs . groupPayload)

newtype HasZeroDigit = HasZeroDigit { unHasZeroDigit :: Bool }
  deriving (Eq, Ord, Show)

data ZeroDigit = ZeroDigit
  deriving (Eq, Ord, Show)

newtype Zeroes = Zeroes { unZeroes :: Pos }
  deriving (Eq, Ord, Show)

addZeroes :: Zeroes -> Zeroes -> Zeroes
addZeroes (Zeroes x) (Zeroes y) = Zeroes $ addPos x y

-- Ungrouped - non-zero

newtype UNWhole = UNWhole { unUNWhole :: NovDecs }
  deriving (Eq, Ord, Show)

data UNWholeRadix r = UNWholeRadix NovDecs (Radix r) (Maybe DecDecs)
  deriving (Eq, Ord, Show)

data UNRadFrac r = UNRadFrac HasZeroDigit (Radix r) ZeroesNovDecs
  deriving (Eq, Ord, Show)

-- Ungrouped - zero

-- | An ungrouped zero; consists only of the zero digit.
data UZZeroOnly = UZZeroOnly
  deriving (Eq, Ord, Show)

data UZTrailing r = UZTrailing HasZeroDigit (Radix r) (Maybe Zeroes)
  deriving (Eq, Ord, Show)

-- Grouped - zero

data GZ r = GZ HasZeroDigit (Radix r) Zeroes (Group r Zeroes)
                 (Seq (Group r Zeroes))
  deriving (Eq, Ord, Show)

ungroupGZ :: GZ r -> UZTrailing r
ungroupGZ (GZ _ rdx z1 g1 gs) =
  UZTrailing (HasZeroDigit True) rdx (Just zs)
  where
    zs = F.foldl' addZeroes (addZeroes z1 . groupPayload $ g1)
      . fmap groupPayload $ gs

-- Grouped - non-zero

-- Grouped - greater than or equal to one

-- | Greater than or equal to one, grouped on left side.  No radix.
data MasunoGroupedLeft r =
  MasunoGroupedLeft NovDecs (Group r DecDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

ungroupMasunoGroupedLeft
  :: MasunoGroupedLeft r
  -> UNWhole
ungroupMasunoGroupedLeft (MasunoGroupedLeft (NovDecs nv ds) g1 gs) =
  UNWhole . NovDecs nv $ ds <> flattenDecDecs (groupPayload g1)
    <> flattenGroupedDecDecs gs

-- | Greater than or equal to one, grouped on left side, with radix.
-- Optional grouping on right side.
data MasunoGroupedLeftRad r =
  MasunoGroupedLeftRad (MasunoGroupedLeft r)
                       (Radix r)
                       (Maybe (DecDecs, Seq (Group r DecDecs)))
  deriving (Eq, Ord, Show)

ungroupMasunoGroupedLeftRad
  :: MasunoGroupedLeftRad r
  -> UNWholeRadix r
ungroupMasunoGroupedLeftRad (MasunoGroupedLeftRad mgl rdx may) =
  UNWholeRadix nd rdx may'
  where
    nd = unUNWhole $ ungroupMasunoGroupedLeft mgl
    may' = fmap ungroupPair may
    ungroupPair (DecDecs d1 ds, sq) = DecDecs d1 (ds <>
      flattenGroupedDecDecs sq)

-- | Greater than or equal to one, grouped on right side only.

data MasunoGroupedRight r =
  MasunoGroupedRight (NovDecs) (Radix r)
                     DecDecs (Group r DecDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

ungroupMasunoGroupedRight
  :: MasunoGroupedRight r
  -> UNWholeRadix r
ungroupMasunoGroupedRight (MasunoGroupedRight nd rdx dd1 g1 gs) =
  UNWholeRadix nd rdx (Just dd')
  where
    DecDecs d1 ds = dd1
    dd' = DecDecs d1 (ds <> flattenDecDecs (groupPayload g1)
      <> flattenGroupedDecDecs gs)

-- Grouped - less than one

-- | Less than one, first group is zeroes only.  Optional leading
-- zero.  Immediately after the last group of zeroes (without being
-- prefixed by a grouping character) is a NovDecs, followed by
-- optional additional groups.

data FracunoFirstGroupZNovDecs r =
  FracunoFirstGroupZNovDecs HasZeroDigit (Radix r)
  Zeroes (Group r Zeroes) (Seq (Group r Zeroes)) NovDecs
  (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

ungroupFracunoFirstGroupZNovDecs
  :: FracunoFirstGroupZNovDecs r
  -> UNRadFrac r
ungroupFracunoFirstGroupZNovDecs = undefined

-- | Less than one, first group is zeroes only.  Optional leading
-- zero.  After the last group of zeroes is a group with optional
-- zeroes, followed by a NovDecs.  Optional additional groups follow.
data FracunoFirstGroupZ r =
  FracunoFirstGroupZ HasZeroDigit (Radix r) Zeroes
  (Seq (Group r Zeroes)) (Group r ZeroesNovDecs) (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

ungroupFracunoFirstGroupZ
  :: FracunoFirstGroupZ r
  -> UNRadFrac r
ungroupFracunoFirstGroupZ fnz = UNRadFrac hzd rdx znd
  where
    FracunoFirstGroupZ hzd rdx zz1 zzs gz1 dds = fnz
    zzTot = F.foldl add zz . fmap groupPayload $ zzs
      where
        zz = case nonNegToPos . zndZeroes . groupPayload $ gz1 of
          Nothing -> zz1
          Just p -> add zz1 . Zeroes $ p
        add = addZeroes
    znd = ZeroesNovDecs (posToNonNeg . unZeroes $ zzTot) nd
    nd = NovDecs nd1 dd
    NovDecs nd1 dd1 = zndNovDecs . groupPayload $ gz1
    dd = dd1 <> flattenGroupedDecDecs dds


-- | Less than one, first group has non-zero digit.  Optional leading
-- zero.
data FracunoFirstGroupNZ r =
  FracunoFirstGroupNZ HasZeroDigit (Radix r)
                      ZeroesNovDecs (Group r DecDecs)
                      (Seq (Group r DecDecs))
  deriving (Eq, Ord, Show)

ungroupFracunoFirstGroupNZ
  :: FracunoFirstGroupNZ r
  -> UNRadFrac r
ungroupFracunoFirstGroupNZ fnz = UNRadFrac hzd rdx znd'
  where
    FracunoFirstGroupNZ hzd rdx znd g1 gs = fnz
    ZeroesNovDecs nzz (NovDecs nv dd1) = znd
    dds = dd1 <> flattenDecDecs (groupPayload g1)
      <> flattenGroupedDecDecs gs
    znd' = ZeroesNovDecs nzz (NovDecs nv dds)
