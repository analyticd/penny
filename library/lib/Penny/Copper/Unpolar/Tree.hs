-- | Tree to parse unpolar numbers.
module Penny.Copper.Unpolar.Tree where

import Data.Sequence (Seq)
import Data.Sums
import Penny.Numbers.Abstract.RadGroup
import Penny.Numbers.Concrete (NovDecs)
import Penny.Numbers.Abstract.Unpolar hiding (Zeroes)

-- | A single zero.
data Zero = Zero

-- | One or more zeroes.
data Zeroes = Zeroes Zero (Seq Zero)

newtype Start r = Start (S3 (NovDecsFirst r) (ZeroFirst r) (RadixFirst r))

-- | 'NovDecs' that occur on the left side of the string.  All these
-- values necessarily are not equal to zero.
data NovDecsFirst r = NovDecsFirst NovDecs (NDF1 r)

-- | First production that occurs in the string after a
-- 'NovDecsFirst'.

data NDF1 r
  = NDF1End
  -- ^ The string ends immediately after the 'NovDecs'.
  | NDF1Radix (Radix r) (NDF1Radix r)
  -- ^ The string is followed by a radix point.
  | NDF1Group (Group r DecDecs) (Seq (Group r DecDecs)) (NDF1Group r)
  -- ^ The string is followed by a group of digits

-- | Occurs after the radix point in a string that begins (on the left
-- side of the radix) with a 'NovDecs'.

data NDF1Radix r
  = NDF1RadixEnd
  -- ^ The string ends immediately after the radix point.
  | NDF1RadixDigits DecDecs (NDF1RadixDigits r)
  -- ^ There are digits after the radix point.

-- | Digits occuring after the radix point in a string that begins (on
-- the left side of the radix) with a 'NovDecs'.  The first group of
-- digits after the radix point is already represented in the
-- 'NDF1Radix'.
data NDF1RadixDigits r
  = NDF1RadixDigitsEnd
  -- ^ There are no groups after the first group.
  | NDF1RadixDigitsGroups (Group r DecDecs) (Seq (Group r DecDecs))
  -- ^ There is at least one group after the first group.

-- | Immediately follows the first groups of digits in a string that
-- begins (left of the radix) with a 'NovDecs'.  There is at least one
-- group of digits to the left of this production, though there could
-- be more than one.

data NDF1Group r
  = NDF1GroupEnd
  -- ^ There is nothing after the first groups of digits.
  | NDF1GroupRadix (Radix r) (NDF1Radix r)
  -- ^ There is a radix point, possibly followed by something after
  -- the radix.

-- # ZeroFirst

-- | Strings that begin with a zero.
data ZeroFirst r = ZeroFirst Zero (ZF r)

-- | First production that occurs after the zero in a 'ZeroFirst'.

data ZF r
  = ZFEnd
  -- ^ There is nothing after the zero.
  | ZFRadix (Radix r) (AfterRad r)
  -- ^ There is a radix after the zero.

-- | Production after the radix point, either in a 'ZeroFirst' or in a
-- 'RadixFirst'.
data AfterRad r
  = AREnd
  -- ^ There is nothing after the radix point.
  | ARZeroes Zeroes (ARZeroes r)
  -- ^ There are one or more groups of zeroes after the radix point.
  -- The first set of zeroes is not preceded by a grouping character.
  | ARNovDecs NovDecs (Seq (Group r DecDecs))
  -- ^ Immediately after the radix point is a NovDecs, followed by
  -- optional groups.

-- | After the first set of zeroes after the radix.
data ARZeroes r
  = ARZEnd
  -- ^ There is nothing after the first set of zeroes.
  | ARZGroupsZ (Group r Zeroes) (Seq (Group r Zeroes)) (ARZNext r)
  -- ^ There is at least one group of zeroes after the first set of
  -- zeroes. FIXME - this is not predictive; after parsing a separator
  -- in the last group, the entire parser will fail if a zero does not
  -- follow, which is not desirable
  | ARZNovDecs NovDecs (Seq (Group r DecDecs))
  -- ^ Immediately after the first zeroes, without a grouping
  -- character, is a 'NovDecs', followed by optional groups.

-- | After groups of zeroes to the right of the radix.
data ARZNext r
  = ARZNEnd
  -- ^ There is nothing after the last group of zeroes.

  | ARZNNovDecs NovDecs (Seq (Group r DecDecs))
  -- ^ Immediately after the last group of zeroes (not separated by a
  -- grouping character) there are NovDecs, followed by optional
  -- groups of 'DecDecs'.

  | ARZNGroups (Group r (Maybe Zeroes, NovDecs)) (Seq (Group r DecDecs))
  -- ^ After the last group of zeroes, separated by a grouping
  -- character, is a group that optionally leads with zeroes but then
  -- contains at least one non-zero digit.  Optional additional groups
  -- follow.

-- # RadixFirst

-- | The radix is the first character on the left of the string.
data RadixFirst r = RadixFirst (Radix r) (AfterRad r)

