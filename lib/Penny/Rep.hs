{-# LANGUAGE TypeFamilies #-}
-- | Number representations that are not part of the grammar.
module Penny.Rep where

import Data.Monoid ((<>))
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq

import Penny.Polar
import Penny.Grammar

-- | Qty representations with a comma radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadCom = Moderated NilRadCom BrimRadCom

-- | Qty representations with a period radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadPer = Moderated NilRadPer BrimRadPer

-- | Qty representations that may be neutral or non-neutral and that
-- have a period or comma radix.  Stored along with the side if the
-- number is non-nil.
type RepAnyRadix = Either RepRadCom RepRadPer

-- | A non-neutral representation that does not include a side.
type BrimScalarAnyRadix = Either BrimRadCom BrimRadPer

-- | Number representation that may be neutral or non-neutral, with
-- a comma radix.  Does not have a polarity.
type NilOrBrimScalarRadCom = Either NilRadCom BrimRadCom

-- | Number representation that may be neutral or non-neutral, with
-- a period radix.  Does not have a polarity.
type NilOrBrimScalarRadPer = Either NilRadPer BrimRadPer

-- | Number representation that may be neutral or non-neutral, with
-- either a period or comma radix.  Does not have a polarity.
type NilOrBrimScalarAnyRadix
  = Either NilOrBrimScalarRadCom NilOrBrimScalarRadPer

-- | Things that might have grouping characters, allowing all
-- grouping characters (if any) to be extracted.
class MayGrouped a where
  type MayGrouper a :: *
  mayGroupers :: a -> Seq (MayGrouper a)

instance MayGrouped DigitGroupRadCom'Seq where
  type MayGrouper DigitGroupRadCom'Seq = GrpRadCom
  mayGroupers (DigitGroupRadCom'Seq sq)
    = fmap _r'DigitGroupRadCom'0'GrpRadCom sq

instance MayGrouped DigitGroupRadPer'Seq where
  type MayGrouper DigitGroupRadPer'Seq = GrpRadPer
  mayGroupers (DigitGroupRadPer'Seq sq)
    = fmap _r'DigitGroupRadPer'0'GrpRadPer sq

instance MayGrouped BG7RadCom where
  type MayGrouper BG7RadCom = GrpRadCom
  mayGroupers (BG7ZeroesRadCom _ _ b8) = mayGroupers b8
  mayGroupers (BG7NovemRadCom _ _ digs) = mayGroupers digs

instance MayGrouped BG8RadCom where
  type MayGrouper BG8RadCom = GrpRadCom
  mayGroupers (BG8NovemRadCom _ _ digs) = mayGroupers digs
  mayGroupers (BG8GroupRadCom grpr b7) = grpr <| mayGroupers b7

instance MayGrouped BG7RadPer where
  type MayGrouper BG7RadPer = GrpRadPer
  mayGroupers (BG7ZeroesRadPer _ _ b8) = mayGroupers b8
  mayGroupers (BG7NovemRadPer _ _ digs) = mayGroupers digs

instance MayGrouped BG8RadPer where
  type MayGrouper BG8RadPer = GrpRadPer
  mayGroupers (BG8NovemRadPer _ _ digs) = mayGroupers digs
  mayGroupers (BG8GroupRadPer grpr b7) = grpr <| mayGroupers b7

instance MayGrouped BG6RadCom where
  type MayGrouper BG6RadCom = GrpRadCom
  mayGroupers (BG6NovemRadCom _ _ g1 _ _ gs) = g1 <| mayGroupers gs
  mayGroupers (BG6GroupRadCom g1 b7) = g1 <| mayGroupers b7

instance MayGrouped BG6RadPer where
  type MayGrouper BG6RadPer = GrpRadPer
  mayGroupers (BG6NovemRadPer _ _ g1 _ _ gs) = g1 <| mayGroupers gs
  mayGroupers (BG6GroupRadPer g1 b7) = g1 <| mayGroupers b7

instance MayGrouped BG5RadCom where
  type MayGrouper BG5RadCom = GrpRadCom
  mayGroupers (BG5NovemRadCom _ _ g1 _ _ gs) = g1 <| mayGroupers gs
  mayGroupers (BG5ZeroRadCom _ _ b6) = mayGroupers b6

instance MayGrouped BG5RadPer where
  type MayGrouper BG5RadPer = GrpRadPer
  mayGroupers (BG5NovemRadPer _ _ g1 _ _ gs) = g1 <| mayGroupers gs
  mayGroupers (BG5ZeroRadPer _ _ b6) = mayGroupers b6

instance MayGrouped BG4RadCom where
  type MayGrouper BG4RadCom = GrpRadCom
  mayGroupers (BG4DigitRadCom _ _ digs) = mayGroupers digs
  mayGroupers BG4NilRadCom = Seq.empty

instance MayGrouped BG4RadPer where
  type MayGrouper BG4RadPer = GrpRadPer
  mayGroupers (BG4DigitRadPer _ _ digs) = mayGroupers digs
  mayGroupers BG4NilRadPer = Seq.empty

instance MayGrouped BG3RadCom where
  type MayGrouper BG3RadCom = GrpRadCom
  mayGroupers (BG3RadixRadCom _ b4) = mayGroupers b4
  mayGroupers BG3NilRadCom = Seq.empty

instance MayGrouped BG3RadPer where
  type MayGrouper BG3RadPer = GrpRadPer
  mayGroupers (BG3RadixRadPer _ b4) = mayGroupers b4
  mayGroupers BG3NilRadPer = Seq.empty

instance MayGrouped BG1RadCom where
  type MayGrouper BG1RadCom = GrpRadCom
  mayGroupers (BG1GroupOnLeftRadCom g1 _ _ digs b3)
    = g1 <| mayGroupers digs <> mayGroupers b3
  mayGroupers (BG1GroupOnRightRadCom _ _ _ g1 _ _ digs)
    = g1 <| mayGroupers digs

instance MayGrouped BG1RadPer where
  type MayGrouper BG1RadPer = GrpRadPer
  mayGroupers (BG1GroupOnLeftRadPer g1 _ _ digs b3)
    = g1 <| mayGroupers digs <> mayGroupers b3
  mayGroupers (BG1GroupOnRightRadPer _ _ _ g1 _ _ digs)
    = g1 <| mayGroupers digs

-- | Removes the 'Side' from a 'RepAnyRadix'.
c'NilOrBrimScalarAnyRadix'RepAnyRadix
  :: RepAnyRadix
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'RepAnyRadix ei = case ei of
  Left rc -> Left $ case rc of
    Moderate n -> Left n
    Extreme (Polarized c _) -> Right c
  Right rp -> Right $ case rp of
    Moderate n -> Left n
    Extreme (Polarized c _) -> Right c

c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix
  :: BrimScalarAnyRadix
  -> NilOrBrimScalarAnyRadix
c'NilOrBrimScalarAnyRadix'BrimScalarAnyRadix
  = either (Left . Right) (Right . Right)
