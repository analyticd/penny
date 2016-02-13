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

-- | Things that have a GrpRadCom grouping character.
class MayGroupedRadCom a where
  mayGroupersRadCom :: a -> Seq GrpRadCom

-- | Things that have a GrpRadPer grouping character.
class MayGroupedRadPer a where
  mayGroupersRadPer :: a -> Seq GrpRadPer

instance MayGroupedRadCom DigitGroupRadCom'Seq where
  mayGroupersRadCom (DigitGroupRadCom'Seq sq)
    = fmap _r'DigitGroupRadCom'0'GrpRadCom sq

instance MayGroupedRadPer DigitGroupRadPer'Seq where
  mayGroupersRadPer (DigitGroupRadPer'Seq sq)
    = fmap _r'DigitGroupRadPer'0'GrpRadPer sq

instance MayGroupedRadCom BG7RadCom where
  mayGroupersRadCom (BG7ZeroesRadCom _ _ b8)
    = mayGroupersRadCom b8
  mayGroupersRadCom (BG7NovemRadCom _ _ digs) = mayGroupersRadCom digs

instance MayGroupedRadPer BG7RadPer where
  mayGroupersRadPer (BG7ZeroesRadPer _ _ b8) = mayGroupersRadPer b8
  mayGroupersRadPer (BG7NovemRadPer _ _ digs) = mayGroupersRadPer digs

instance MayGroupedRadCom BG8RadCom where
  mayGroupersRadCom (BG8NovemRadCom _ _ digs)
    = mayGroupersRadCom digs
  mayGroupersRadCom (BG8GroupRadCom grpr b7)
    = grpr <| mayGroupersRadCom b7

instance MayGroupedRadPer BG8RadPer where
  mayGroupersRadPer (BG8NovemRadPer _ _ digs) = mayGroupersRadPer digs
  mayGroupersRadPer (BG8GroupRadPer grpr b7) = grpr <| mayGroupersRadPer b7

instance MayGroupedRadCom BG6RadCom where
  mayGroupersRadCom (BG6NovemRadCom _ _ g1 _ _ gs) = g1 <| mayGroupersRadCom gs
  mayGroupersRadCom (BG6GroupRadCom g1 b7) = g1 <| mayGroupersRadCom b7

instance MayGroupedRadPer BG6RadPer where
  mayGroupersRadPer (BG6NovemRadPer _ _ g1 _ _ gs) = g1 <| mayGroupersRadPer gs
  mayGroupersRadPer (BG6GroupRadPer g1 b7) = g1 <| mayGroupersRadPer b7

instance MayGroupedRadCom BG5RadCom where
  mayGroupersRadCom (BG5NovemRadCom _ _ g1 _ _ gs) = g1 <| mayGroupersRadCom gs
  mayGroupersRadCom (BG5ZeroRadCom _ _ b6) = mayGroupersRadCom b6

instance MayGroupedRadPer BG5RadPer where
  mayGroupersRadPer (BG5NovemRadPer _ _ g1 _ _ gs) = g1 <| mayGroupersRadPer gs
  mayGroupersRadPer (BG5ZeroRadPer _ _ b6) = mayGroupersRadPer b6

instance MayGroupedRadCom BG4RadCom where
  mayGroupersRadCom (BG4DigitRadCom _ _ digs) = mayGroupersRadCom digs
  mayGroupersRadCom BG4NilRadCom = Seq.empty

instance MayGroupedRadPer BG4RadPer where
  mayGroupersRadPer (BG4DigitRadPer _ _ digs) = mayGroupersRadPer digs
  mayGroupersRadPer BG4NilRadPer = Seq.empty

instance MayGroupedRadCom BG3RadCom where
  mayGroupersRadCom (BG3RadixRadCom _ b4) = mayGroupersRadCom b4
  mayGroupersRadCom BG3NilRadCom = Seq.empty

instance MayGroupedRadPer BG3RadPer where
  mayGroupersRadPer (BG3RadixRadPer _ b4) = mayGroupersRadPer b4
  mayGroupersRadPer BG3NilRadPer = Seq.empty

instance MayGroupedRadCom BG1RadCom where
  mayGroupersRadCom (BG1GroupOnLeftRadCom g1 _ _ digs b3)
    = g1 <| mayGroupersRadCom digs <> mayGroupersRadCom b3
  mayGroupersRadCom (BG1GroupOnRightRadCom _ _ _ g1 _ _ digs)
    = g1 <| mayGroupersRadCom digs

instance MayGroupedRadPer BG1RadPer where
  mayGroupersRadPer (BG1GroupOnLeftRadPer g1 _ _ digs b3)
    = g1 <| mayGroupersRadPer digs <> mayGroupersRadPer b3
  mayGroupersRadPer (BG1GroupOnRightRadPer _ _ _ g1 _ _ digs)
    = g1 <| mayGroupersRadPer digs

instance MayGroupedRadCom BrimGroupedRadCom where
  mayGroupersRadCom (BGGreaterThanOneRadCom _ _ bg1)
    = mayGroupersRadCom bg1
  mayGroupersRadCom (BGLessThanOneRadCom _ _ b5)
    = mayGroupersRadCom b5

instance MayGroupedRadPer BrimGroupedRadPer where
  mayGroupersRadPer (BGGreaterThanOneRadPer _ _ bg1)
    = mayGroupersRadPer bg1
  mayGroupersRadPer (BGLessThanOneRadPer _ _ b5)
    = mayGroupersRadPer b5

instance MayGroupedRadCom ZeroGroupRadCom where
  mayGroupersRadCom (ZeroGroupRadCom g _ _) = Seq.singleton g

instance MayGroupedRadPer ZeroGroupRadPer where
  mayGroupersRadPer (ZeroGroupRadPer g _ _) = Seq.singleton g

instance MayGroupedRadCom NilGroupedRadCom where
  mayGroupersRadCom (NilGroupedRadCom _ _ _ _ (ZeroGroupRadCom'Seq1 (g1, gs)))
    = addGroup g1 (foldr addGroup Seq.empty gs)
    where
      addGroup g acc = mayGroupersRadCom g <> acc

instance MayGroupedRadPer NilGroupedRadPer where
  mayGroupersRadPer (NilGroupedRadPer _ _ _ _ (ZeroGroupRadPer'Seq1 (g1, gs)))
    = addGroup g1 (foldr addGroup Seq.empty gs)
    where
      addGroup g acc = mayGroupersRadPer g <> acc

instance MayGroupedRadCom NilRadCom where
  mayGroupersRadCom (NilRadCom'NilUngroupedRadCom _)
    = Seq.empty
  mayGroupersRadCom (NilRadCom'NilGroupedRadCom x)
    = mayGroupersRadCom x

instance MayGroupedRadPer NilRadPer where
  mayGroupersRadPer (NilRadPer'NilUngroupedRadPer _)
    = Seq.empty
  mayGroupersRadPer (NilRadPer'NilGroupedRadPer x)
    = mayGroupersRadPer x

instance MayGroupedRadCom BrimRadCom where
  mayGroupersRadCom (BrimRadCom'BrimUngroupedRadCom _)
    = Seq.empty
  mayGroupersRadCom (BrimRadCom'BrimGroupedRadCom b)
    = mayGroupersRadCom b

instance MayGroupedRadPer BrimRadPer where
  mayGroupersRadPer (BrimRadPer'BrimUngroupedRadPer _)
    = Seq.empty
  mayGroupersRadPer (BrimRadPer'BrimGroupedRadPer b)
    = mayGroupersRadPer b

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
