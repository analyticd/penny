{-# LANGUAGE TypeFamilies #-}
-- | Number representations that are not part of the grammar.
--
-- To convert these types to 'Penny.Decimal.Decimal' and the like,
-- functions are available in "Penny.Copper.Decopperize".
module Penny.Rep where

import qualified Control.Lens as Lens
import qualified Control.Lens.Extras as Lens (is)
import Data.Coerce (coerce)
import Data.Monoid ((<>))
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import qualified Data.Sequence.NonEmpty as NE

import Penny.Copper.Types
import Penny.Copper.Terminalizers
import Penny.Polar

-- | Qty representations with a comma radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadCom = Moderated (NilRadCom Char ()) (BrimRadCom Char ())

-- | Qty representations with a period radix that may be either nil
-- or brim.  Stored along with the side if the number is non-nil.
type RepRadPer = Moderated (NilRadPer Char ()) (BrimRadPer Char ())

-- | Qty representations that may be neutral or non-neutral and that
-- have a period or comma radix.  Stored along with the side if the
-- number is non-nil.
type RepAnyRadix = Either RepRadCom RepRadPer

oppositeRepAnyRadix :: RepAnyRadix -> RepAnyRadix
oppositeRepAnyRadix
  = Lens.over Lens._Left oppositeModerated
  . Lens.over Lens._Right oppositeModerated

-- | True if the 'RepAnyRadix' is zero.
repAnyRadixIsZero :: RepAnyRadix -> Bool
repAnyRadixIsZero rar
  = Lens.is (Lens._Left . _Moderate) rar
  || Lens.is (Lens._Right . _Moderate) rar

-- | A non-neutral representation that does not include a side.
type BrimAnyRadix = Either (BrimRadCom Char ()) (BrimRadPer Char ())

-- | A neutral representation of any radix.
type NilAnyRadix = Either (NilRadCom Char ()) (NilRadPer Char ())

-- | Number representation that may be neutral or non-neutral, with
-- either a period or comma radix.  Does not have a polarity.
type NilOrBrimAnyRadix
  = Either (NilOrBrimRadCom Char ()) (NilOrBrimRadPer Char ())

c'RepAnyRadix'BrimAnyRadix
  :: Pole
  -- ^ Use this side
  -> BrimAnyRadix
  -> RepAnyRadix
c'RepAnyRadix'BrimAnyRadix pole b = case b of
  Left brc -> Left . Extreme $ Polarized brc pole
  Right brp -> Right . Extreme $ Polarized brp pole


t'NilOrBrimAnyRadix
  :: NilOrBrimAnyRadix
  -> Seq Char
t'NilOrBrimAnyRadix
  = NE.nonEmptySeqToSeq
  . fmap fst
  . either t'NilOrBrimRadCom t'NilOrBrimRadPer


splitNilOrBrimAnyRadix
  :: NilOrBrimAnyRadix
  -> Either NilAnyRadix BrimAnyRadix
splitNilOrBrimAnyRadix x = case x of
  Left nobCom -> case nobCom of
    NilOrBrimRadCom'NilRadCom nilCom -> Left (Left nilCom)
    NilOrBrimRadCom'BrimRadCom brimCom -> Right (Left brimCom)
  Right nobPer -> case nobPer of
    NilOrBrimRadPer'NilRadPer nilPer -> Left (Right nilPer)
    NilOrBrimRadPer'BrimRadPer brimPer -> Right (Right brimPer)

groupers'DigitGroupRadCom'Star
  :: DigitGroupRadCom'Star t a
  -> Seq (GrpRadCom t a)
groupers'DigitGroupRadCom'Star
  = fmap _r'DigitGroupRadCom'0'GrpRadCom . coerce

groupers'DigitGroupRadPer'Star
  :: DigitGroupRadPer'Star t a
  -> Seq (GrpRadPer t a)
groupers'DigitGroupRadPer'Star
  = fmap _r'DigitGroupRadPer'0'GrpRadPer . coerce

groupers'BG7RadCom
  :: BG7RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG7RadCom (BG7ZeroesRadCom _ _ b8)
  = groupers'BG8RadCom b8
groupers'BG7RadCom (BG7NovemRadCom _ _ digs)
  = groupers'DigitGroupRadCom'Star digs

groupers'BG8RadCom
  :: BG8RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG8RadCom (BG8NovemRadCom _ _ digs)
  = groupers'DigitGroupRadCom'Star digs
groupers'BG8RadCom (BG8GroupRadCom grpr b7)
  = grpr <| groupers'BG7RadCom b7

groupers'BG7RadPer
  :: BG7RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG7RadPer (BG7ZeroesRadPer _ _ b8)
  = groupers'BG8RadPer b8
groupers'BG7RadPer (BG7NovemRadPer _ _ digs)
  = groupers'DigitGroupRadPer'Star digs

groupers'BG8RadPer
  :: BG8RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG8RadPer (BG8NovemRadPer _ _ digs)
  = groupers'DigitGroupRadPer'Star digs
groupers'BG8RadPer (BG8GroupRadPer grpr b7)
  = grpr <| groupers'BG7RadPer b7

groupers'BG6RadCom
  :: BG6RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG6RadCom (BG6NovemRadCom _ _ g1 _ _ gs)
  = g1 <| groupers'DigitGroupRadCom'Star gs
groupers'BG6RadCom (BG6GroupRadCom g1 b7) = g1 <| groupers'BG7RadCom b7

groupers'BG6RadPer
  :: BG6RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG6RadPer (BG6NovemRadPer _ _ g1 _ _ gs)
  = g1 <| groupers'DigitGroupRadPer'Star gs
groupers'BG6RadPer (BG6GroupRadPer g1 b7) = g1 <| groupers'BG7RadPer b7

groupers'BG5RadCom
  :: BG5RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG5RadCom (BG5NovemRadCom _ _ g1 _ _ gs)
  = g1 <| groupers'DigitGroupRadCom'Star gs
groupers'BG5RadCom (BG5ZeroRadCom _ _ b6)
  = groupers'BG6RadCom b6

groupers'BG5RadPer
  :: BG5RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG5RadPer (BG5NovemRadPer _ _ g1 _ _ gs)
  = g1 <| groupers'DigitGroupRadPer'Star gs
groupers'BG5RadPer (BG5ZeroRadPer _ _ b6)
  = groupers'BG6RadPer b6

groupers'BG4RadCom
  :: BG4RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG4RadCom (BG4DigitRadCom _ _ digs)
  = groupers'DigitGroupRadCom'Star digs
groupers'BG4RadCom BG4NilRadCom = Seq.empty

groupers'BG4RadPer
  :: BG4RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG4RadPer (BG4DigitRadPer _ _ digs)
  = groupers'DigitGroupRadPer'Star digs
groupers'BG4RadPer BG4NilRadPer = Seq.empty

groupers'BG3RadCom
  :: BG3RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG3RadCom (BG3RadixRadCom _ b4)
  = groupers'BG4RadCom b4
groupers'BG3RadCom BG3NilRadCom = Seq.empty

groupers'BG3RadPer
  :: BG3RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG3RadPer (BG3RadixRadPer _ b4)
  = groupers'BG4RadPer b4
groupers'BG3RadPer BG3NilRadPer = Seq.empty

groupers'BG1RadCom
  :: BG1RadCom t a
  -> Seq (GrpRadCom t a)
groupers'BG1RadCom (BG1GroupOnLeftRadCom g1 _ _ digs b3)
    = g1 <| groupers'DigitGroupRadCom'Star digs <> groupers'BG3RadCom b3
groupers'BG1RadCom (BG1GroupOnRightRadCom _ _ _ g1 _ _ digs)
  = g1 <| groupers'DigitGroupRadCom'Star digs

groupers'BG1RadPer
  :: BG1RadPer t a
  -> Seq (GrpRadPer t a)
groupers'BG1RadPer (BG1GroupOnLeftRadPer g1 _ _ digs b3)
    = g1 <| groupers'DigitGroupRadPer'Star digs <> groupers'BG3RadPer b3
groupers'BG1RadPer (BG1GroupOnRightRadPer _ _ _ g1 _ _ digs)
  = g1 <| groupers'DigitGroupRadPer'Star digs

groupers'BrimGroupedRadCom
  :: BrimGroupedRadCom t a
  -> Seq (GrpRadCom t a)
groupers'BrimGroupedRadCom (BGGreaterThanOneRadCom _ _ bg1)
  = groupers'BG1RadCom bg1
groupers'BrimGroupedRadCom (BGLessThanOneRadCom _ _ b5)
  = groupers'BG5RadCom b5

groupers'BrimGroupedRadPer
  :: BrimGroupedRadPer t a
  -> Seq (GrpRadPer t a)
groupers'BrimGroupedRadPer (BGGreaterThanOneRadPer _ _ bg1)
  = groupers'BG1RadPer bg1
groupers'BrimGroupedRadPer (BGLessThanOneRadPer _ _ b5)
  = groupers'BG5RadPer b5

groupers'ZeroGroupRadCom
  :: ZeroGroupRadCom t a
  -> Seq (GrpRadCom t a)
groupers'ZeroGroupRadCom (ZeroGroupRadCom g _ _) = Seq.singleton g

groupers'ZeroGroupRadPer
  :: ZeroGroupRadPer t a
  -> Seq (GrpRadPer t a)
groupers'ZeroGroupRadPer (ZeroGroupRadPer g _ _) = Seq.singleton g

groupers'NilGroupedRadCom
  :: NilGroupedRadCom t a
  -> Seq (GrpRadCom t a)
groupers'NilGroupedRadCom
  (NilGroupedRadCom _ _ _ _ (ZeroGroupRadCom'Plus (NE.NonEmptySeq g1 gs)))
  = addGroup g1 (foldr addGroup Seq.empty gs)
  where
    addGroup g acc = groupers'ZeroGroupRadCom g <> acc

groupers'NilGroupedRadPer
  :: NilGroupedRadPer t a
  -> Seq (GrpRadPer t a)
groupers'NilGroupedRadPer
  (NilGroupedRadPer _ _ _ _ (ZeroGroupRadPer'Plus (NE.NonEmptySeq g1 gs)))
  = addGroup g1 (foldr addGroup Seq.empty gs)
  where
    addGroup g acc = groupers'ZeroGroupRadPer g <> acc

groupers'NilRadCom
  :: NilRadCom t a
  -> Seq (GrpRadCom t a)
groupers'NilRadCom (NilRadCom'NilUngroupedRadCom _)
  = Seq.empty
groupers'NilRadCom (NilRadCom'NilGroupedRadCom x)
  = groupers'NilGroupedRadCom x

groupers'NilRadPer
  :: NilRadPer t a
  -> Seq (GrpRadPer t a)
groupers'NilRadPer (NilRadPer'NilUngroupedRadPer _)
  = Seq.empty
groupers'NilRadPer (NilRadPer'NilGroupedRadPer x)
  = groupers'NilGroupedRadPer x

groupers'BrimRadCom
  :: BrimRadCom t a
  -> Seq (GrpRadCom t a)
groupers'BrimRadCom (BrimRadCom'BrimUngroupedRadCom _)
  = Seq.empty
groupers'BrimRadCom (BrimRadCom'BrimGroupedRadCom b)
  = groupers'BrimGroupedRadCom b

groupers'BrimRadPer
  :: BrimRadPer t a
  -> Seq (GrpRadPer t a)
groupers'BrimRadPer (BrimRadPer'BrimUngroupedRadPer _)
  = Seq.empty
groupers'BrimRadPer (BrimRadPer'BrimGroupedRadPer b)
  = groupers'BrimGroupedRadPer b

groupers'RepRadCom
  :: RepRadCom
  -> Seq (GrpRadCom Char ())
groupers'RepRadCom x = case x of
  Moderate n -> groupers'NilRadCom n
  Extreme (Polarized o _) -> groupers'BrimRadCom o

groupers'RepRadPer
  :: RepRadPer
  -> Seq (GrpRadPer Char ())
groupers'RepRadPer x = case x of
  Moderate n -> groupers'NilRadPer n
  Extreme (Polarized o _) -> groupers'BrimRadPer o

groupers'RepAnyRadix
  :: RepAnyRadix
  -> Either (Seq (GrpRadCom Char ())) (Seq (GrpRadPer Char ()))
groupers'RepAnyRadix = either (Left . groupers'RepRadCom)
  (Right . groupers'RepRadPer)

groupers'BrimAnyRadix
  :: BrimAnyRadix
  -> Either (Seq (GrpRadCom Char ())) (Seq (GrpRadPer Char ()))
groupers'BrimAnyRadix = either (Left . groupers'BrimRadCom)
  (Right . groupers'BrimRadPer)

-- | Removes the 'Side' from a 'RepAnyRadix'.
c'NilOrBrimAnyRadix'RepAnyRadix
  :: RepAnyRadix
  -> NilOrBrimAnyRadix
c'NilOrBrimAnyRadix'RepAnyRadix ei = case ei of
  Left rc -> Left $ case rc of
    Moderate n -> NilOrBrimRadCom'NilRadCom n
    Extreme (Polarized c _) -> NilOrBrimRadCom'BrimRadCom c
  Right rp -> Right $ case rp of
    Moderate n -> NilOrBrimRadPer'NilRadPer n
    Extreme (Polarized c _) -> NilOrBrimRadPer'BrimRadPer c

-- | Removes the 'Side' from a 'RepAnyRadix' and returns either Nil
-- or a Brim with the side.
splitRepAnyRadix
  :: RepAnyRadix
  -> Either NilAnyRadix (BrimAnyRadix, Pole)
splitRepAnyRadix ei = case ei of
  Left rc -> case rc of
    Moderate n -> Left . Left $ n
    Extreme (Polarized brim pole) -> Right (Left brim, pole)
  Right rp -> case rp of
    Moderate n -> Left . Right $ n
    Extreme (Polarized brim pole) -> Right (Right brim, pole)

c'NilOrBrimAnyRadix'BrimAnyRadix
  :: BrimAnyRadix
  -> NilOrBrimAnyRadix
c'NilOrBrimAnyRadix'BrimAnyRadix brim = case brim of
  Left brc -> Left (NilOrBrimRadCom'BrimRadCom brc)
  Right brp -> Right (NilOrBrimRadPer'BrimRadPer brp)

c'NilOrBrimAnyRadix'NilAnyRadix
  :: NilAnyRadix
  -> NilOrBrimAnyRadix
c'NilOrBrimAnyRadix'NilAnyRadix nil = case nil of
  Left nrc -> Left (NilOrBrimRadCom'NilRadCom nrc)
  Right nrp -> Right (NilOrBrimRadPer'NilRadPer nrp)


-- # Poles
pole'RepRadCom :: RepRadCom -> Maybe Pole
pole'RepRadCom = pole'Moderated

pole'RepRadPer :: RepRadPer -> Maybe Pole
pole'RepRadPer = pole'Moderated

pole'RepAnyRadix :: RepAnyRadix -> Maybe Pole
pole'RepAnyRadix = either pole'RepRadCom pole'RepRadPer

