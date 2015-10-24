{-# LANGUAGE FlexibleInstances #-}
module Penny.Display where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time
import Text.Printf

import Penny.Decimal
import Penny.Ents
import Penny.Grammar
import Penny.Grammar.Convert
import Penny.NonZero
import Penny.Polar
import Penny.Trio

-- | Display of representations.
class Display a where
  display :: a -> ShowS
  -- ^ Displays the value in a manner intended for end-user
  -- consumption.

instance Display Text where
  display a = \rest -> X.unpack a ++ rest

instance Display Int where
  display = shows

instance Display Day where
  display d = (formatTime defaultTimeLocale "%F" d ++)

instance Display TimeOfDay where
  display t = (formatTime defaultTimeLocale "%T" t ++)

instance Display TimeZone where
  display z = (printf "%+05i" (timeZoneMinutes z) ++)

instance Display Prelude.Integer where
  display = shows

instance Display a => Display [a] where
  display = foldr (.) id . map display

instance Display a => Display (S.Seq a) where
  display = F.foldr (.) id . fmap display

instance (Display a, Display b) => Display (a, b) where
  display (a, b) = display a . display b

instance (Display a, Display b, Display c) => Display (a, b, c) where
  display (a, b, c) = display a . display b . display c

instance Display a => Display (Maybe a) where
  display Nothing = id
  display (Just x) = display x

instance Display Hours where
  display (H0to19 Nothing d9) = display Zero . display d9
  display (H0to19 (Just d1) d9) = display d1 . display d9
  display (H20to23 _ d3) = display Two . display d3

instance Display ZeroTo59 where
  display (ZeroTo59 Nothing d9) = display Zero . display d9
  display (ZeroTo59 (Just d5) d9) = display d5 . display d9

instance Display PluMin where
  display Plus = ('+':)
  display Minus = ('-':)

instance Display Zero where display _ = ('0':)

instance Display One where display _ = ('1':)

instance Display Two where display _ = ('2':)

instance Display Three where display _ = ('3':)

instance Display Four where display _ = ('4':)

instance Display Five where display _ = ('5':)

instance Display Six where display _ = ('6':)

instance Display Seven where display _ = ('7':)

instance Display Eight where display _ = ('8':)

instance Display Nine where display _ = ('9':)

instance Display D1z where display = (:) . c'Char'D1z

instance Display D2z where display = (:) . c'Char'D2z

instance Display D2 where display = (:) . c'Char'D2

instance Display D3z where display = (:) . c'Char'D3z

instance Display D3 where display = (:) . c'Char'D3

instance Display D4z where display = (:) . c'Char'D4z

instance Display D4 where display = (:) . c'Char'D4

instance Display D5z where display = (:) . c'Char'D5z

instance Display D5 where display = (:) . c'Char'D5

instance Display D6z where display = (:) . c'Char'D6z

instance Display D6 where display = (:) . c'Char'D6

instance Display D7z where display = (:) . c'Char'D7z

instance Display D7 where display = (:) . c'Char'D7

instance Display D8z where display = (:) . c'Char'D8z

instance Display D8 where display = (:) . c'Char'D8

instance Display D9z where display = (:) . c'Char'D9z

instance Display D9 where display = (:) . c'Char'D9

instance Display RadPer where
  display RPComma = (',':)
  display (RPGrouper g) = display g

instance Display RadCom where
  display RCPeriod = ('.':)
  display (RCGrouper g) = display g

instance Display (Nil RadCom) where
  display (NilU x) = display x
  display (NilG x) = display x

instance Display (Nil RadPer) where
  display (NilU x) = display x
  display (NilG x) = display x

instance Display (NilGrouped RadCom) where
  display (NilGrouped may1 _rdx2 z3 zs4 g5 z6 zs7 sq8)
    = display may1 . (',':) . display z3 . display zs4 . display g5
      . display z6 . display zs7 . display sq8

instance Display (NilGrouped RadPer) where
  display (NilGrouped may1 _rdx2 z3 zs4 g5 z6 zs7 sq8)
    = display may1 . ('.':) . display z3 . display zs4 . display g5
      . display z6 . display zs7 . display sq8

instance Display (Brim RadCom) where
  display (BrimGrouped bg) = display bg
  display (BrimUngrouped bu) = display bu

instance Display (Brim RadPer) where
  display (BrimGrouped bg) = display bg
  display (BrimUngrouped bu) = display bu


instance Display Grouper where
  display ThinSpace = ('\x2009':)
  display Underscore = ('_':)

instance Display (NilUngrouped RadCom) where
  display (NUZero z may) = display z . case may of
    Nothing -> id
    Just (_rdx, may2) -> (',':) . display may2
  display (NURadix _rdx1 z2 zs3) = (',':) . display z2 . display zs3

instance Display (NilUngrouped RadPer) where
  display (NUZero z may) = display z . case may of
    Nothing -> id
    Just (_rdx, may2) -> ('.':) . display may2
  display (NURadix _rdx1 z2 zs3) = ('.':) . display z2 . display zs3


instance Display (BrimUngrouped RadCom) where
  display (BUGreaterThanOne d1 sq2 may3) = display d1 . display sq2 .
    case may3 of
      Nothing -> id
      Just (_rdx, sq4) -> (',':) . display sq4
  display (BULessThanOne may1 _rdx2 sq3 d4 ds5) =
    display may1 . (',':) . display sq3 . display d4 . display ds5

instance Display (BrimUngrouped RadPer) where
  display (BUGreaterThanOne d1 sq2 may3) = display d1 . display sq2 .
    case may3 of
      Nothing -> id
      Just (_rdx, sq4) -> ('.':) . display sq4
  display (BULessThanOne may1 _rdx2 sq3 d4 ds5) =
    display may1 . ('.':) . display sq3 . display d4 . display ds5

instance Display (BrimGrouped RadCom) where
  display (BGGreaterThanOne d1 sq2 bg1'3) =
    display d1 . display sq2 . display bg1'3
  display (BGLessThanOne m1 _rdx2 bg5'3) =
    display m1 . (',':) . display bg5'3

instance Display (BrimGrouped RadPer) where
  display (BGGreaterThanOne d1 sq2 bg1'3) =
    display d1 . display sq2 . display bg1'3
  display (BGLessThanOne m1 _rdx2 bg5'3) =
    display m1 . ('.':) . display bg5'3

instance Display (BG1 RadCom) where
  display (BG1GroupOnLeft g1 d2 sq3 sq4 may5) =
    display g1 . display d2 . display sq3 . display sq4 . case may5 of
      Nothing -> id
      Just (_rdx, may6) -> (',':) . display may6

  display (BG1GroupOnRight _rdx1 d2 sq3 g4 d5 sq6 sq7) =
    (',':) . display d2 . display sq3 . display g4 . display d5
    . display sq6 . display sq7

instance Display (BG1 RadPer) where
  display (BG1GroupOnLeft g1 d2 sq3 sq4 may5) =
    display g1 . display d2 . display sq3 . display sq4 . case may5 of
      Nothing -> id
      Just (_rdx, may6) -> ('.':) . display may6

  display (BG1GroupOnRight _rdx1 d2 sq3 g4 d5 sq6 sq7) =
    ('.':) . display d2 . display sq3 . display g4 . display d5
    . display sq6 . display sq7

instance Display r => Display (BG5 r) where
  display (BG5Novem d1 sq2 g3 d4 sq5 sq6) = display d1
    . display sq2 . display g3 . display d4 . display sq5
    . display sq6
  display (BG5Zero z1 sq2 bg6'3) = display z1 . display sq2 . display bg6'3

instance Display r => Display (BG6 r) where
  display (BG6Novem d1 sq2 r3 d4 sq5 sq6)
    = display d1 . display sq2 . display r3 . display d4
      . display sq5 . display sq6
  display (BG6Group g1 bg7'2) = display g1 . display bg7'2

instance Display r => Display (BG7 r) where
  display (BG7Zeroes z1 sq2 bg8'3) = display z1 . display sq2 . display bg8'3
  display (BG7Novem d1 sq2 sq3) = display d1 . display sq2 . display sq3

instance Display r => Display (BG8 r) where
  display (BG8Novem d1 ds2 sq3) = display d1 . display ds2 . display sq3
  display (BG8Group r bg7) = display r . display bg7


-- | Provide a simple ungrouped string for a decimal.
displayDecimalAsQty
  :: Decimal
  -> ShowS
displayDecimalAsQty d = (sideChar :) .  (' ':) . rend
  where
    sideChar = case equatorial d of
      Nothing -> ' '
      Just v
        | v == debit -> '<'
        | otherwise -> '>'
    rend = case repUngroupedDecimal (Radix :: Radix RadPer) d of
      Moderate nu -> display nu
      Extreme (Polarized bu _) -> display bu

instance Display TrioError where
  display te = case te of
    NoImbalance -> (++) . unlines $
      [ "The posting you gave requires there to be a current imbalance,"
      , "but the postings are perfectly balanced."
      ]
    MultipleImbalance i1 i2 is -> (++) . unlines $
      [ "The posting you gave requires there to be exactly one commodity"
      , "that is not balanced, but there are multiple imbalances:"
      , showImbalance i1
      , showImbalance i2
      ] ++ map showImbalance is

    CommodityNotFound cy -> (++) . unlines $
      [ "Necessary commodity not found in imbalances: " ++ X.unpack cy ]

    BalanceIsSameSide s -> (++) . unlines $
      [ "Imbalance needs to be on opposite side of given posting,"
      , "but it is on the same side: " ++ (dispSide s)
      ]
    UnsignedTooLarge rnn qnz -> (++) . unlines $
      [ "Specified quantity of "
        ++ (either display display rnn "") ++ " is larger than "
        ++ "quantity in the imbalance, which is " ++ disp qnz
      ]
    where
      showImbalance (cy, qnz) = X.unpack cy ++ " " ++ disp qnz
      disp = ($ "") . displayDecimalAsQty . fmap nonZeroToInteger
      dispSide side
        | side == debit = "<"
        | otherwise = ">"

class Friendly a where
  friendly :: a -> ShowS

instance Friendly ImbalancedError where
  friendly (ImbalancedError c1 cs) = (++) . unlines $
    [ "Transaction is not balanced.  Imbalance:"
    , showImb c1
    ] ++ map showImb cs
    where
      showImb (cy, q)
        = "  " ++ X.unpack cy ++ " "
               ++ displayDecimalAsQty (fmap nonZeroToInteger q) ""
