module Penny.Display where

import qualified Data.Sequence as S
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as X
import Data.Time
import Text.Printf

import Penny.Grammar
import Penny.Grammar.Convert

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
