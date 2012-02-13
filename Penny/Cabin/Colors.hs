module Penny.Cabin.Colors (
  Colors(Colors0, Colors8, Colors256),
  Chunk,
  Color8(Black, Red, Green, Yellow, Blue, Magenta,
         Cyan, White),
  Color256,
  color256,
  Color(Default, Color),
  ForeBack(ForeBack, foreground, background),
  defaultForeBack,
  ColorSet(ColorSet, colorSet8, colorSet256),
  defaultColorSet,
  TextSpec(TextSpec, colorSet, bold, underline, flash,
           invisible, inverse),
  Switch(Off, On),
  Bold(Bold),
  Underline(Underline),
  Flash(Flash),
  Inverse(Inverse),
  Invisible(Invisible),
  Width,
  unWidth,
  chunkSize,
  chunk,
  defaultSpec,
  color) where
  

import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as X
import Data.Word (Word8, Word)

import Penny.Lincoln.Classes
  (NonNegative, add, subt, mult, fromInt, unsafeFromInt, zero,
   NonNegInt, toInt)

-- | The terminal (as described using the TERM environment variable or
-- something similar) supports at least this many colors. Remember,
-- just because the terminal is described this way by TERM does not
-- mean that the program is actually hooked up to such a terminal
-- (output might be going to a file.)
data Colors = Colors0 | Colors8 | Colors256
            deriving Show

data Bit = Bit TextSpec Text

data Chunk = Chunk (Seq Bit)

instance Monoid Chunk where
  mempty = Chunk S.empty
  mappend (Chunk c1) (Chunk c2) = Chunk (c1 `mappend` c2)

data Color8 =
  Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  deriving Show

data Color256 = Color256 Word8
                deriving Show

color256 :: Word8 -> Color256
color256 w =
  if w < 0 || w > 255
  then error "color number out of range"
  else Color256 w

data Color a = Default | Color a
data ForeBack a = ForeBack { foreground :: Color a
                           , background :: Color a }

defaultForeBack :: ForeBack a
defaultForeBack = ForeBack Default Default

data ColorSet = ColorSet { colorSet8 :: ForeBack Color8
                         , colorSet256 :: ForeBack Color256 }

defaultColorSet :: ColorSet
defaultColorSet = ColorSet defaultForeBack defaultForeBack

data TextSpec =
  TextSpec { colorSet :: ColorSet
           , bold :: Switch Bold
           , underline :: Switch Underline
           , flash :: Switch Flash
           , invisible :: Switch Invisible
           , inverse :: Switch Inverse }

data Bold = Bold
data Underline = Underline
data Flash = Flash
data Inverse = Inverse
data Invisible = Invisible

data Switch a = Off a | On a

newtype Width = Width { unWidth :: Word }
                deriving (Show, Eq, Ord)

instance NonNegative Width where
  add (Width w1) (Width w2) = Width $ w1 + w2
  subt (Width w1) (Width w2) =
    if w2 > w1
    then Nothing
    else Just (Width (w1 - w2))
  mult (Width w1) (Width w2) = Width (w1 * w2)
  zero = Width 0
  fromInt f = let fi = fromIntegral f in
    if fi < 0 then Nothing else Just (Width fi)
  unsafeFromInt f = let fi = fromIntegral f in
    if fi < 0
    then error "width cannot be negative"
    else Width fi

instance NonNegInt Width where
  toInt (Width w) = fromIntegral w

chunkSize :: Chunk -> Width
chunkSize (Chunk cs) = F.foldr f zero cs where
  f (Bit _ x) t = (unsafeFromInt . X.length $ x) `add` t

single :: Bit -> Chunk
single = Chunk . S.singleton

chunk :: TextSpec -> Text -> Chunk
chunk ts = single . Bit ts

defaultSpec :: TextSpec
defaultSpec = TextSpec { colorSet = defaultColorSet
                       , bold = Off Bold
                       , underline = Off Underline
                       , flash = Off Flash
                       , invisible = Off Invisible
                       , inverse = Off Inverse }

color :: ColorSet -> TextSpec
color cs = defaultSpec { colorSet = cs }
