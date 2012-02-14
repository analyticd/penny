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
  Width(Width, unWidth),
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
import Data.Word (Word8)

import Penny.Lincoln.Classes
  (NonNeg, add, unsafeFromInt, zero, NonNegInt)
import qualified Penny.Lincoln.NonNegInt as NNI

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

newtype Width = Width { unWidth :: Int }
                deriving (Show, Eq, Ord)

chunkSize :: Chunk -> Width
chunkSize (Chunk cs) = F.foldr f (Width 0) cs where
  f (Bit _ x) (Width t) = Width $ X.length x + t

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
