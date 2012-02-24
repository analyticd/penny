module Penny.Cabin.Colors (
  -- * Colors
  Colors(Colors0, Colors8, Colors256),
  Color,
  Color8,
  Color256,
  black, red, green, yellow, blue, magenta, cyan, white,
  color256,
  defaultColor,

  -- * Chunks
  Chunk,
  chunk,
  chunkSize,
  Width(Width, unWidth),

  -- * Style and TextSpec
  Style(Style, foreground, background, bold, underline, flash,
           invisible, inverse),
  TextSpec(TextSpec, style8, style256),
  defaultStyle,
  defaultSpec,

  -- * Specific effects
  Switch(Off, On),
  Bold(Bold),
  Underline(Underline),
  Flash(Flash),
  Inverse(Inverse),
  Invisible(Invisible) ) where
  

import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as X
import Data.Word (Word8)

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

black :: Color Color8
black = Color Black

red :: Color Color8
red = Color Red

green :: Color Color8
green = Color Green

yellow :: Color Color8
yellow = Color Yellow

blue :: Color Color8
blue = Color Blue

magenta :: Color Color8
magenta = Color Magenta

cyan :: Color Color8
cyan = Color Cyan

white :: Color Color8
white = Color White

color256 :: Int -> Color Color256
color256 = Color . c256

defaultColor :: Color a
defaultColor = Default

data Color256 = Color256 Word8
                deriving Show

c256 :: Int -> Color256
c256 w =
  if w < 0 || w > 255
  then error "color number out of range"
  else Color256 (fromIntegral w)

data Color a = Default | Color a

data Style a =
  Style { foreground :: Color a
        , background :: Color a
        , bold :: Switch Bold
        , underline :: Switch Underline
        , flash :: Switch Flash
        , invisible :: Switch Invisible
        , inverse :: Switch Inverse }

data TextSpec =
  TextSpec { style8 :: Style Color8
           , style256 :: Style Color256 }

data Bold = Bold
data Underline = Underline
data Flash = Flash
data Inverse = Inverse
data Invisible = Invisible

data Switch a = Off a | On a

newtype Width = Width { unWidth :: Int }
                deriving (Show, Eq, Ord)

instance Monoid Width where
  mempty = Width 0
  mappend (Width w1) (Width w2) = Width $ w1 + w2

chunkSize :: Chunk -> Width
chunkSize (Chunk cs) = F.foldr f (Width 0) cs where
  f (Bit _ x) (Width t) = Width $ X.length x + t

single :: Bit -> Chunk
single = Chunk . S.singleton

chunk :: TextSpec -> Text -> Chunk
chunk ts = single . Bit ts

defaultStyle :: Style a
defaultStyle = Style { foreground = Default
                     , background = Default
                     , bold = Off Bold
                     , underline = Off Underline
                     , flash = Off Flash
                     , invisible = Off Invisible
                     , inverse = Off Inverse }

defaultSpec :: TextSpec
defaultSpec = TextSpec { style8 = defaultStyle
                       , style256 = defaultStyle }

