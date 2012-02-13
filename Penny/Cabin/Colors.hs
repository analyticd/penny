module Penny.Cabin.Colors (
  Colors(Colors0, Colors8, Colors256),
  Chunk,
  Intensity(Normal, Bold),
  ColorName(Black, Red, Green, Yellow, Blue, Magenta,
            Cyan, White, Default),
  Color8(Color8),
  Color256,
  ColorSpec(ColorSpec),
  Width(Width, unWidth),
  chunkSize,
  text) where

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

data Bit = Control ColorSpec
         | Payload Text
         deriving Show

data Chunk = Chunk (Seq Bit)

instance Monoid Chunk where
  mempty = Chunk S.empty
  mappend (Chunk c1) (Chunk c2) = Chunk (c1 `mappend` c2)

data Intensity = Normal | Bold
               deriving Show

data ColorName =
  Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  deriving Show

data Color8 = Color8 ColorName Intensity
              deriving Show

data Color256 = Color256 Word8
                deriving Show

data ColorSpec = ColorSpec Color8 Color256
                 deriving Show

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
  f b t = case b of
    (Payload x) -> (unsafeFromInt . X.length $ x) `add` t
    _ -> t

single :: Bit -> Chunk
single = Chunk . S.singleton

text :: Color8 -> Color256 -> Text -> Chunk
text c8 c256 t = single cspec `mappend` single tspec where
  cspec = Control (ColorSpec c8 c256)
  tspec = Payload t
