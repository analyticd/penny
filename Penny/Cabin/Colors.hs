module Penny.Cabin.Colors where

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

data Color256 = Color256 { unColor256 :: Word8 }
                deriving Show

data ColorSpec = ColorSpec Color8 Color256
                 deriving Show

chunkSize :: Chunk -> Int
chunkSize (Chunk cs) = F.foldr f 0 cs where
  f b t = case b of
    (Payload x) -> X.length x + t
    _ -> t

single :: Bit -> Chunk
single = Chunk . S.singleton

text :: Color8 -> Color256 -> Text -> Chunk
text c8 c256 t = single cspec `mappend` single tspec where
  cspec = Control (ColorSpec c8 c256)
  tspec = Payload t
