module Penny.Cabin.Chunk (
  -- * Colors
  Colors(Colors0, Colors8, Colors256),
  ColorPref(Pref0, Pref8, Pref256, PrefAuto),
  Color,
  Color8,
  Color256,
  Foreground(Foreground, unForeground),
  Background(Background, unBackground),
  black, red, green, yellow, blue, magenta, cyan, white,
  color256,
  defaultColor,
  autoColors,

  -- * Chunks
  Chunk,
  chunk,
  emptyChunk,
  chunkSize,
  Width(Width, unWidth),
  printChunk,

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
import qualified Data.Text.Lazy.IO as TIO
import qualified Data.Text.Lazy.Builder as TB
import Data.Word (Word8)
import System.Environment (getEnvironment)
import System.IO (hIsTerminalDevice, stdout)

-- | The user's color preference.
data ColorPref = Pref0 | Pref8 | Pref256 | PrefAuto
               deriving Show

-- | How many colors to actually show.
data Colors = Colors0 | Colors8 | Colors256
            deriving Show

data Bit = Bit TextSpec Text

data Chunk = Chunk (Seq Bit)

emptyChunk :: Chunk
emptyChunk = Chunk S.empty

instance Monoid Chunk where
  mempty = emptyChunk
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

instance HasForegroundCode Color8 where
  foregroundCode a = case a of
    Black -> code "30"
    Red -> code "31"
    Green -> code "32"
    Yellow -> code "33"
    Blue -> code "34"
    Magenta -> code "35"
    Cyan -> code "36"
    White -> code "37"

instance HasBackgroundCode Color8 where
  backgroundCode a = case a of
    Black -> code "40"
    Red -> code "41"
    Green -> code "42"
    Yellow -> code "43"
    Blue -> code "44"
    Magenta -> code "45"
    Cyan -> code "46"
    White -> code "47"

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

instance HasForegroundCode Color256 where
  foregroundCode (Color256 w) =
    code ("38;5;" ++ show w)

instance HasBackgroundCode Color256 where
  backgroundCode (Color256 w) =
    code ("48;5;" ++ show w)

c256 :: Int -> Color256
c256 w =
  if w < 0 || w > 255
  then error "color number out of range"
  else Color256 (fromIntegral w)

data Color a = Default | Color a

newtype Foreground a = Foreground { unForeground :: Color a }
newtype Background a = Background { unBackground :: Color a }

colorForeground :: HasForegroundCode a => Foreground a -> Maybe Code
colorForeground (Foreground c) = case c of
  Default -> Nothing
  Color col -> Just . foregroundCode $ col

colorBackground :: HasBackgroundCode a => Background a -> Maybe Code
colorBackground (Background c) = case c of
  Default -> Nothing
  Color col -> Just . backgroundCode $ col

class HasForegroundCode c where
  foregroundCode :: c -> Code

class HasBackgroundCode c where
  backgroundCode :: c -> Code

data Style a =
  Style { foreground :: Foreground a
        , background :: Background a
        , bold :: Switch Bold
        , underline :: Switch Underline
        , flash :: Switch Flash
        , invisible :: Switch Invisible
        , inverse :: Switch Inverse }

(.++.) :: TB.Builder -> TB.Builder -> TB.Builder
(.++.) = mappend
infixr 5 .++.

styleCodes :: (HasForegroundCode a, HasBackgroundCode a)
              => Style a -> TB.Builder
styleCodes s =
  controlCode (onCode Reset)
  .++. printMaybeCode (colorForeground (foreground s))
  .++. printMaybeCode (colorBackground (background s))
  .++. printCodeIfOn (bold s)
  .++. printCodeIfOn (underline s)
  .++. printCodeIfOn (flash s)
  .++. printCodeIfOn (invisible s)
  .++. printCodeIfOn (inverse s)

printCodeIfOn :: HasOnCode a => Switch a -> TB.Builder
printCodeIfOn s = case s of
  Off _ -> mempty
  On a -> controlCode . onCode $ a

printMaybeCode :: Maybe Code -> TB.Builder
printMaybeCode c = case c of
  Nothing -> mempty
  Just cd -> controlCode cd

data TextSpec =
  TextSpec { style8 :: Style Color8
           , style256 :: Style Color256 }

textSpecCode :: Colors -> TextSpec -> TB.Builder
textSpecCode c ts = case c of
  Colors0 -> mempty
  Colors8 -> styleCodes . style8 $ ts
  Colors256 -> styleCodes . style256 $ ts

{-
printBit :: Colors -> Bit -> IO ()
printBit c (Bit ts t) =
  textSpecCode c ts
  >> TIO.putStr t
-}
builder :: Colors -> Bit -> TB.Builder -> TB.Builder
builder c (Bit ts t) acc =
  textSpecCode c ts 
  .++. TB.fromText t
  .++. acc

printChunk :: ColorPref -> Chunk -> IO ()
printChunk cp (Chunk cs) = do
  c <- case cp of
        Pref0 -> return Colors0
        Pref8 -> return Colors8
        Pref256 -> return Colors256
        PrefAuto -> autoColors
  let z = printReset c
      bldr = F.foldr (builder c) z cs
  TIO.putStr (TB.toLazyText bldr)
  --T.traverse (printBit c) cs *> printReset c

autoColors :: IO Colors
autoColors = do
  isTerm <- hIsTerminalDevice stdout
  if isTerm
    then do
    env <- getEnvironment
    case lookup "TERM" env of
      Nothing -> return Colors8
      (Just t) -> if t == "xterm-256color"
                  then return Colors256
                  else return Colors8
    else return Colors0

class HasOnCode a where
  onCode :: a -> Code

data Bold = Bold
data Underline = Underline
data Flash = Flash
data Inverse = Inverse
data Invisible = Invisible
data Reset = Reset
newtype Code = Code { unCode :: TB.Builder }

code :: String -> Code
code s = Code $ csi .++. TB.fromString s .++. TB.singleton 'm'

instance HasOnCode Bold where onCode _ = code "1"
instance HasOnCode Underline where onCode _ = code "4"
instance HasOnCode Flash where onCode _ = code "5"
instance HasOnCode Inverse where onCode _ = code "7"
instance HasOnCode Invisible where onCode _ = code "8"
instance HasOnCode Reset where onCode _ = code "0"

data Switch a = Off a | On a

printReset :: Colors -> TB.Builder
printReset c = case c of
  Colors0 -> mempty
  _ -> controlCode (onCode Reset)

csi :: TB.Builder
csi = TB.fromString (toEnum 27:'[':[])

controlCode :: Code -> TB.Builder
controlCode = unCode

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
defaultStyle = Style { foreground = Foreground Default
                     , background = Background Default
                     , bold = Off Bold
                     , underline = Off Underline
                     , flash = Off Flash
                     , invisible = Off Invisible
                     , inverse = Off Inverse }

defaultSpec :: TextSpec
defaultSpec = TextSpec { style8 = defaultStyle
                       , style256 = defaultStyle }

