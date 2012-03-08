module Penny.Cabin.Colors (
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
  

import Control.Applicative (pure, (*>))
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid, mempty, mappend)
import qualified Data.Foldable as F
import Data.List (intersperse)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.IO as TIO
import qualified Data.Traversable as T
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
    Black -> Code "30"
    Red -> Code "31"
    Green -> Code "32"
    Yellow -> Code "33"
    Blue -> Code "34"
    Magenta -> Code "35"
    Cyan -> Code "36"
    White -> Code "37"

instance HasBackgroundCode Color8 where
  backgroundCode a = case a of
    Black -> Code "40"
    Red -> Code "41"
    Green -> Code "42"
    Yellow -> Code "43"
    Blue -> Code "44"
    Magenta -> Code "45"
    Cyan -> Code "46"
    White -> Code "47"

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
  foregroundCode (Color256 w) = Code $ "38;5;" ++ show w

instance HasBackgroundCode Color256 where
  backgroundCode (Color256 w) = Code $ "48;5;" ++ show w

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

--instance HasForegroundCode Color
data Style a =
  Style { foreground :: Foreground a
        , background :: Background a
        , bold :: Switch Bold
        , underline :: Switch Underline
        , flash :: Switch Flash
        , invisible :: Switch Invisible
        , inverse :: Switch Inverse }


styleCodes :: (HasForegroundCode a, HasBackgroundCode a)
              => Style a -> String
styleCodes s = controlSequence ls where
  ls = catMaybes $
       [Just $ onCode Reset,
        colorForeground (foreground s),
        colorBackground (background s),
        codeIfOn (bold s),
        codeIfOn (underline s),
        codeIfOn (flash s),
        codeIfOn (invisible s),
        codeIfOn (inverse s)]

data TextSpec =
  TextSpec { style8 :: Style Color8
           , style256 :: Style Color256 }

textSpecCode :: Colors -> TextSpec -> String
textSpecCode c ts = case c of
  Colors0 -> ""
  Colors8 -> styleCodes . style8 $ ts
  Colors256 -> styleCodes . style256 $ ts

printBit :: Colors -> Bit -> IO ()
printBit c (Bit ts t) = let
  tsStr = textSpecCode c ts
  in putStr tsStr >> TIO.putStr t

printChunk :: ColorPref -> Chunk -> IO ()
printChunk cp (Chunk cs) = do
  c <- case cp of
        Pref0 -> return Colors0
        Pref8 -> return Colors8
        Pref256 -> return Colors256
        PrefAuto -> autoColors
  T.traverse (printBit c) cs *> printReset c

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
newtype Code = Code { unCode :: String }

instance HasOnCode Bold where onCode _ = Code "1"
instance HasOnCode Underline where onCode _ = Code "4"
instance HasOnCode Flash where onCode _ = Code "5"
instance HasOnCode Inverse where onCode _ = Code "7"
instance HasOnCode Invisible where onCode _ = Code "8"
instance HasOnCode Reset where onCode _ = Code "0"

data Switch a = Off a | On a

printReset :: Colors -> IO ()
printReset c = case c of
  Colors0 -> pure ()
  _ -> putStr . controlSequence $ [onCode Reset]

codeIfOn :: HasOnCode a => Switch a -> Maybe Code
codeIfOn s = case s of
  Off _ -> Nothing
  On a -> Just . onCode $ a

csi :: String
csi = toEnum 27:'[':[]

controlSequence :: [Code] -> String
controlSequence [] = ""
controlSequence xs = csi ++ s ++ "m" where
  s = concat . intersperse ";" . map unCode $ xs

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

