-- | Handles colors and special effects for text. This module was
-- written using the control sequences documented for xterm in
-- ctlseqs.txt, which is included in the xterm source code (on Debian
-- GNU/Linux systems, it is at
-- \/usr\/share\/doc\/xterm\/ctlseqs.txt.gz). The code in here should also
-- work for any terminal which recognizes ISO 6429 escape sequences
-- (also known as ANSI escape sequences), but only for 8 colors. This
-- module also generates sequences for 256 color xterms; though this
-- works fine with xterm, it might not work on other terminals (I
-- believe it works for other terminals commonly found on Linux
-- systems, such as gnome-terminal and Konsole, but I have not tested
-- these terminals as I do not use them.) Perhaps it also works on
-- Windows or Mac OS X systems and their terminals, though I have not
-- tested them (in particular, Windows could be problematic as not all
-- Windows terminals support ISO 6429.)
--
-- In theory there are more portable ways to generate color codes,
-- such as through curses (or is it ncurses?) but support for ISO 6429
-- is widespread enough that I am prepared to say that if your
-- terminal does not support it, too bad; just use the colorless mode.
module Penny.Cabin.Chunk (
  -- * Colors
  Colors(Colors0, Colors8, Colors256),
  maxCapableColors,
  Background8,
  Background256,
  Foreground8,
  Foreground256,

  -- * Chunks
  Chunk,
  chunk,
  Width(Width, unWidth),
  chunkWidth,
  chunksToText,

  -- * Effects
  Bold(Bold, unBold),
  Underline(Underline, unUnderline),
  Flash(Flash, unFlash),
  Inverse(Inverse, unInverse),

  -- * Style and TextSpec

  -- | A style is a bundle of attributes that describes text
  -- attributes, such as its color and whether it is bold.
  StyleCommon (StyleCommon, bold, underline, flash, inverse),
  Style8 (Style8, foreground8, background8, common8),
  Style256 (Style256, foreground256, background256, common256),
  defaultStyleCommon,
  defaultStyle8,
  defaultStyle256,

  TextSpec (TextSpec, style8, style256),
  defaultTextSpec,

  -- * Specific colors
  -- * 8 color foreground colors
  color8_f_default,
  color8_f_black,
  color8_f_red,
  color8_f_green,
  color8_f_yellow,
  color8_f_blue,
  color8_f_magenta,
  color8_f_cyan,
  color8_f_white,

  -- ** 8 color background colors
  color8_b_default,
  color8_b_black,
  color8_b_red,
  color8_b_green,
  color8_b_yellow,
  color8_b_blue,
  color8_b_magenta,
  color8_b_cyan,
  color8_b_white,

  -- * 256 color foreground colors
  color256_f_default,
  color256_f_0,
  color256_f_1,
  color256_f_2,
  color256_f_3,
  color256_f_4,
  color256_f_5,
  color256_f_6,
  color256_f_7,
  color256_f_8,
  color256_f_9,
  color256_f_10,
  color256_f_11,
  color256_f_12,
  color256_f_13,
  color256_f_14,
  color256_f_15,
  color256_f_16,
  color256_f_17,
  color256_f_18,
  color256_f_19,
  color256_f_20,
  color256_f_21,
  color256_f_22,
  color256_f_23,
  color256_f_24,
  color256_f_25,
  color256_f_26,
  color256_f_27,
  color256_f_28,
  color256_f_29,
  color256_f_30,
  color256_f_31,
  color256_f_32,
  color256_f_33,
  color256_f_34,
  color256_f_35,
  color256_f_36,
  color256_f_37,
  color256_f_38,
  color256_f_39,
  color256_f_40,
  color256_f_41,
  color256_f_42,
  color256_f_43,
  color256_f_44,
  color256_f_45,
  color256_f_46,
  color256_f_47,
  color256_f_48,
  color256_f_49,
  color256_f_50,
  color256_f_51,
  color256_f_52,
  color256_f_53,
  color256_f_54,
  color256_f_55,
  color256_f_56,
  color256_f_57,
  color256_f_58,
  color256_f_59,
  color256_f_60,
  color256_f_61,
  color256_f_62,
  color256_f_63,
  color256_f_64,
  color256_f_65,
  color256_f_66,
  color256_f_67,
  color256_f_68,
  color256_f_69,
  color256_f_70,
  color256_f_71,
  color256_f_72,
  color256_f_73,
  color256_f_74,
  color256_f_75,
  color256_f_76,
  color256_f_77,
  color256_f_78,
  color256_f_79,
  color256_f_80,
  color256_f_81,
  color256_f_82,
  color256_f_83,
  color256_f_84,
  color256_f_85,
  color256_f_86,
  color256_f_87,
  color256_f_88,
  color256_f_89,
  color256_f_90,
  color256_f_91,
  color256_f_92,
  color256_f_93,
  color256_f_94,
  color256_f_95,
  color256_f_96,
  color256_f_97,
  color256_f_98,
  color256_f_99,
  color256_f_100,
  color256_f_101,
  color256_f_102,
  color256_f_103,
  color256_f_104,
  color256_f_105,
  color256_f_106,
  color256_f_107,
  color256_f_108,
  color256_f_109,
  color256_f_110,
  color256_f_111,
  color256_f_112,
  color256_f_113,
  color256_f_114,
  color256_f_115,
  color256_f_116,
  color256_f_117,
  color256_f_118,
  color256_f_119,
  color256_f_120,
  color256_f_121,
  color256_f_122,
  color256_f_123,
  color256_f_124,
  color256_f_125,
  color256_f_126,
  color256_f_127,
  color256_f_128,
  color256_f_129,
  color256_f_130,
  color256_f_131,
  color256_f_132,
  color256_f_133,
  color256_f_134,
  color256_f_135,
  color256_f_136,
  color256_f_137,
  color256_f_138,
  color256_f_139,
  color256_f_140,
  color256_f_141,
  color256_f_142,
  color256_f_143,
  color256_f_144,
  color256_f_145,
  color256_f_146,
  color256_f_147,
  color256_f_148,
  color256_f_149,
  color256_f_150,
  color256_f_151,
  color256_f_152,
  color256_f_153,
  color256_f_154,
  color256_f_155,
  color256_f_156,
  color256_f_157,
  color256_f_158,
  color256_f_159,
  color256_f_160,
  color256_f_161,
  color256_f_162,
  color256_f_163,
  color256_f_164,
  color256_f_165,
  color256_f_166,
  color256_f_167,
  color256_f_168,
  color256_f_169,
  color256_f_170,
  color256_f_171,
  color256_f_172,
  color256_f_173,
  color256_f_174,
  color256_f_175,
  color256_f_176,
  color256_f_177,
  color256_f_178,
  color256_f_179,
  color256_f_180,
  color256_f_181,
  color256_f_182,
  color256_f_183,
  color256_f_184,
  color256_f_185,
  color256_f_186,
  color256_f_187,
  color256_f_188,
  color256_f_189,
  color256_f_190,
  color256_f_191,
  color256_f_192,
  color256_f_193,
  color256_f_194,
  color256_f_195,
  color256_f_196,
  color256_f_197,
  color256_f_198,
  color256_f_199,
  color256_f_200,
  color256_f_201,
  color256_f_202,
  color256_f_203,
  color256_f_204,
  color256_f_205,
  color256_f_206,
  color256_f_207,
  color256_f_208,
  color256_f_209,
  color256_f_210,
  color256_f_211,
  color256_f_212,
  color256_f_213,
  color256_f_214,
  color256_f_215,
  color256_f_216,
  color256_f_217,
  color256_f_218,
  color256_f_219,
  color256_f_220,
  color256_f_221,
  color256_f_222,
  color256_f_223,
  color256_f_224,
  color256_f_225,
  color256_f_226,
  color256_f_227,
  color256_f_228,
  color256_f_229,
  color256_f_230,
  color256_f_231,
  color256_f_232,
  color256_f_233,
  color256_f_234,
  color256_f_235,
  color256_f_236,
  color256_f_237,
  color256_f_238,
  color256_f_239,
  color256_f_240,
  color256_f_241,
  color256_f_242,
  color256_f_243,
  color256_f_244,
  color256_f_245,
  color256_f_246,
  color256_f_247,
  color256_f_248,
  color256_f_249,
  color256_f_250,
  color256_f_251,
  color256_f_252,
  color256_f_253,
  color256_f_254,
  color256_f_255,

  -- ** 256 color background colors
  color256_b_default,
  color256_b_0,
  color256_b_1,
  color256_b_2,
  color256_b_3,
  color256_b_4,
  color256_b_5,
  color256_b_6,
  color256_b_7,
  color256_b_8,
  color256_b_9,
  color256_b_10,
  color256_b_11,
  color256_b_12,
  color256_b_13,
  color256_b_14,
  color256_b_15,
  color256_b_16,
  color256_b_17,
  color256_b_18,
  color256_b_19,
  color256_b_20,
  color256_b_21,
  color256_b_22,
  color256_b_23,
  color256_b_24,
  color256_b_25,
  color256_b_26,
  color256_b_27,
  color256_b_28,
  color256_b_29,
  color256_b_30,
  color256_b_31,
  color256_b_32,
  color256_b_33,
  color256_b_34,
  color256_b_35,
  color256_b_36,
  color256_b_37,
  color256_b_38,
  color256_b_39,
  color256_b_40,
  color256_b_41,
  color256_b_42,
  color256_b_43,
  color256_b_44,
  color256_b_45,
  color256_b_46,
  color256_b_47,
  color256_b_48,
  color256_b_49,
  color256_b_50,
  color256_b_51,
  color256_b_52,
  color256_b_53,
  color256_b_54,
  color256_b_55,
  color256_b_56,
  color256_b_57,
  color256_b_58,
  color256_b_59,
  color256_b_60,
  color256_b_61,
  color256_b_62,
  color256_b_63,
  color256_b_64,
  color256_b_65,
  color256_b_66,
  color256_b_67,
  color256_b_68,
  color256_b_69,
  color256_b_70,
  color256_b_71,
  color256_b_72,
  color256_b_73,
  color256_b_74,
  color256_b_75,
  color256_b_76,
  color256_b_77,
  color256_b_78,
  color256_b_79,
  color256_b_80,
  color256_b_81,
  color256_b_82,
  color256_b_83,
  color256_b_84,
  color256_b_85,
  color256_b_86,
  color256_b_87,
  color256_b_88,
  color256_b_89,
  color256_b_90,
  color256_b_91,
  color256_b_92,
  color256_b_93,
  color256_b_94,
  color256_b_95,
  color256_b_96,
  color256_b_97,
  color256_b_98,
  color256_b_99,
  color256_b_100,
  color256_b_101,
  color256_b_102,
  color256_b_103,
  color256_b_104,
  color256_b_105,
  color256_b_106,
  color256_b_107,
  color256_b_108,
  color256_b_109,
  color256_b_110,
  color256_b_111,
  color256_b_112,
  color256_b_113,
  color256_b_114,
  color256_b_115,
  color256_b_116,
  color256_b_117,
  color256_b_118,
  color256_b_119,
  color256_b_120,
  color256_b_121,
  color256_b_122,
  color256_b_123,
  color256_b_124,
  color256_b_125,
  color256_b_126,
  color256_b_127,
  color256_b_128,
  color256_b_129,
  color256_b_130,
  color256_b_131,
  color256_b_132,
  color256_b_133,
  color256_b_134,
  color256_b_135,
  color256_b_136,
  color256_b_137,
  color256_b_138,
  color256_b_139,
  color256_b_140,
  color256_b_141,
  color256_b_142,
  color256_b_143,
  color256_b_144,
  color256_b_145,
  color256_b_146,
  color256_b_147,
  color256_b_148,
  color256_b_149,
  color256_b_150,
  color256_b_151,
  color256_b_152,
  color256_b_153,
  color256_b_154,
  color256_b_155,
  color256_b_156,
  color256_b_157,
  color256_b_158,
  color256_b_159,
  color256_b_160,
  color256_b_161,
  color256_b_162,
  color256_b_163,
  color256_b_164,
  color256_b_165,
  color256_b_166,
  color256_b_167,
  color256_b_168,
  color256_b_169,
  color256_b_170,
  color256_b_171,
  color256_b_172,
  color256_b_173,
  color256_b_174,
  color256_b_175,
  color256_b_176,
  color256_b_177,
  color256_b_178,
  color256_b_179,
  color256_b_180,
  color256_b_181,
  color256_b_182,
  color256_b_183,
  color256_b_184,
  color256_b_185,
  color256_b_186,
  color256_b_187,
  color256_b_188,
  color256_b_189,
  color256_b_190,
  color256_b_191,
  color256_b_192,
  color256_b_193,
  color256_b_194,
  color256_b_195,
  color256_b_196,
  color256_b_197,
  color256_b_198,
  color256_b_199,
  color256_b_200,
  color256_b_201,
  color256_b_202,
  color256_b_203,
  color256_b_204,
  color256_b_205,
  color256_b_206,
  color256_b_207,
  color256_b_208,
  color256_b_209,
  color256_b_210,
  color256_b_211,
  color256_b_212,
  color256_b_213,
  color256_b_214,
  color256_b_215,
  color256_b_216,
  color256_b_217,
  color256_b_218,
  color256_b_219,
  color256_b_220,
  color256_b_221,
  color256_b_222,
  color256_b_223,
  color256_b_224,
  color256_b_225,
  color256_b_226,
  color256_b_227,
  color256_b_228,
  color256_b_229,
  color256_b_230,
  color256_b_231,
  color256_b_232,
  color256_b_233,
  color256_b_234,
  color256_b_235,
  color256_b_236,
  color256_b_237,
  color256_b_238,
  color256_b_239,
  color256_b_240,
  color256_b_241,
  color256_b_242,
  color256_b_243,
  color256_b_244,
  color256_b_245,
  color256_b_246,
  color256_b_247,
  color256_b_248,
  color256_b_249,
  color256_b_250,
  color256_b_251,
  color256_b_252,
  color256_b_253,
  color256_b_254,
  color256_b_255

  ) where


import Data.Monoid (Monoid, mempty, mappend)
import Data.Text (Text)
import qualified Data.Text as X
import qualified Data.Text.Lazy as XL
import qualified Data.Text.Lazy.Builder as TB
import qualified Penny.Shield as S

--
-- Colors
--

-- | How many colors to actually show.
data Colors = Colors0 | Colors8 | Colors256
            deriving Show

-- | The maximum number of colors that can be displayed. If not a TTY,
-- no colors. Otherwise, Examines TERM. If it is @xterm-256color@,
-- then 256 colors; otherwise, assumes 8 colors are available.
maxCapableColors :: S.Runtime -> Colors
maxCapableColors r = case S.output r of
  S.NotTTY -> Colors0
  S.IsTTY ->
    case lookup "TERM" (S.environment r) of
      Nothing -> Colors8
      (Just t) -> if t == "xterm-256color"
                  then Colors256
                  else Colors8

-- | Background color in an 8 color setting.
newtype Background8 = Background8 { unBackground8 :: Code }

-- | Background color in a 256 color setting.
newtype Background256 = Background256 { unBackground256 :: Code }

-- | Foreground color in an 8 color setting.
newtype Foreground8 = Foreground8 { unForeground8 :: Code }

-- | Foreground color in a 256 color setting.
newtype Foreground256 = Foreground256 { unForeground256 :: Code }


--
-- Chunks
--

-- | A chunk is some textual data coupled with a description of what
-- color the text is. The chunk knows what colors to use for both
-- foreground color and background color, in both an 8 color terminal
-- and a 256 color terminal. The chunk has only one set of color
-- descriptions. To change colors, you must use a new chunk.
--
-- There is no way to combine chunks. To print large numbers of
-- chunks, lazily build a list of them and then print them using
-- chunksToText.
data Chunk = Chunk TextSpec Text

chunk :: TextSpec -> Text -> Chunk
chunk = Chunk

-- | How wide the text of a chunk is.
newtype Width = Width { unWidth :: Int }
                deriving (Show, Eq, Ord)

instance Monoid Width where
  mempty = Width 0
  mappend (Width w1) (Width w2) = Width $ w1 + w2

chunkWidth :: Chunk -> Width
chunkWidth (Chunk _ t) = Width . X.length $ t

-- | Transforms chunks to a lazy Text. This function runs lazily and
-- in constant time and space.
chunksToText :: Colors -> [Chunk] -> XL.Text
chunksToText c = TB.toLazyText . foldr (builder c) (printReset c)


--
-- Effects
--
newtype Bold = Bold { unBold :: Bool }
newtype Underline = Underline { unUnderline :: Bool }
newtype Flash = Flash { unFlash :: Bool }
newtype Inverse = Inverse { unInverse :: Bool }

--
-- Styles
--

-- | Style elements that apply in both 8 and 256 color
-- terminals. However, the elements are described separately for 8 and
-- 256 color terminals, so that the text appearance can change
-- depending on how many colors a terminal has.
data StyleCommon = StyleCommon {
  bold :: Bold
  , underline :: Underline
  , flash :: Flash
  , inverse :: Inverse }

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for an 8 color terminal.
data Style8 = Style8 {
  foreground8 :: Foreground8
  , background8 :: Background8
  , common8 :: StyleCommon }

-- | Describes text appearance (foreground and background colors, as
-- well as other attributes such as bold) for a 256 color terminal.
data Style256 = Style256 {
  foreground256 :: Foreground256
  , background256 :: Background256
  , common256 :: StyleCommon }

-- | Has all bold, flash, underline, and inverse turned off.
defaultStyleCommon :: StyleCommon
defaultStyleCommon = StyleCommon {
  bold = Bold False
  , underline = Underline False
  , flash = Flash False
  , inverse = Inverse False }

-- | Uses the default terminal colors (which will vary depending on
-- the terminal).
defaultStyle8 :: Style8
defaultStyle8 = Style8 {
  foreground8 = color8_f_default
  , background8 = color8_b_default
  , common8 = defaultStyleCommon }

-- | Uses the default terminal colors (which will vary depending on
-- the terminal).
defaultStyle256 :: Style256
defaultStyle256 = Style256 {
  foreground256 = color256_f_default
  , background256 = color256_b_default
  , common256 = defaultStyleCommon }

--
-- TextSpec
--

-- | The TextSpec bundles together the styles for the 8 and 256 color
-- terminals, so that the text can be portrayed on any terminal.
data TextSpec = TextSpec {
  style8 :: Style8
  , style256 :: Style256 }

-- | A TextSpec with the default colors on 8 and 256 color terminals,
-- with all attributes turned off.
defaultTextSpec :: TextSpec
defaultTextSpec = TextSpec {
  style8 = defaultStyle8
  , style256 = defaultStyle256 }

--
-- Internal
--

(+++) :: TB.Builder -> TB.Builder -> TB.Builder
(+++) = mappend
infixr 5 +++

builder :: Colors -> Chunk -> TB.Builder -> TB.Builder
builder c (Chunk ts t) acc =
  printReset c
  +++ textSpecCodes c ts 
  +++ TB.fromText t
  +++ acc

textSpecCodes :: Colors -> TextSpec -> TB.Builder
textSpecCodes c s = case c of
  Colors0 -> mempty
  Colors8 -> style8Colors $ style8 s
  Colors256 -> style256Colors $ style256 s

style8Colors :: Style8 -> TB.Builder
style8Colors s =
  (unCode . unForeground8 . foreground8 $ s)
  +++ (unCode . unBackground8 . background8 $ s)
  +++ (common (common8 s))

style256Colors :: Style256 -> TB.Builder
style256Colors s =
  (unCode . unForeground256 . foreground256 $ s)
  +++ (unCode . unBackground256 . background256 $ s)
  +++ (common (common256 s))

common :: StyleCommon -> TB.Builder
common s =
  (if unBold . bold $ s then unCode boldOn else mempty)
  +++ (if unUnderline . underline $ s
       then unCode underlineOn else mempty)
  +++ (if unFlash . flash $ s
       then unCode flashOn else mempty)
  +++ (if unInverse . inverse $ s
       then unCode inverseOn else mempty)

newtype Code = Code { unCode :: TB.Builder }

code :: String -> Code
code s = Code $
         TB.fromString (toEnum 27:'[':[])
         +++ TB.fromString s
         +++ TB.singleton 'm'

boldOn :: Code
boldOn = code "1"

underlineOn :: Code
underlineOn = code "4"

flashOn :: Code
flashOn = code "5"

inverseOn :: Code
inverseOn = code "7"

resetOn :: Code
resetOn = code "0"

printReset :: Colors -> TB.Builder
printReset c = case c of
  Colors0 -> mempty
  _ -> unCode resetOn

--
-- Color basement
--
color8_f_default :: Foreground8
color8_f_default = Foreground8 . Code $ mempty

color8_f_black :: Foreground8
color8_f_black = Foreground8 . code $ "30"

color8_f_red :: Foreground8
color8_f_red = Foreground8 . code $ "31"

color8_f_green :: Foreground8
color8_f_green = Foreground8 . code $ "32"

color8_f_yellow :: Foreground8
color8_f_yellow = Foreground8 . code $ "33"

color8_f_blue :: Foreground8
color8_f_blue = Foreground8 . code $ "34"

color8_f_magenta :: Foreground8
color8_f_magenta = Foreground8 . code $ "35"

color8_f_cyan :: Foreground8
color8_f_cyan = Foreground8 . code $ "36"

color8_f_white :: Foreground8
color8_f_white = Foreground8 . code $ "37"

color8_b_default :: Background8
color8_b_default = Background8 . Code $ mempty

color8_b_black :: Background8
color8_b_black = Background8 . code $ "40"

color8_b_red :: Background8
color8_b_red = Background8 . code $ "41"

color8_b_green :: Background8
color8_b_green = Background8 . code $ "42"

color8_b_yellow :: Background8
color8_b_yellow = Background8 . code $ "43"

color8_b_blue :: Background8
color8_b_blue = Background8 . code $ "44"

color8_b_magenta :: Background8
color8_b_magenta = Background8 . code $ "45"

color8_b_cyan :: Background8
color8_b_cyan = Background8 . code $ "46"

color8_b_white :: Background8
color8_b_white = Background8 . code $ "47"

color256_f_default :: Foreground256
color256_f_default = Foreground256 . Code $ mempty

color256_f_0 :: Foreground256
color256_f_0 = Foreground256 . code $ "38;5;0"

color256_f_1 :: Foreground256
color256_f_1 = Foreground256 . code $ "38;5;1"

color256_f_2 :: Foreground256
color256_f_2 = Foreground256 . code $ "38;5;2"

color256_f_3 :: Foreground256
color256_f_3 = Foreground256 . code $ "38;5;3"

color256_f_4 :: Foreground256
color256_f_4 = Foreground256 . code $ "38;5;4"

color256_f_5 :: Foreground256
color256_f_5 = Foreground256 . code $ "38;5;5"

color256_f_6 :: Foreground256
color256_f_6 = Foreground256 . code $ "38;5;6"

color256_f_7 :: Foreground256
color256_f_7 = Foreground256 . code $ "38;5;7"

color256_f_8 :: Foreground256
color256_f_8 = Foreground256 . code $ "38;5;8"

color256_f_9 :: Foreground256
color256_f_9 = Foreground256 . code $ "38;5;9"

color256_f_10 :: Foreground256
color256_f_10 = Foreground256 . code $ "38;5;10"

color256_f_11 :: Foreground256
color256_f_11 = Foreground256 . code $ "38;5;11"

color256_f_12 :: Foreground256
color256_f_12 = Foreground256 . code $ "38;5;12"

color256_f_13 :: Foreground256
color256_f_13 = Foreground256 . code $ "38;5;13"

color256_f_14 :: Foreground256
color256_f_14 = Foreground256 . code $ "38;5;14"

color256_f_15 :: Foreground256
color256_f_15 = Foreground256 . code $ "38;5;15"

color256_f_16 :: Foreground256
color256_f_16 = Foreground256 . code $ "38;5;16"

color256_f_17 :: Foreground256
color256_f_17 = Foreground256 . code $ "38;5;17"

color256_f_18 :: Foreground256
color256_f_18 = Foreground256 . code $ "38;5;18"

color256_f_19 :: Foreground256
color256_f_19 = Foreground256 . code $ "38;5;19"

color256_f_20 :: Foreground256
color256_f_20 = Foreground256 . code $ "38;5;20"

color256_f_21 :: Foreground256
color256_f_21 = Foreground256 . code $ "38;5;21"

color256_f_22 :: Foreground256
color256_f_22 = Foreground256 . code $ "38;5;22"

color256_f_23 :: Foreground256
color256_f_23 = Foreground256 . code $ "38;5;23"

color256_f_24 :: Foreground256
color256_f_24 = Foreground256 . code $ "38;5;24"

color256_f_25 :: Foreground256
color256_f_25 = Foreground256 . code $ "38;5;25"

color256_f_26 :: Foreground256
color256_f_26 = Foreground256 . code $ "38;5;26"

color256_f_27 :: Foreground256
color256_f_27 = Foreground256 . code $ "38;5;27"

color256_f_28 :: Foreground256
color256_f_28 = Foreground256 . code $ "38;5;28"

color256_f_29 :: Foreground256
color256_f_29 = Foreground256 . code $ "38;5;29"

color256_f_30 :: Foreground256
color256_f_30 = Foreground256 . code $ "38;5;30"

color256_f_31 :: Foreground256
color256_f_31 = Foreground256 . code $ "38;5;31"

color256_f_32 :: Foreground256
color256_f_32 = Foreground256 . code $ "38;5;32"

color256_f_33 :: Foreground256
color256_f_33 = Foreground256 . code $ "38;5;33"

color256_f_34 :: Foreground256
color256_f_34 = Foreground256 . code $ "38;5;34"

color256_f_35 :: Foreground256
color256_f_35 = Foreground256 . code $ "38;5;35"

color256_f_36 :: Foreground256
color256_f_36 = Foreground256 . code $ "38;5;36"

color256_f_37 :: Foreground256
color256_f_37 = Foreground256 . code $ "38;5;37"

color256_f_38 :: Foreground256
color256_f_38 = Foreground256 . code $ "38;5;38"

color256_f_39 :: Foreground256
color256_f_39 = Foreground256 . code $ "38;5;39"

color256_f_40 :: Foreground256
color256_f_40 = Foreground256 . code $ "38;5;40"

color256_f_41 :: Foreground256
color256_f_41 = Foreground256 . code $ "38;5;41"

color256_f_42 :: Foreground256
color256_f_42 = Foreground256 . code $ "38;5;42"

color256_f_43 :: Foreground256
color256_f_43 = Foreground256 . code $ "38;5;43"

color256_f_44 :: Foreground256
color256_f_44 = Foreground256 . code $ "38;5;44"

color256_f_45 :: Foreground256
color256_f_45 = Foreground256 . code $ "38;5;45"

color256_f_46 :: Foreground256
color256_f_46 = Foreground256 . code $ "38;5;46"

color256_f_47 :: Foreground256
color256_f_47 = Foreground256 . code $ "38;5;47"

color256_f_48 :: Foreground256
color256_f_48 = Foreground256 . code $ "38;5;48"

color256_f_49 :: Foreground256
color256_f_49 = Foreground256 . code $ "38;5;49"

color256_f_50 :: Foreground256
color256_f_50 = Foreground256 . code $ "38;5;50"

color256_f_51 :: Foreground256
color256_f_51 = Foreground256 . code $ "38;5;51"

color256_f_52 :: Foreground256
color256_f_52 = Foreground256 . code $ "38;5;52"

color256_f_53 :: Foreground256
color256_f_53 = Foreground256 . code $ "38;5;53"

color256_f_54 :: Foreground256
color256_f_54 = Foreground256 . code $ "38;5;54"

color256_f_55 :: Foreground256
color256_f_55 = Foreground256 . code $ "38;5;55"

color256_f_56 :: Foreground256
color256_f_56 = Foreground256 . code $ "38;5;56"

color256_f_57 :: Foreground256
color256_f_57 = Foreground256 . code $ "38;5;57"

color256_f_58 :: Foreground256
color256_f_58 = Foreground256 . code $ "38;5;58"

color256_f_59 :: Foreground256
color256_f_59 = Foreground256 . code $ "38;5;59"

color256_f_60 :: Foreground256
color256_f_60 = Foreground256 . code $ "38;5;60"

color256_f_61 :: Foreground256
color256_f_61 = Foreground256 . code $ "38;5;61"

color256_f_62 :: Foreground256
color256_f_62 = Foreground256 . code $ "38;5;62"

color256_f_63 :: Foreground256
color256_f_63 = Foreground256 . code $ "38;5;63"

color256_f_64 :: Foreground256
color256_f_64 = Foreground256 . code $ "38;5;64"

color256_f_65 :: Foreground256
color256_f_65 = Foreground256 . code $ "38;5;65"

color256_f_66 :: Foreground256
color256_f_66 = Foreground256 . code $ "38;5;66"

color256_f_67 :: Foreground256
color256_f_67 = Foreground256 . code $ "38;5;67"

color256_f_68 :: Foreground256
color256_f_68 = Foreground256 . code $ "38;5;68"

color256_f_69 :: Foreground256
color256_f_69 = Foreground256 . code $ "38;5;69"

color256_f_70 :: Foreground256
color256_f_70 = Foreground256 . code $ "38;5;70"

color256_f_71 :: Foreground256
color256_f_71 = Foreground256 . code $ "38;5;71"

color256_f_72 :: Foreground256
color256_f_72 = Foreground256 . code $ "38;5;72"

color256_f_73 :: Foreground256
color256_f_73 = Foreground256 . code $ "38;5;73"

color256_f_74 :: Foreground256
color256_f_74 = Foreground256 . code $ "38;5;74"

color256_f_75 :: Foreground256
color256_f_75 = Foreground256 . code $ "38;5;75"

color256_f_76 :: Foreground256
color256_f_76 = Foreground256 . code $ "38;5;76"

color256_f_77 :: Foreground256
color256_f_77 = Foreground256 . code $ "38;5;77"

color256_f_78 :: Foreground256
color256_f_78 = Foreground256 . code $ "38;5;78"

color256_f_79 :: Foreground256
color256_f_79 = Foreground256 . code $ "38;5;79"

color256_f_80 :: Foreground256
color256_f_80 = Foreground256 . code $ "38;5;80"

color256_f_81 :: Foreground256
color256_f_81 = Foreground256 . code $ "38;5;81"

color256_f_82 :: Foreground256
color256_f_82 = Foreground256 . code $ "38;5;82"

color256_f_83 :: Foreground256
color256_f_83 = Foreground256 . code $ "38;5;83"

color256_f_84 :: Foreground256
color256_f_84 = Foreground256 . code $ "38;5;84"

color256_f_85 :: Foreground256
color256_f_85 = Foreground256 . code $ "38;5;85"

color256_f_86 :: Foreground256
color256_f_86 = Foreground256 . code $ "38;5;86"

color256_f_87 :: Foreground256
color256_f_87 = Foreground256 . code $ "38;5;87"

color256_f_88 :: Foreground256
color256_f_88 = Foreground256 . code $ "38;5;88"

color256_f_89 :: Foreground256
color256_f_89 = Foreground256 . code $ "38;5;89"

color256_f_90 :: Foreground256
color256_f_90 = Foreground256 . code $ "38;5;90"

color256_f_91 :: Foreground256
color256_f_91 = Foreground256 . code $ "38;5;91"

color256_f_92 :: Foreground256
color256_f_92 = Foreground256 . code $ "38;5;92"

color256_f_93 :: Foreground256
color256_f_93 = Foreground256 . code $ "38;5;93"

color256_f_94 :: Foreground256
color256_f_94 = Foreground256 . code $ "38;5;94"

color256_f_95 :: Foreground256
color256_f_95 = Foreground256 . code $ "38;5;95"

color256_f_96 :: Foreground256
color256_f_96 = Foreground256 . code $ "38;5;96"

color256_f_97 :: Foreground256
color256_f_97 = Foreground256 . code $ "38;5;97"

color256_f_98 :: Foreground256
color256_f_98 = Foreground256 . code $ "38;5;98"

color256_f_99 :: Foreground256
color256_f_99 = Foreground256 . code $ "38;5;99"

color256_f_100 :: Foreground256
color256_f_100 = Foreground256 . code $ "38;5;100"

color256_f_101 :: Foreground256
color256_f_101 = Foreground256 . code $ "38;5;101"

color256_f_102 :: Foreground256
color256_f_102 = Foreground256 . code $ "38;5;102"

color256_f_103 :: Foreground256
color256_f_103 = Foreground256 . code $ "38;5;103"

color256_f_104 :: Foreground256
color256_f_104 = Foreground256 . code $ "38;5;104"

color256_f_105 :: Foreground256
color256_f_105 = Foreground256 . code $ "38;5;105"

color256_f_106 :: Foreground256
color256_f_106 = Foreground256 . code $ "38;5;106"

color256_f_107 :: Foreground256
color256_f_107 = Foreground256 . code $ "38;5;107"

color256_f_108 :: Foreground256
color256_f_108 = Foreground256 . code $ "38;5;108"

color256_f_109 :: Foreground256
color256_f_109 = Foreground256 . code $ "38;5;109"

color256_f_110 :: Foreground256
color256_f_110 = Foreground256 . code $ "38;5;110"

color256_f_111 :: Foreground256
color256_f_111 = Foreground256 . code $ "38;5;111"

color256_f_112 :: Foreground256
color256_f_112 = Foreground256 . code $ "38;5;112"

color256_f_113 :: Foreground256
color256_f_113 = Foreground256 . code $ "38;5;113"

color256_f_114 :: Foreground256
color256_f_114 = Foreground256 . code $ "38;5;114"

color256_f_115 :: Foreground256
color256_f_115 = Foreground256 . code $ "38;5;115"

color256_f_116 :: Foreground256
color256_f_116 = Foreground256 . code $ "38;5;116"

color256_f_117 :: Foreground256
color256_f_117 = Foreground256 . code $ "38;5;117"

color256_f_118 :: Foreground256
color256_f_118 = Foreground256 . code $ "38;5;118"

color256_f_119 :: Foreground256
color256_f_119 = Foreground256 . code $ "38;5;119"

color256_f_120 :: Foreground256
color256_f_120 = Foreground256 . code $ "38;5;120"

color256_f_121 :: Foreground256
color256_f_121 = Foreground256 . code $ "38;5;121"

color256_f_122 :: Foreground256
color256_f_122 = Foreground256 . code $ "38;5;122"

color256_f_123 :: Foreground256
color256_f_123 = Foreground256 . code $ "38;5;123"

color256_f_124 :: Foreground256
color256_f_124 = Foreground256 . code $ "38;5;124"

color256_f_125 :: Foreground256
color256_f_125 = Foreground256 . code $ "38;5;125"

color256_f_126 :: Foreground256
color256_f_126 = Foreground256 . code $ "38;5;126"

color256_f_127 :: Foreground256
color256_f_127 = Foreground256 . code $ "38;5;127"

color256_f_128 :: Foreground256
color256_f_128 = Foreground256 . code $ "38;5;128"

color256_f_129 :: Foreground256
color256_f_129 = Foreground256 . code $ "38;5;129"

color256_f_130 :: Foreground256
color256_f_130 = Foreground256 . code $ "38;5;130"

color256_f_131 :: Foreground256
color256_f_131 = Foreground256 . code $ "38;5;131"

color256_f_132 :: Foreground256
color256_f_132 = Foreground256 . code $ "38;5;132"

color256_f_133 :: Foreground256
color256_f_133 = Foreground256 . code $ "38;5;133"

color256_f_134 :: Foreground256
color256_f_134 = Foreground256 . code $ "38;5;134"

color256_f_135 :: Foreground256
color256_f_135 = Foreground256 . code $ "38;5;135"

color256_f_136 :: Foreground256
color256_f_136 = Foreground256 . code $ "38;5;136"

color256_f_137 :: Foreground256
color256_f_137 = Foreground256 . code $ "38;5;137"

color256_f_138 :: Foreground256
color256_f_138 = Foreground256 . code $ "38;5;138"

color256_f_139 :: Foreground256
color256_f_139 = Foreground256 . code $ "38;5;139"

color256_f_140 :: Foreground256
color256_f_140 = Foreground256 . code $ "38;5;140"

color256_f_141 :: Foreground256
color256_f_141 = Foreground256 . code $ "38;5;141"

color256_f_142 :: Foreground256
color256_f_142 = Foreground256 . code $ "38;5;142"

color256_f_143 :: Foreground256
color256_f_143 = Foreground256 . code $ "38;5;143"

color256_f_144 :: Foreground256
color256_f_144 = Foreground256 . code $ "38;5;144"

color256_f_145 :: Foreground256
color256_f_145 = Foreground256 . code $ "38;5;145"

color256_f_146 :: Foreground256
color256_f_146 = Foreground256 . code $ "38;5;146"

color256_f_147 :: Foreground256
color256_f_147 = Foreground256 . code $ "38;5;147"

color256_f_148 :: Foreground256
color256_f_148 = Foreground256 . code $ "38;5;148"

color256_f_149 :: Foreground256
color256_f_149 = Foreground256 . code $ "38;5;149"

color256_f_150 :: Foreground256
color256_f_150 = Foreground256 . code $ "38;5;150"

color256_f_151 :: Foreground256
color256_f_151 = Foreground256 . code $ "38;5;151"

color256_f_152 :: Foreground256
color256_f_152 = Foreground256 . code $ "38;5;152"

color256_f_153 :: Foreground256
color256_f_153 = Foreground256 . code $ "38;5;153"

color256_f_154 :: Foreground256
color256_f_154 = Foreground256 . code $ "38;5;154"

color256_f_155 :: Foreground256
color256_f_155 = Foreground256 . code $ "38;5;155"

color256_f_156 :: Foreground256
color256_f_156 = Foreground256 . code $ "38;5;156"

color256_f_157 :: Foreground256
color256_f_157 = Foreground256 . code $ "38;5;157"

color256_f_158 :: Foreground256
color256_f_158 = Foreground256 . code $ "38;5;158"

color256_f_159 :: Foreground256
color256_f_159 = Foreground256 . code $ "38;5;159"

color256_f_160 :: Foreground256
color256_f_160 = Foreground256 . code $ "38;5;160"

color256_f_161 :: Foreground256
color256_f_161 = Foreground256 . code $ "38;5;161"

color256_f_162 :: Foreground256
color256_f_162 = Foreground256 . code $ "38;5;162"

color256_f_163 :: Foreground256
color256_f_163 = Foreground256 . code $ "38;5;163"

color256_f_164 :: Foreground256
color256_f_164 = Foreground256 . code $ "38;5;164"

color256_f_165 :: Foreground256
color256_f_165 = Foreground256 . code $ "38;5;165"

color256_f_166 :: Foreground256
color256_f_166 = Foreground256 . code $ "38;5;166"

color256_f_167 :: Foreground256
color256_f_167 = Foreground256 . code $ "38;5;167"

color256_f_168 :: Foreground256
color256_f_168 = Foreground256 . code $ "38;5;168"

color256_f_169 :: Foreground256
color256_f_169 = Foreground256 . code $ "38;5;169"

color256_f_170 :: Foreground256
color256_f_170 = Foreground256 . code $ "38;5;170"

color256_f_171 :: Foreground256
color256_f_171 = Foreground256 . code $ "38;5;171"

color256_f_172 :: Foreground256
color256_f_172 = Foreground256 . code $ "38;5;172"

color256_f_173 :: Foreground256
color256_f_173 = Foreground256 . code $ "38;5;173"

color256_f_174 :: Foreground256
color256_f_174 = Foreground256 . code $ "38;5;174"

color256_f_175 :: Foreground256
color256_f_175 = Foreground256 . code $ "38;5;175"

color256_f_176 :: Foreground256
color256_f_176 = Foreground256 . code $ "38;5;176"

color256_f_177 :: Foreground256
color256_f_177 = Foreground256 . code $ "38;5;177"

color256_f_178 :: Foreground256
color256_f_178 = Foreground256 . code $ "38;5;178"

color256_f_179 :: Foreground256
color256_f_179 = Foreground256 . code $ "38;5;179"

color256_f_180 :: Foreground256
color256_f_180 = Foreground256 . code $ "38;5;180"

color256_f_181 :: Foreground256
color256_f_181 = Foreground256 . code $ "38;5;181"

color256_f_182 :: Foreground256
color256_f_182 = Foreground256 . code $ "38;5;182"

color256_f_183 :: Foreground256
color256_f_183 = Foreground256 . code $ "38;5;183"

color256_f_184 :: Foreground256
color256_f_184 = Foreground256 . code $ "38;5;184"

color256_f_185 :: Foreground256
color256_f_185 = Foreground256 . code $ "38;5;185"

color256_f_186 :: Foreground256
color256_f_186 = Foreground256 . code $ "38;5;186"

color256_f_187 :: Foreground256
color256_f_187 = Foreground256 . code $ "38;5;187"

color256_f_188 :: Foreground256
color256_f_188 = Foreground256 . code $ "38;5;188"

color256_f_189 :: Foreground256
color256_f_189 = Foreground256 . code $ "38;5;189"

color256_f_190 :: Foreground256
color256_f_190 = Foreground256 . code $ "38;5;190"

color256_f_191 :: Foreground256
color256_f_191 = Foreground256 . code $ "38;5;191"

color256_f_192 :: Foreground256
color256_f_192 = Foreground256 . code $ "38;5;192"

color256_f_193 :: Foreground256
color256_f_193 = Foreground256 . code $ "38;5;193"

color256_f_194 :: Foreground256
color256_f_194 = Foreground256 . code $ "38;5;194"

color256_f_195 :: Foreground256
color256_f_195 = Foreground256 . code $ "38;5;195"

color256_f_196 :: Foreground256
color256_f_196 = Foreground256 . code $ "38;5;196"

color256_f_197 :: Foreground256
color256_f_197 = Foreground256 . code $ "38;5;197"

color256_f_198 :: Foreground256
color256_f_198 = Foreground256 . code $ "38;5;198"

color256_f_199 :: Foreground256
color256_f_199 = Foreground256 . code $ "38;5;199"

color256_f_200 :: Foreground256
color256_f_200 = Foreground256 . code $ "38;5;200"

color256_f_201 :: Foreground256
color256_f_201 = Foreground256 . code $ "38;5;201"

color256_f_202 :: Foreground256
color256_f_202 = Foreground256 . code $ "38;5;202"

color256_f_203 :: Foreground256
color256_f_203 = Foreground256 . code $ "38;5;203"

color256_f_204 :: Foreground256
color256_f_204 = Foreground256 . code $ "38;5;204"

color256_f_205 :: Foreground256
color256_f_205 = Foreground256 . code $ "38;5;205"

color256_f_206 :: Foreground256
color256_f_206 = Foreground256 . code $ "38;5;206"

color256_f_207 :: Foreground256
color256_f_207 = Foreground256 . code $ "38;5;207"

color256_f_208 :: Foreground256
color256_f_208 = Foreground256 . code $ "38;5;208"

color256_f_209 :: Foreground256
color256_f_209 = Foreground256 . code $ "38;5;209"

color256_f_210 :: Foreground256
color256_f_210 = Foreground256 . code $ "38;5;210"

color256_f_211 :: Foreground256
color256_f_211 = Foreground256 . code $ "38;5;211"

color256_f_212 :: Foreground256
color256_f_212 = Foreground256 . code $ "38;5;212"

color256_f_213 :: Foreground256
color256_f_213 = Foreground256 . code $ "38;5;213"

color256_f_214 :: Foreground256
color256_f_214 = Foreground256 . code $ "38;5;214"

color256_f_215 :: Foreground256
color256_f_215 = Foreground256 . code $ "38;5;215"

color256_f_216 :: Foreground256
color256_f_216 = Foreground256 . code $ "38;5;216"

color256_f_217 :: Foreground256
color256_f_217 = Foreground256 . code $ "38;5;217"

color256_f_218 :: Foreground256
color256_f_218 = Foreground256 . code $ "38;5;218"

color256_f_219 :: Foreground256
color256_f_219 = Foreground256 . code $ "38;5;219"

color256_f_220 :: Foreground256
color256_f_220 = Foreground256 . code $ "38;5;220"

color256_f_221 :: Foreground256
color256_f_221 = Foreground256 . code $ "38;5;221"

color256_f_222 :: Foreground256
color256_f_222 = Foreground256 . code $ "38;5;222"

color256_f_223 :: Foreground256
color256_f_223 = Foreground256 . code $ "38;5;223"

color256_f_224 :: Foreground256
color256_f_224 = Foreground256 . code $ "38;5;224"

color256_f_225 :: Foreground256
color256_f_225 = Foreground256 . code $ "38;5;225"

color256_f_226 :: Foreground256
color256_f_226 = Foreground256 . code $ "38;5;226"

color256_f_227 :: Foreground256
color256_f_227 = Foreground256 . code $ "38;5;227"

color256_f_228 :: Foreground256
color256_f_228 = Foreground256 . code $ "38;5;228"

color256_f_229 :: Foreground256
color256_f_229 = Foreground256 . code $ "38;5;229"

color256_f_230 :: Foreground256
color256_f_230 = Foreground256 . code $ "38;5;230"

color256_f_231 :: Foreground256
color256_f_231 = Foreground256 . code $ "38;5;231"

color256_f_232 :: Foreground256
color256_f_232 = Foreground256 . code $ "38;5;232"

color256_f_233 :: Foreground256
color256_f_233 = Foreground256 . code $ "38;5;233"

color256_f_234 :: Foreground256
color256_f_234 = Foreground256 . code $ "38;5;234"

color256_f_235 :: Foreground256
color256_f_235 = Foreground256 . code $ "38;5;235"

color256_f_236 :: Foreground256
color256_f_236 = Foreground256 . code $ "38;5;236"

color256_f_237 :: Foreground256
color256_f_237 = Foreground256 . code $ "38;5;237"

color256_f_238 :: Foreground256
color256_f_238 = Foreground256 . code $ "38;5;238"

color256_f_239 :: Foreground256
color256_f_239 = Foreground256 . code $ "38;5;239"

color256_f_240 :: Foreground256
color256_f_240 = Foreground256 . code $ "38;5;240"

color256_f_241 :: Foreground256
color256_f_241 = Foreground256 . code $ "38;5;241"

color256_f_242 :: Foreground256
color256_f_242 = Foreground256 . code $ "38;5;242"

color256_f_243 :: Foreground256
color256_f_243 = Foreground256 . code $ "38;5;243"

color256_f_244 :: Foreground256
color256_f_244 = Foreground256 . code $ "38;5;244"

color256_f_245 :: Foreground256
color256_f_245 = Foreground256 . code $ "38;5;245"

color256_f_246 :: Foreground256
color256_f_246 = Foreground256 . code $ "38;5;246"

color256_f_247 :: Foreground256
color256_f_247 = Foreground256 . code $ "38;5;247"

color256_f_248 :: Foreground256
color256_f_248 = Foreground256 . code $ "38;5;248"

color256_f_249 :: Foreground256
color256_f_249 = Foreground256 . code $ "38;5;249"

color256_f_250 :: Foreground256
color256_f_250 = Foreground256 . code $ "38;5;250"

color256_f_251 :: Foreground256
color256_f_251 = Foreground256 . code $ "38;5;251"

color256_f_252 :: Foreground256
color256_f_252 = Foreground256 . code $ "38;5;252"

color256_f_253 :: Foreground256
color256_f_253 = Foreground256 . code $ "38;5;253"

color256_f_254 :: Foreground256
color256_f_254 = Foreground256 . code $ "38;5;254"

color256_f_255 :: Foreground256
color256_f_255 = Foreground256 . code $ "38;5;255"

color256_b_default :: Background256
color256_b_default = Background256 . Code $ mempty

color256_b_0 :: Background256
color256_b_0 = Background256 . code $ "48;5;0"

color256_b_1 :: Background256
color256_b_1 = Background256 . code $ "48;5;1"

color256_b_2 :: Background256
color256_b_2 = Background256 . code $ "48;5;2"

color256_b_3 :: Background256
color256_b_3 = Background256 . code $ "48;5;3"

color256_b_4 :: Background256
color256_b_4 = Background256 . code $ "48;5;4"

color256_b_5 :: Background256
color256_b_5 = Background256 . code $ "48;5;5"

color256_b_6 :: Background256
color256_b_6 = Background256 . code $ "48;5;6"

color256_b_7 :: Background256
color256_b_7 = Background256 . code $ "48;5;7"

color256_b_8 :: Background256
color256_b_8 = Background256 . code $ "48;5;8"

color256_b_9 :: Background256
color256_b_9 = Background256 . code $ "48;5;9"

color256_b_10 :: Background256
color256_b_10 = Background256 . code $ "48;5;10"

color256_b_11 :: Background256
color256_b_11 = Background256 . code $ "48;5;11"

color256_b_12 :: Background256
color256_b_12 = Background256 . code $ "48;5;12"

color256_b_13 :: Background256
color256_b_13 = Background256 . code $ "48;5;13"

color256_b_14 :: Background256
color256_b_14 = Background256 . code $ "48;5;14"

color256_b_15 :: Background256
color256_b_15 = Background256 . code $ "48;5;15"

color256_b_16 :: Background256
color256_b_16 = Background256 . code $ "48;5;16"

color256_b_17 :: Background256
color256_b_17 = Background256 . code $ "48;5;17"

color256_b_18 :: Background256
color256_b_18 = Background256 . code $ "48;5;18"

color256_b_19 :: Background256
color256_b_19 = Background256 . code $ "48;5;19"

color256_b_20 :: Background256
color256_b_20 = Background256 . code $ "48;5;20"

color256_b_21 :: Background256
color256_b_21 = Background256 . code $ "48;5;21"

color256_b_22 :: Background256
color256_b_22 = Background256 . code $ "48;5;22"

color256_b_23 :: Background256
color256_b_23 = Background256 . code $ "48;5;23"

color256_b_24 :: Background256
color256_b_24 = Background256 . code $ "48;5;24"

color256_b_25 :: Background256
color256_b_25 = Background256 . code $ "48;5;25"

color256_b_26 :: Background256
color256_b_26 = Background256 . code $ "48;5;26"

color256_b_27 :: Background256
color256_b_27 = Background256 . code $ "48;5;27"

color256_b_28 :: Background256
color256_b_28 = Background256 . code $ "48;5;28"

color256_b_29 :: Background256
color256_b_29 = Background256 . code $ "48;5;29"

color256_b_30 :: Background256
color256_b_30 = Background256 . code $ "48;5;30"

color256_b_31 :: Background256
color256_b_31 = Background256 . code $ "48;5;31"

color256_b_32 :: Background256
color256_b_32 = Background256 . code $ "48;5;32"

color256_b_33 :: Background256
color256_b_33 = Background256 . code $ "48;5;33"

color256_b_34 :: Background256
color256_b_34 = Background256 . code $ "48;5;34"

color256_b_35 :: Background256
color256_b_35 = Background256 . code $ "48;5;35"

color256_b_36 :: Background256
color256_b_36 = Background256 . code $ "48;5;36"

color256_b_37 :: Background256
color256_b_37 = Background256 . code $ "48;5;37"

color256_b_38 :: Background256
color256_b_38 = Background256 . code $ "48;5;38"

color256_b_39 :: Background256
color256_b_39 = Background256 . code $ "48;5;39"

color256_b_40 :: Background256
color256_b_40 = Background256 . code $ "48;5;40"

color256_b_41 :: Background256
color256_b_41 = Background256 . code $ "48;5;41"

color256_b_42 :: Background256
color256_b_42 = Background256 . code $ "48;5;42"

color256_b_43 :: Background256
color256_b_43 = Background256 . code $ "48;5;43"

color256_b_44 :: Background256
color256_b_44 = Background256 . code $ "48;5;44"

color256_b_45 :: Background256
color256_b_45 = Background256 . code $ "48;5;45"

color256_b_46 :: Background256
color256_b_46 = Background256 . code $ "48;5;46"

color256_b_47 :: Background256
color256_b_47 = Background256 . code $ "48;5;47"

color256_b_48 :: Background256
color256_b_48 = Background256 . code $ "48;5;48"

color256_b_49 :: Background256
color256_b_49 = Background256 . code $ "48;5;49"

color256_b_50 :: Background256
color256_b_50 = Background256 . code $ "48;5;50"

color256_b_51 :: Background256
color256_b_51 = Background256 . code $ "48;5;51"

color256_b_52 :: Background256
color256_b_52 = Background256 . code $ "48;5;52"

color256_b_53 :: Background256
color256_b_53 = Background256 . code $ "48;5;53"

color256_b_54 :: Background256
color256_b_54 = Background256 . code $ "48;5;54"

color256_b_55 :: Background256
color256_b_55 = Background256 . code $ "48;5;55"

color256_b_56 :: Background256
color256_b_56 = Background256 . code $ "48;5;56"

color256_b_57 :: Background256
color256_b_57 = Background256 . code $ "48;5;57"

color256_b_58 :: Background256
color256_b_58 = Background256 . code $ "48;5;58"

color256_b_59 :: Background256
color256_b_59 = Background256 . code $ "48;5;59"

color256_b_60 :: Background256
color256_b_60 = Background256 . code $ "48;5;60"

color256_b_61 :: Background256
color256_b_61 = Background256 . code $ "48;5;61"

color256_b_62 :: Background256
color256_b_62 = Background256 . code $ "48;5;62"

color256_b_63 :: Background256
color256_b_63 = Background256 . code $ "48;5;63"

color256_b_64 :: Background256
color256_b_64 = Background256 . code $ "48;5;64"

color256_b_65 :: Background256
color256_b_65 = Background256 . code $ "48;5;65"

color256_b_66 :: Background256
color256_b_66 = Background256 . code $ "48;5;66"

color256_b_67 :: Background256
color256_b_67 = Background256 . code $ "48;5;67"

color256_b_68 :: Background256
color256_b_68 = Background256 . code $ "48;5;68"

color256_b_69 :: Background256
color256_b_69 = Background256 . code $ "48;5;69"

color256_b_70 :: Background256
color256_b_70 = Background256 . code $ "48;5;70"

color256_b_71 :: Background256
color256_b_71 = Background256 . code $ "48;5;71"

color256_b_72 :: Background256
color256_b_72 = Background256 . code $ "48;5;72"

color256_b_73 :: Background256
color256_b_73 = Background256 . code $ "48;5;73"

color256_b_74 :: Background256
color256_b_74 = Background256 . code $ "48;5;74"

color256_b_75 :: Background256
color256_b_75 = Background256 . code $ "48;5;75"

color256_b_76 :: Background256
color256_b_76 = Background256 . code $ "48;5;76"

color256_b_77 :: Background256
color256_b_77 = Background256 . code $ "48;5;77"

color256_b_78 :: Background256
color256_b_78 = Background256 . code $ "48;5;78"

color256_b_79 :: Background256
color256_b_79 = Background256 . code $ "48;5;79"

color256_b_80 :: Background256
color256_b_80 = Background256 . code $ "48;5;80"

color256_b_81 :: Background256
color256_b_81 = Background256 . code $ "48;5;81"

color256_b_82 :: Background256
color256_b_82 = Background256 . code $ "48;5;82"

color256_b_83 :: Background256
color256_b_83 = Background256 . code $ "48;5;83"

color256_b_84 :: Background256
color256_b_84 = Background256 . code $ "48;5;84"

color256_b_85 :: Background256
color256_b_85 = Background256 . code $ "48;5;85"

color256_b_86 :: Background256
color256_b_86 = Background256 . code $ "48;5;86"

color256_b_87 :: Background256
color256_b_87 = Background256 . code $ "48;5;87"

color256_b_88 :: Background256
color256_b_88 = Background256 . code $ "48;5;88"

color256_b_89 :: Background256
color256_b_89 = Background256 . code $ "48;5;89"

color256_b_90 :: Background256
color256_b_90 = Background256 . code $ "48;5;90"

color256_b_91 :: Background256
color256_b_91 = Background256 . code $ "48;5;91"

color256_b_92 :: Background256
color256_b_92 = Background256 . code $ "48;5;92"

color256_b_93 :: Background256
color256_b_93 = Background256 . code $ "48;5;93"

color256_b_94 :: Background256
color256_b_94 = Background256 . code $ "48;5;94"

color256_b_95 :: Background256
color256_b_95 = Background256 . code $ "48;5;95"

color256_b_96 :: Background256
color256_b_96 = Background256 . code $ "48;5;96"

color256_b_97 :: Background256
color256_b_97 = Background256 . code $ "48;5;97"

color256_b_98 :: Background256
color256_b_98 = Background256 . code $ "48;5;98"

color256_b_99 :: Background256
color256_b_99 = Background256 . code $ "48;5;99"

color256_b_100 :: Background256
color256_b_100 = Background256 . code $ "48;5;100"

color256_b_101 :: Background256
color256_b_101 = Background256 . code $ "48;5;101"

color256_b_102 :: Background256
color256_b_102 = Background256 . code $ "48;5;102"

color256_b_103 :: Background256
color256_b_103 = Background256 . code $ "48;5;103"

color256_b_104 :: Background256
color256_b_104 = Background256 . code $ "48;5;104"

color256_b_105 :: Background256
color256_b_105 = Background256 . code $ "48;5;105"

color256_b_106 :: Background256
color256_b_106 = Background256 . code $ "48;5;106"

color256_b_107 :: Background256
color256_b_107 = Background256 . code $ "48;5;107"

color256_b_108 :: Background256
color256_b_108 = Background256 . code $ "48;5;108"

color256_b_109 :: Background256
color256_b_109 = Background256 . code $ "48;5;109"

color256_b_110 :: Background256
color256_b_110 = Background256 . code $ "48;5;110"

color256_b_111 :: Background256
color256_b_111 = Background256 . code $ "48;5;111"

color256_b_112 :: Background256
color256_b_112 = Background256 . code $ "48;5;112"

color256_b_113 :: Background256
color256_b_113 = Background256 . code $ "48;5;113"

color256_b_114 :: Background256
color256_b_114 = Background256 . code $ "48;5;114"

color256_b_115 :: Background256
color256_b_115 = Background256 . code $ "48;5;115"

color256_b_116 :: Background256
color256_b_116 = Background256 . code $ "48;5;116"

color256_b_117 :: Background256
color256_b_117 = Background256 . code $ "48;5;117"

color256_b_118 :: Background256
color256_b_118 = Background256 . code $ "48;5;118"

color256_b_119 :: Background256
color256_b_119 = Background256 . code $ "48;5;119"

color256_b_120 :: Background256
color256_b_120 = Background256 . code $ "48;5;120"

color256_b_121 :: Background256
color256_b_121 = Background256 . code $ "48;5;121"

color256_b_122 :: Background256
color256_b_122 = Background256 . code $ "48;5;122"

color256_b_123 :: Background256
color256_b_123 = Background256 . code $ "48;5;123"

color256_b_124 :: Background256
color256_b_124 = Background256 . code $ "48;5;124"

color256_b_125 :: Background256
color256_b_125 = Background256 . code $ "48;5;125"

color256_b_126 :: Background256
color256_b_126 = Background256 . code $ "48;5;126"

color256_b_127 :: Background256
color256_b_127 = Background256 . code $ "48;5;127"

color256_b_128 :: Background256
color256_b_128 = Background256 . code $ "48;5;128"

color256_b_129 :: Background256
color256_b_129 = Background256 . code $ "48;5;129"

color256_b_130 :: Background256
color256_b_130 = Background256 . code $ "48;5;130"

color256_b_131 :: Background256
color256_b_131 = Background256 . code $ "48;5;131"

color256_b_132 :: Background256
color256_b_132 = Background256 . code $ "48;5;132"

color256_b_133 :: Background256
color256_b_133 = Background256 . code $ "48;5;133"

color256_b_134 :: Background256
color256_b_134 = Background256 . code $ "48;5;134"

color256_b_135 :: Background256
color256_b_135 = Background256 . code $ "48;5;135"

color256_b_136 :: Background256
color256_b_136 = Background256 . code $ "48;5;136"

color256_b_137 :: Background256
color256_b_137 = Background256 . code $ "48;5;137"

color256_b_138 :: Background256
color256_b_138 = Background256 . code $ "48;5;138"

color256_b_139 :: Background256
color256_b_139 = Background256 . code $ "48;5;139"

color256_b_140 :: Background256
color256_b_140 = Background256 . code $ "48;5;140"

color256_b_141 :: Background256
color256_b_141 = Background256 . code $ "48;5;141"

color256_b_142 :: Background256
color256_b_142 = Background256 . code $ "48;5;142"

color256_b_143 :: Background256
color256_b_143 = Background256 . code $ "48;5;143"

color256_b_144 :: Background256
color256_b_144 = Background256 . code $ "48;5;144"

color256_b_145 :: Background256
color256_b_145 = Background256 . code $ "48;5;145"

color256_b_146 :: Background256
color256_b_146 = Background256 . code $ "48;5;146"

color256_b_147 :: Background256
color256_b_147 = Background256 . code $ "48;5;147"

color256_b_148 :: Background256
color256_b_148 = Background256 . code $ "48;5;148"

color256_b_149 :: Background256
color256_b_149 = Background256 . code $ "48;5;149"

color256_b_150 :: Background256
color256_b_150 = Background256 . code $ "48;5;150"

color256_b_151 :: Background256
color256_b_151 = Background256 . code $ "48;5;151"

color256_b_152 :: Background256
color256_b_152 = Background256 . code $ "48;5;152"

color256_b_153 :: Background256
color256_b_153 = Background256 . code $ "48;5;153"

color256_b_154 :: Background256
color256_b_154 = Background256 . code $ "48;5;154"

color256_b_155 :: Background256
color256_b_155 = Background256 . code $ "48;5;155"

color256_b_156 :: Background256
color256_b_156 = Background256 . code $ "48;5;156"

color256_b_157 :: Background256
color256_b_157 = Background256 . code $ "48;5;157"

color256_b_158 :: Background256
color256_b_158 = Background256 . code $ "48;5;158"

color256_b_159 :: Background256
color256_b_159 = Background256 . code $ "48;5;159"

color256_b_160 :: Background256
color256_b_160 = Background256 . code $ "48;5;160"

color256_b_161 :: Background256
color256_b_161 = Background256 . code $ "48;5;161"

color256_b_162 :: Background256
color256_b_162 = Background256 . code $ "48;5;162"

color256_b_163 :: Background256
color256_b_163 = Background256 . code $ "48;5;163"

color256_b_164 :: Background256
color256_b_164 = Background256 . code $ "48;5;164"

color256_b_165 :: Background256
color256_b_165 = Background256 . code $ "48;5;165"

color256_b_166 :: Background256
color256_b_166 = Background256 . code $ "48;5;166"

color256_b_167 :: Background256
color256_b_167 = Background256 . code $ "48;5;167"

color256_b_168 :: Background256
color256_b_168 = Background256 . code $ "48;5;168"

color256_b_169 :: Background256
color256_b_169 = Background256 . code $ "48;5;169"

color256_b_170 :: Background256
color256_b_170 = Background256 . code $ "48;5;170"

color256_b_171 :: Background256
color256_b_171 = Background256 . code $ "48;5;171"

color256_b_172 :: Background256
color256_b_172 = Background256 . code $ "48;5;172"

color256_b_173 :: Background256
color256_b_173 = Background256 . code $ "48;5;173"

color256_b_174 :: Background256
color256_b_174 = Background256 . code $ "48;5;174"

color256_b_175 :: Background256
color256_b_175 = Background256 . code $ "48;5;175"

color256_b_176 :: Background256
color256_b_176 = Background256 . code $ "48;5;176"

color256_b_177 :: Background256
color256_b_177 = Background256 . code $ "48;5;177"

color256_b_178 :: Background256
color256_b_178 = Background256 . code $ "48;5;178"

color256_b_179 :: Background256
color256_b_179 = Background256 . code $ "48;5;179"

color256_b_180 :: Background256
color256_b_180 = Background256 . code $ "48;5;180"

color256_b_181 :: Background256
color256_b_181 = Background256 . code $ "48;5;181"

color256_b_182 :: Background256
color256_b_182 = Background256 . code $ "48;5;182"

color256_b_183 :: Background256
color256_b_183 = Background256 . code $ "48;5;183"

color256_b_184 :: Background256
color256_b_184 = Background256 . code $ "48;5;184"

color256_b_185 :: Background256
color256_b_185 = Background256 . code $ "48;5;185"

color256_b_186 :: Background256
color256_b_186 = Background256 . code $ "48;5;186"

color256_b_187 :: Background256
color256_b_187 = Background256 . code $ "48;5;187"

color256_b_188 :: Background256
color256_b_188 = Background256 . code $ "48;5;188"

color256_b_189 :: Background256
color256_b_189 = Background256 . code $ "48;5;189"

color256_b_190 :: Background256
color256_b_190 = Background256 . code $ "48;5;190"

color256_b_191 :: Background256
color256_b_191 = Background256 . code $ "48;5;191"

color256_b_192 :: Background256
color256_b_192 = Background256 . code $ "48;5;192"

color256_b_193 :: Background256
color256_b_193 = Background256 . code $ "48;5;193"

color256_b_194 :: Background256
color256_b_194 = Background256 . code $ "48;5;194"

color256_b_195 :: Background256
color256_b_195 = Background256 . code $ "48;5;195"

color256_b_196 :: Background256
color256_b_196 = Background256 . code $ "48;5;196"

color256_b_197 :: Background256
color256_b_197 = Background256 . code $ "48;5;197"

color256_b_198 :: Background256
color256_b_198 = Background256 . code $ "48;5;198"

color256_b_199 :: Background256
color256_b_199 = Background256 . code $ "48;5;199"

color256_b_200 :: Background256
color256_b_200 = Background256 . code $ "48;5;200"

color256_b_201 :: Background256
color256_b_201 = Background256 . code $ "48;5;201"

color256_b_202 :: Background256
color256_b_202 = Background256 . code $ "48;5;202"

color256_b_203 :: Background256
color256_b_203 = Background256 . code $ "48;5;203"

color256_b_204 :: Background256
color256_b_204 = Background256 . code $ "48;5;204"

color256_b_205 :: Background256
color256_b_205 = Background256 . code $ "48;5;205"

color256_b_206 :: Background256
color256_b_206 = Background256 . code $ "48;5;206"

color256_b_207 :: Background256
color256_b_207 = Background256 . code $ "48;5;207"

color256_b_208 :: Background256
color256_b_208 = Background256 . code $ "48;5;208"

color256_b_209 :: Background256
color256_b_209 = Background256 . code $ "48;5;209"

color256_b_210 :: Background256
color256_b_210 = Background256 . code $ "48;5;210"

color256_b_211 :: Background256
color256_b_211 = Background256 . code $ "48;5;211"

color256_b_212 :: Background256
color256_b_212 = Background256 . code $ "48;5;212"

color256_b_213 :: Background256
color256_b_213 = Background256 . code $ "48;5;213"

color256_b_214 :: Background256
color256_b_214 = Background256 . code $ "48;5;214"

color256_b_215 :: Background256
color256_b_215 = Background256 . code $ "48;5;215"

color256_b_216 :: Background256
color256_b_216 = Background256 . code $ "48;5;216"

color256_b_217 :: Background256
color256_b_217 = Background256 . code $ "48;5;217"

color256_b_218 :: Background256
color256_b_218 = Background256 . code $ "48;5;218"

color256_b_219 :: Background256
color256_b_219 = Background256 . code $ "48;5;219"

color256_b_220 :: Background256
color256_b_220 = Background256 . code $ "48;5;220"

color256_b_221 :: Background256
color256_b_221 = Background256 . code $ "48;5;221"

color256_b_222 :: Background256
color256_b_222 = Background256 . code $ "48;5;222"

color256_b_223 :: Background256
color256_b_223 = Background256 . code $ "48;5;223"

color256_b_224 :: Background256
color256_b_224 = Background256 . code $ "48;5;224"

color256_b_225 :: Background256
color256_b_225 = Background256 . code $ "48;5;225"

color256_b_226 :: Background256
color256_b_226 = Background256 . code $ "48;5;226"

color256_b_227 :: Background256
color256_b_227 = Background256 . code $ "48;5;227"

color256_b_228 :: Background256
color256_b_228 = Background256 . code $ "48;5;228"

color256_b_229 :: Background256
color256_b_229 = Background256 . code $ "48;5;229"

color256_b_230 :: Background256
color256_b_230 = Background256 . code $ "48;5;230"

color256_b_231 :: Background256
color256_b_231 = Background256 . code $ "48;5;231"

color256_b_232 :: Background256
color256_b_232 = Background256 . code $ "48;5;232"

color256_b_233 :: Background256
color256_b_233 = Background256 . code $ "48;5;233"

color256_b_234 :: Background256
color256_b_234 = Background256 . code $ "48;5;234"

color256_b_235 :: Background256
color256_b_235 = Background256 . code $ "48;5;235"

color256_b_236 :: Background256
color256_b_236 = Background256 . code $ "48;5;236"

color256_b_237 :: Background256
color256_b_237 = Background256 . code $ "48;5;237"

color256_b_238 :: Background256
color256_b_238 = Background256 . code $ "48;5;238"

color256_b_239 :: Background256
color256_b_239 = Background256 . code $ "48;5;239"

color256_b_240 :: Background256
color256_b_240 = Background256 . code $ "48;5;240"

color256_b_241 :: Background256
color256_b_241 = Background256 . code $ "48;5;241"

color256_b_242 :: Background256
color256_b_242 = Background256 . code $ "48;5;242"

color256_b_243 :: Background256
color256_b_243 = Background256 . code $ "48;5;243"

color256_b_244 :: Background256
color256_b_244 = Background256 . code $ "48;5;244"

color256_b_245 :: Background256
color256_b_245 = Background256 . code $ "48;5;245"

color256_b_246 :: Background256
color256_b_246 = Background256 . code $ "48;5;246"

color256_b_247 :: Background256
color256_b_247 = Background256 . code $ "48;5;247"

color256_b_248 :: Background256
color256_b_248 = Background256 . code $ "48;5;248"

color256_b_249 :: Background256
color256_b_249 = Background256 . code $ "48;5;249"

color256_b_250 :: Background256
color256_b_250 = Background256 . code $ "48;5;250"

color256_b_251 :: Background256
color256_b_251 = Background256 . code $ "48;5;251"

color256_b_252 :: Background256
color256_b_252 = Background256 . code $ "48;5;252"

color256_b_253 :: Background256
color256_b_253 = Background256 . code $ "48;5;253"

color256_b_254 :: Background256
color256_b_254 = Background256 . code $ "48;5;254"

color256_b_255 :: Background256
color256_b_255 = Background256 . code $ "48;5;255"

