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
  color256_0,
  color256_1,
  color256_2,
  color256_3,
  color256_4,
  color256_5,
  color256_6,
  color256_7,
  color256_8,
  color256_9,
  color256_10,
  color256_11,
  color256_12,
  color256_13,
  color256_14,
  color256_15,
  color256_16,
  color256_17,
  color256_18,
  color256_19,
  color256_20,
  color256_21,
  color256_22,
  color256_23,
  color256_24,
  color256_25,
  color256_26,
  color256_27,
  color256_28,
  color256_29,
  color256_30,
  color256_31,
  color256_32,
  color256_33,
  color256_34,
  color256_35,
  color256_36,
  color256_37,
  color256_38,
  color256_39,
  color256_40,
  color256_41,
  color256_42,
  color256_43,
  color256_44,
  color256_45,
  color256_46,
  color256_47,
  color256_48,
  color256_49,
  color256_50,
  color256_51,
  color256_52,
  color256_53,
  color256_54,
  color256_55,
  color256_56,
  color256_57,
  color256_58,
  color256_59,
  color256_60,
  color256_61,
  color256_62,
  color256_63,
  color256_64,
  color256_65,
  color256_66,
  color256_67,
  color256_68,
  color256_69,
  color256_70,
  color256_71,
  color256_72,
  color256_73,
  color256_74,
  color256_75,
  color256_76,
  color256_77,
  color256_78,
  color256_79,
  color256_80,
  color256_81,
  color256_82,
  color256_83,
  color256_84,
  color256_85,
  color256_86,
  color256_87,
  color256_88,
  color256_89,
  color256_90,
  color256_91,
  color256_92,
  color256_93,
  color256_94,
  color256_95,
  color256_96,
  color256_97,
  color256_98,
  color256_99,
  color256_100,
  color256_101,
  color256_102,
  color256_103,
  color256_104,
  color256_105,
  color256_106,
  color256_107,
  color256_108,
  color256_109,
  color256_110,
  color256_111,
  color256_112,
  color256_113,
  color256_114,
  color256_115,
  color256_116,
  color256_117,
  color256_118,
  color256_119,
  color256_120,
  color256_121,
  color256_122,
  color256_123,
  color256_124,
  color256_125,
  color256_126,
  color256_127,
  color256_128,
  color256_129,
  color256_130,
  color256_131,
  color256_132,
  color256_133,
  color256_134,
  color256_135,
  color256_136,
  color256_137,
  color256_138,
  color256_139,
  color256_140,
  color256_141,
  color256_142,
  color256_143,
  color256_144,
  color256_145,
  color256_146,
  color256_147,
  color256_148,
  color256_149,
  color256_150,
  color256_151,
  color256_152,
  color256_153,
  color256_154,
  color256_155,
  color256_156,
  color256_157,
  color256_158,
  color256_159,
  color256_160,
  color256_161,
  color256_162,
  color256_163,
  color256_164,
  color256_165,
  color256_166,
  color256_167,
  color256_168,
  color256_169,
  color256_170,
  color256_171,
  color256_172,
  color256_173,
  color256_174,
  color256_175,
  color256_176,
  color256_177,
  color256_178,
  color256_179,
  color256_180,
  color256_181,
  color256_182,
  color256_183,
  color256_184,
  color256_185,
  color256_186,
  color256_187,
  color256_188,
  color256_189,
  color256_190,
  color256_191,
  color256_192,
  color256_193,
  color256_194,
  color256_195,
  color256_196,
  color256_197,
  color256_198,
  color256_199,
  color256_200,
  color256_201,
  color256_202,
  color256_203,
  color256_204,
  color256_205,
  color256_206,
  color256_207,
  color256_208,
  color256_209,
  color256_210,
  color256_211,
  color256_212,
  color256_213,
  color256_214,
  color256_215,
  color256_216,
  color256_217,
  color256_218,
  color256_219,
  color256_220,
  color256_221,
  color256_222,
  color256_223,
  color256_224,
  color256_225,
  color256_226,
  color256_227,
  color256_228,
  color256_229,
  color256_230,
  color256_231,
  color256_232,
  color256_233,
  color256_234,
  color256_235,
  color256_236,
  color256_237,
  color256_238,
  color256_239,
  color256_240,
  color256_241,
  color256_242,
  color256_243,
  color256_244,
  color256_245,
  color256_246,
  color256_247,
  color256_248,
  color256_249,
  color256_250,
  color256_251,
  color256_252,
  color256_253,
  color256_254,
  color256_255,
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

defaultColor :: Color a
defaultColor = Default

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

data Color256 =
  Color256_0
  | Color256_1
  | Color256_2
  | Color256_3
  | Color256_4
  | Color256_5
  | Color256_6
  | Color256_7
  | Color256_8
  | Color256_9
  | Color256_10
  | Color256_11
  | Color256_12
  | Color256_13
  | Color256_14
  | Color256_15
  | Color256_16
  | Color256_17
  | Color256_18
  | Color256_19
  | Color256_20
  | Color256_21
  | Color256_22
  | Color256_23
  | Color256_24
  | Color256_25
  | Color256_26
  | Color256_27
  | Color256_28
  | Color256_29
  | Color256_30
  | Color256_31
  | Color256_32
  | Color256_33
  | Color256_34
  | Color256_35
  | Color256_36
  | Color256_37
  | Color256_38
  | Color256_39
  | Color256_40
  | Color256_41
  | Color256_42
  | Color256_43
  | Color256_44
  | Color256_45
  | Color256_46
  | Color256_47
  | Color256_48
  | Color256_49
  | Color256_50
  | Color256_51
  | Color256_52
  | Color256_53
  | Color256_54
  | Color256_55
  | Color256_56
  | Color256_57
  | Color256_58
  | Color256_59
  | Color256_60
  | Color256_61
  | Color256_62
  | Color256_63
  | Color256_64
  | Color256_65
  | Color256_66
  | Color256_67
  | Color256_68
  | Color256_69
  | Color256_70
  | Color256_71
  | Color256_72
  | Color256_73
  | Color256_74
  | Color256_75
  | Color256_76
  | Color256_77
  | Color256_78
  | Color256_79
  | Color256_80
  | Color256_81
  | Color256_82
  | Color256_83
  | Color256_84
  | Color256_85
  | Color256_86
  | Color256_87
  | Color256_88
  | Color256_89
  | Color256_90
  | Color256_91
  | Color256_92
  | Color256_93
  | Color256_94
  | Color256_95
  | Color256_96
  | Color256_97
  | Color256_98
  | Color256_99
  | Color256_100
  | Color256_101
  | Color256_102
  | Color256_103
  | Color256_104
  | Color256_105
  | Color256_106
  | Color256_107
  | Color256_108
  | Color256_109
  | Color256_110
  | Color256_111
  | Color256_112
  | Color256_113
  | Color256_114
  | Color256_115
  | Color256_116
  | Color256_117
  | Color256_118
  | Color256_119
  | Color256_120
  | Color256_121
  | Color256_122
  | Color256_123
  | Color256_124
  | Color256_125
  | Color256_126
  | Color256_127
  | Color256_128
  | Color256_129
  | Color256_130
  | Color256_131
  | Color256_132
  | Color256_133
  | Color256_134
  | Color256_135
  | Color256_136
  | Color256_137
  | Color256_138
  | Color256_139
  | Color256_140
  | Color256_141
  | Color256_142
  | Color256_143
  | Color256_144
  | Color256_145
  | Color256_146
  | Color256_147
  | Color256_148
  | Color256_149
  | Color256_150
  | Color256_151
  | Color256_152
  | Color256_153
  | Color256_154
  | Color256_155
  | Color256_156
  | Color256_157
  | Color256_158
  | Color256_159
  | Color256_160
  | Color256_161
  | Color256_162
  | Color256_163
  | Color256_164
  | Color256_165
  | Color256_166
  | Color256_167
  | Color256_168
  | Color256_169
  | Color256_170
  | Color256_171
  | Color256_172
  | Color256_173
  | Color256_174
  | Color256_175
  | Color256_176
  | Color256_177
  | Color256_178
  | Color256_179
  | Color256_180
  | Color256_181
  | Color256_182
  | Color256_183
  | Color256_184
  | Color256_185
  | Color256_186
  | Color256_187
  | Color256_188
  | Color256_189
  | Color256_190
  | Color256_191
  | Color256_192
  | Color256_193
  | Color256_194
  | Color256_195
  | Color256_196
  | Color256_197
  | Color256_198
  | Color256_199
  | Color256_200
  | Color256_201
  | Color256_202
  | Color256_203
  | Color256_204
  | Color256_205
  | Color256_206
  | Color256_207
  | Color256_208
  | Color256_209
  | Color256_210
  | Color256_211
  | Color256_212
  | Color256_213
  | Color256_214
  | Color256_215
  | Color256_216
  | Color256_217
  | Color256_218
  | Color256_219
  | Color256_220
  | Color256_221
  | Color256_222
  | Color256_223
  | Color256_224
  | Color256_225
  | Color256_226
  | Color256_227
  | Color256_228
  | Color256_229
  | Color256_230
  | Color256_231
  | Color256_232
  | Color256_233
  | Color256_234
  | Color256_235
  | Color256_236
  | Color256_237
  | Color256_238
  | Color256_239
  | Color256_240
  | Color256_241
  | Color256_242
  | Color256_243
  | Color256_244
  | Color256_245
  | Color256_246
  | Color256_247
  | Color256_248
  | Color256_249
  | Color256_250
  | Color256_251
  | Color256_252
  | Color256_253
  | Color256_254
  | Color256_255
  deriving (Eq, Show)

instance HasForegroundCode Color256 where
  foregroundCode c = case c of
    Color256_0 -> code "38;5;0"
    Color256_1 -> code "38;5;1"
    Color256_2 -> code "38;5;2"
    Color256_3 -> code "38;5;3"
    Color256_4 -> code "38;5;4"
    Color256_5 -> code "38;5;5"
    Color256_6 -> code "38;5;6"
    Color256_7 -> code "38;5;7"
    Color256_8 -> code "38;5;8"
    Color256_9 -> code "38;5;9"
    Color256_10 -> code "38;5;10"
    Color256_11 -> code "38;5;11"
    Color256_12 -> code "38;5;12"
    Color256_13 -> code "38;5;13"
    Color256_14 -> code "38;5;14"
    Color256_15 -> code "38;5;15"
    Color256_16 -> code "38;5;16"
    Color256_17 -> code "38;5;17"
    Color256_18 -> code "38;5;18"
    Color256_19 -> code "38;5;19"
    Color256_20 -> code "38;5;20"
    Color256_21 -> code "38;5;21"
    Color256_22 -> code "38;5;22"
    Color256_23 -> code "38;5;23"
    Color256_24 -> code "38;5;24"
    Color256_25 -> code "38;5;25"
    Color256_26 -> code "38;5;26"
    Color256_27 -> code "38;5;27"
    Color256_28 -> code "38;5;28"
    Color256_29 -> code "38;5;29"
    Color256_30 -> code "38;5;30"
    Color256_31 -> code "38;5;31"
    Color256_32 -> code "38;5;32"
    Color256_33 -> code "38;5;33"
    Color256_34 -> code "38;5;34"
    Color256_35 -> code "38;5;35"
    Color256_36 -> code "38;5;36"
    Color256_37 -> code "38;5;37"
    Color256_38 -> code "38;5;38"
    Color256_39 -> code "38;5;39"
    Color256_40 -> code "38;5;40"
    Color256_41 -> code "38;5;41"
    Color256_42 -> code "38;5;42"
    Color256_43 -> code "38;5;43"
    Color256_44 -> code "38;5;44"
    Color256_45 -> code "38;5;45"
    Color256_46 -> code "38;5;46"
    Color256_47 -> code "38;5;47"
    Color256_48 -> code "38;5;48"
    Color256_49 -> code "38;5;49"
    Color256_50 -> code "38;5;50"
    Color256_51 -> code "38;5;51"
    Color256_52 -> code "38;5;52"
    Color256_53 -> code "38;5;53"
    Color256_54 -> code "38;5;54"
    Color256_55 -> code "38;5;55"
    Color256_56 -> code "38;5;56"
    Color256_57 -> code "38;5;57"
    Color256_58 -> code "38;5;58"
    Color256_59 -> code "38;5;59"
    Color256_60 -> code "38;5;60"
    Color256_61 -> code "38;5;61"
    Color256_62 -> code "38;5;62"
    Color256_63 -> code "38;5;63"
    Color256_64 -> code "38;5;64"
    Color256_65 -> code "38;5;65"
    Color256_66 -> code "38;5;66"
    Color256_67 -> code "38;5;67"
    Color256_68 -> code "38;5;68"
    Color256_69 -> code "38;5;69"
    Color256_70 -> code "38;5;70"
    Color256_71 -> code "38;5;71"
    Color256_72 -> code "38;5;72"
    Color256_73 -> code "38;5;73"
    Color256_74 -> code "38;5;74"
    Color256_75 -> code "38;5;75"
    Color256_76 -> code "38;5;76"
    Color256_77 -> code "38;5;77"
    Color256_78 -> code "38;5;78"
    Color256_79 -> code "38;5;79"
    Color256_80 -> code "38;5;80"
    Color256_81 -> code "38;5;81"
    Color256_82 -> code "38;5;82"
    Color256_83 -> code "38;5;83"
    Color256_84 -> code "38;5;84"
    Color256_85 -> code "38;5;85"
    Color256_86 -> code "38;5;86"
    Color256_87 -> code "38;5;87"
    Color256_88 -> code "38;5;88"
    Color256_89 -> code "38;5;89"
    Color256_90 -> code "38;5;90"
    Color256_91 -> code "38;5;91"
    Color256_92 -> code "38;5;92"
    Color256_93 -> code "38;5;93"
    Color256_94 -> code "38;5;94"
    Color256_95 -> code "38;5;95"
    Color256_96 -> code "38;5;96"
    Color256_97 -> code "38;5;97"
    Color256_98 -> code "38;5;98"
    Color256_99 -> code "38;5;99"
    Color256_100 -> code "38;5;100"
    Color256_101 -> code "38;5;101"
    Color256_102 -> code "38;5;102"
    Color256_103 -> code "38;5;103"
    Color256_104 -> code "38;5;104"
    Color256_105 -> code "38;5;105"
    Color256_106 -> code "38;5;106"
    Color256_107 -> code "38;5;107"
    Color256_108 -> code "38;5;108"
    Color256_109 -> code "38;5;109"
    Color256_110 -> code "38;5;110"
    Color256_111 -> code "38;5;111"
    Color256_112 -> code "38;5;112"
    Color256_113 -> code "38;5;113"
    Color256_114 -> code "38;5;114"
    Color256_115 -> code "38;5;115"
    Color256_116 -> code "38;5;116"
    Color256_117 -> code "38;5;117"
    Color256_118 -> code "38;5;118"
    Color256_119 -> code "38;5;119"
    Color256_120 -> code "38;5;120"
    Color256_121 -> code "38;5;121"
    Color256_122 -> code "38;5;122"
    Color256_123 -> code "38;5;123"
    Color256_124 -> code "38;5;124"
    Color256_125 -> code "38;5;125"
    Color256_126 -> code "38;5;126"
    Color256_127 -> code "38;5;127"
    Color256_128 -> code "38;5;128"
    Color256_129 -> code "38;5;129"
    Color256_130 -> code "38;5;130"
    Color256_131 -> code "38;5;131"
    Color256_132 -> code "38;5;132"
    Color256_133 -> code "38;5;133"
    Color256_134 -> code "38;5;134"
    Color256_135 -> code "38;5;135"
    Color256_136 -> code "38;5;136"
    Color256_137 -> code "38;5;137"
    Color256_138 -> code "38;5;138"
    Color256_139 -> code "38;5;139"
    Color256_140 -> code "38;5;140"
    Color256_141 -> code "38;5;141"
    Color256_142 -> code "38;5;142"
    Color256_143 -> code "38;5;143"
    Color256_144 -> code "38;5;144"
    Color256_145 -> code "38;5;145"
    Color256_146 -> code "38;5;146"
    Color256_147 -> code "38;5;147"
    Color256_148 -> code "38;5;148"
    Color256_149 -> code "38;5;149"
    Color256_150 -> code "38;5;150"
    Color256_151 -> code "38;5;151"
    Color256_152 -> code "38;5;152"
    Color256_153 -> code "38;5;153"
    Color256_154 -> code "38;5;154"
    Color256_155 -> code "38;5;155"
    Color256_156 -> code "38;5;156"
    Color256_157 -> code "38;5;157"
    Color256_158 -> code "38;5;158"
    Color256_159 -> code "38;5;159"
    Color256_160 -> code "38;5;160"
    Color256_161 -> code "38;5;161"
    Color256_162 -> code "38;5;162"
    Color256_163 -> code "38;5;163"
    Color256_164 -> code "38;5;164"
    Color256_165 -> code "38;5;165"
    Color256_166 -> code "38;5;166"
    Color256_167 -> code "38;5;167"
    Color256_168 -> code "38;5;168"
    Color256_169 -> code "38;5;169"
    Color256_170 -> code "38;5;170"
    Color256_171 -> code "38;5;171"
    Color256_172 -> code "38;5;172"
    Color256_173 -> code "38;5;173"
    Color256_174 -> code "38;5;174"
    Color256_175 -> code "38;5;175"
    Color256_176 -> code "38;5;176"
    Color256_177 -> code "38;5;177"
    Color256_178 -> code "38;5;178"
    Color256_179 -> code "38;5;179"
    Color256_180 -> code "38;5;180"
    Color256_181 -> code "38;5;181"
    Color256_182 -> code "38;5;182"
    Color256_183 -> code "38;5;183"
    Color256_184 -> code "38;5;184"
    Color256_185 -> code "38;5;185"
    Color256_186 -> code "38;5;186"
    Color256_187 -> code "38;5;187"
    Color256_188 -> code "38;5;188"
    Color256_189 -> code "38;5;189"
    Color256_190 -> code "38;5;190"
    Color256_191 -> code "38;5;191"
    Color256_192 -> code "38;5;192"
    Color256_193 -> code "38;5;193"
    Color256_194 -> code "38;5;194"
    Color256_195 -> code "38;5;195"
    Color256_196 -> code "38;5;196"
    Color256_197 -> code "38;5;197"
    Color256_198 -> code "38;5;198"
    Color256_199 -> code "38;5;199"
    Color256_200 -> code "38;5;200"
    Color256_201 -> code "38;5;201"
    Color256_202 -> code "38;5;202"
    Color256_203 -> code "38;5;203"
    Color256_204 -> code "38;5;204"
    Color256_205 -> code "38;5;205"
    Color256_206 -> code "38;5;206"
    Color256_207 -> code "38;5;207"
    Color256_208 -> code "38;5;208"
    Color256_209 -> code "38;5;209"
    Color256_210 -> code "38;5;210"
    Color256_211 -> code "38;5;211"
    Color256_212 -> code "38;5;212"
    Color256_213 -> code "38;5;213"
    Color256_214 -> code "38;5;214"
    Color256_215 -> code "38;5;215"
    Color256_216 -> code "38;5;216"
    Color256_217 -> code "38;5;217"
    Color256_218 -> code "38;5;218"
    Color256_219 -> code "38;5;219"
    Color256_220 -> code "38;5;220"
    Color256_221 -> code "38;5;221"
    Color256_222 -> code "38;5;222"
    Color256_223 -> code "38;5;223"
    Color256_224 -> code "38;5;224"
    Color256_225 -> code "38;5;225"
    Color256_226 -> code "38;5;226"
    Color256_227 -> code "38;5;227"
    Color256_228 -> code "38;5;228"
    Color256_229 -> code "38;5;229"
    Color256_230 -> code "38;5;230"
    Color256_231 -> code "38;5;231"
    Color256_232 -> code "38;5;232"
    Color256_233 -> code "38;5;233"
    Color256_234 -> code "38;5;234"
    Color256_235 -> code "38;5;235"
    Color256_236 -> code "38;5;236"
    Color256_237 -> code "38;5;237"
    Color256_238 -> code "38;5;238"
    Color256_239 -> code "38;5;239"
    Color256_240 -> code "38;5;240"
    Color256_241 -> code "38;5;241"
    Color256_242 -> code "38;5;242"
    Color256_243 -> code "38;5;243"
    Color256_244 -> code "38;5;244"
    Color256_245 -> code "38;5;245"
    Color256_246 -> code "38;5;246"
    Color256_247 -> code "38;5;247"
    Color256_248 -> code "38;5;248"
    Color256_249 -> code "38;5;249"
    Color256_250 -> code "38;5;250"
    Color256_251 -> code "38;5;251"
    Color256_252 -> code "38;5;252"
    Color256_253 -> code "38;5;253"
    Color256_254 -> code "38;5;254"
    Color256_255 -> code "38;5;255"

instance HasBackgroundCode Color256 where
  backgroundCode c = case c of
    Color256_0 -> code "48;5;0"
    Color256_1 -> code "48;5;1"
    Color256_2 -> code "48;5;2"
    Color256_3 -> code "48;5;3"
    Color256_4 -> code "48;5;4"
    Color256_5 -> code "48;5;5"
    Color256_6 -> code "48;5;6"
    Color256_7 -> code "48;5;7"
    Color256_8 -> code "48;5;8"
    Color256_9 -> code "48;5;9"
    Color256_10 -> code "48;5;10"
    Color256_11 -> code "48;5;11"
    Color256_12 -> code "48;5;12"
    Color256_13 -> code "48;5;13"
    Color256_14 -> code "48;5;14"
    Color256_15 -> code "48;5;15"
    Color256_16 -> code "48;5;16"
    Color256_17 -> code "48;5;17"
    Color256_18 -> code "48;5;18"
    Color256_19 -> code "48;5;19"
    Color256_20 -> code "48;5;20"
    Color256_21 -> code "48;5;21"
    Color256_22 -> code "48;5;22"
    Color256_23 -> code "48;5;23"
    Color256_24 -> code "48;5;24"
    Color256_25 -> code "48;5;25"
    Color256_26 -> code "48;5;26"
    Color256_27 -> code "48;5;27"
    Color256_28 -> code "48;5;28"
    Color256_29 -> code "48;5;29"
    Color256_30 -> code "48;5;30"
    Color256_31 -> code "48;5;31"
    Color256_32 -> code "48;5;32"
    Color256_33 -> code "48;5;33"
    Color256_34 -> code "48;5;34"
    Color256_35 -> code "48;5;35"
    Color256_36 -> code "48;5;36"
    Color256_37 -> code "48;5;37"
    Color256_38 -> code "48;5;38"
    Color256_39 -> code "48;5;39"
    Color256_40 -> code "48;5;40"
    Color256_41 -> code "48;5;41"
    Color256_42 -> code "48;5;42"
    Color256_43 -> code "48;5;43"
    Color256_44 -> code "48;5;44"
    Color256_45 -> code "48;5;45"
    Color256_46 -> code "48;5;46"
    Color256_47 -> code "48;5;47"
    Color256_48 -> code "48;5;48"
    Color256_49 -> code "48;5;49"
    Color256_50 -> code "48;5;50"
    Color256_51 -> code "48;5;51"
    Color256_52 -> code "48;5;52"
    Color256_53 -> code "48;5;53"
    Color256_54 -> code "48;5;54"
    Color256_55 -> code "48;5;55"
    Color256_56 -> code "48;5;56"
    Color256_57 -> code "48;5;57"
    Color256_58 -> code "48;5;58"
    Color256_59 -> code "48;5;59"
    Color256_60 -> code "48;5;60"
    Color256_61 -> code "48;5;61"
    Color256_62 -> code "48;5;62"
    Color256_63 -> code "48;5;63"
    Color256_64 -> code "48;5;64"
    Color256_65 -> code "48;5;65"
    Color256_66 -> code "48;5;66"
    Color256_67 -> code "48;5;67"
    Color256_68 -> code "48;5;68"
    Color256_69 -> code "48;5;69"
    Color256_70 -> code "48;5;70"
    Color256_71 -> code "48;5;71"
    Color256_72 -> code "48;5;72"
    Color256_73 -> code "48;5;73"
    Color256_74 -> code "48;5;74"
    Color256_75 -> code "48;5;75"
    Color256_76 -> code "48;5;76"
    Color256_77 -> code "48;5;77"
    Color256_78 -> code "48;5;78"
    Color256_79 -> code "48;5;79"
    Color256_80 -> code "48;5;80"
    Color256_81 -> code "48;5;81"
    Color256_82 -> code "48;5;82"
    Color256_83 -> code "48;5;83"
    Color256_84 -> code "48;5;84"
    Color256_85 -> code "48;5;85"
    Color256_86 -> code "48;5;86"
    Color256_87 -> code "48;5;87"
    Color256_88 -> code "48;5;88"
    Color256_89 -> code "48;5;89"
    Color256_90 -> code "48;5;90"
    Color256_91 -> code "48;5;91"
    Color256_92 -> code "48;5;92"
    Color256_93 -> code "48;5;93"
    Color256_94 -> code "48;5;94"
    Color256_95 -> code "48;5;95"
    Color256_96 -> code "48;5;96"
    Color256_97 -> code "48;5;97"
    Color256_98 -> code "48;5;98"
    Color256_99 -> code "48;5;99"
    Color256_100 -> code "48;5;100"
    Color256_101 -> code "48;5;101"
    Color256_102 -> code "48;5;102"
    Color256_103 -> code "48;5;103"
    Color256_104 -> code "48;5;104"
    Color256_105 -> code "48;5;105"
    Color256_106 -> code "48;5;106"
    Color256_107 -> code "48;5;107"
    Color256_108 -> code "48;5;108"
    Color256_109 -> code "48;5;109"
    Color256_110 -> code "48;5;110"
    Color256_111 -> code "48;5;111"
    Color256_112 -> code "48;5;112"
    Color256_113 -> code "48;5;113"
    Color256_114 -> code "48;5;114"
    Color256_115 -> code "48;5;115"
    Color256_116 -> code "48;5;116"
    Color256_117 -> code "48;5;117"
    Color256_118 -> code "48;5;118"
    Color256_119 -> code "48;5;119"
    Color256_120 -> code "48;5;120"
    Color256_121 -> code "48;5;121"
    Color256_122 -> code "48;5;122"
    Color256_123 -> code "48;5;123"
    Color256_124 -> code "48;5;124"
    Color256_125 -> code "48;5;125"
    Color256_126 -> code "48;5;126"
    Color256_127 -> code "48;5;127"
    Color256_128 -> code "48;5;128"
    Color256_129 -> code "48;5;129"
    Color256_130 -> code "48;5;130"
    Color256_131 -> code "48;5;131"
    Color256_132 -> code "48;5;132"
    Color256_133 -> code "48;5;133"
    Color256_134 -> code "48;5;134"
    Color256_135 -> code "48;5;135"
    Color256_136 -> code "48;5;136"
    Color256_137 -> code "48;5;137"
    Color256_138 -> code "48;5;138"
    Color256_139 -> code "48;5;139"
    Color256_140 -> code "48;5;140"
    Color256_141 -> code "48;5;141"
    Color256_142 -> code "48;5;142"
    Color256_143 -> code "48;5;143"
    Color256_144 -> code "48;5;144"
    Color256_145 -> code "48;5;145"
    Color256_146 -> code "48;5;146"
    Color256_147 -> code "48;5;147"
    Color256_148 -> code "48;5;148"
    Color256_149 -> code "48;5;149"
    Color256_150 -> code "48;5;150"
    Color256_151 -> code "48;5;151"
    Color256_152 -> code "48;5;152"
    Color256_153 -> code "48;5;153"
    Color256_154 -> code "48;5;154"
    Color256_155 -> code "48;5;155"
    Color256_156 -> code "48;5;156"
    Color256_157 -> code "48;5;157"
    Color256_158 -> code "48;5;158"
    Color256_159 -> code "48;5;159"
    Color256_160 -> code "48;5;160"
    Color256_161 -> code "48;5;161"
    Color256_162 -> code "48;5;162"
    Color256_163 -> code "48;5;163"
    Color256_164 -> code "48;5;164"
    Color256_165 -> code "48;5;165"
    Color256_166 -> code "48;5;166"
    Color256_167 -> code "48;5;167"
    Color256_168 -> code "48;5;168"
    Color256_169 -> code "48;5;169"
    Color256_170 -> code "48;5;170"
    Color256_171 -> code "48;5;171"
    Color256_172 -> code "48;5;172"
    Color256_173 -> code "48;5;173"
    Color256_174 -> code "48;5;174"
    Color256_175 -> code "48;5;175"
    Color256_176 -> code "48;5;176"
    Color256_177 -> code "48;5;177"
    Color256_178 -> code "48;5;178"
    Color256_179 -> code "48;5;179"
    Color256_180 -> code "48;5;180"
    Color256_181 -> code "48;5;181"
    Color256_182 -> code "48;5;182"
    Color256_183 -> code "48;5;183"
    Color256_184 -> code "48;5;184"
    Color256_185 -> code "48;5;185"
    Color256_186 -> code "48;5;186"
    Color256_187 -> code "48;5;187"
    Color256_188 -> code "48;5;188"
    Color256_189 -> code "48;5;189"
    Color256_190 -> code "48;5;190"
    Color256_191 -> code "48;5;191"
    Color256_192 -> code "48;5;192"
    Color256_193 -> code "48;5;193"
    Color256_194 -> code "48;5;194"
    Color256_195 -> code "48;5;195"
    Color256_196 -> code "48;5;196"
    Color256_197 -> code "48;5;197"
    Color256_198 -> code "48;5;198"
    Color256_199 -> code "48;5;199"
    Color256_200 -> code "48;5;200"
    Color256_201 -> code "48;5;201"
    Color256_202 -> code "48;5;202"
    Color256_203 -> code "48;5;203"
    Color256_204 -> code "48;5;204"
    Color256_205 -> code "48;5;205"
    Color256_206 -> code "48;5;206"
    Color256_207 -> code "48;5;207"
    Color256_208 -> code "48;5;208"
    Color256_209 -> code "48;5;209"
    Color256_210 -> code "48;5;210"
    Color256_211 -> code "48;5;211"
    Color256_212 -> code "48;5;212"
    Color256_213 -> code "48;5;213"
    Color256_214 -> code "48;5;214"
    Color256_215 -> code "48;5;215"
    Color256_216 -> code "48;5;216"
    Color256_217 -> code "48;5;217"
    Color256_218 -> code "48;5;218"
    Color256_219 -> code "48;5;219"
    Color256_220 -> code "48;5;220"
    Color256_221 -> code "48;5;221"
    Color256_222 -> code "48;5;222"
    Color256_223 -> code "48;5;223"
    Color256_224 -> code "48;5;224"
    Color256_225 -> code "48;5;225"
    Color256_226 -> code "48;5;226"
    Color256_227 -> code "48;5;227"
    Color256_228 -> code "48;5;228"
    Color256_229 -> code "48;5;229"
    Color256_230 -> code "48;5;230"
    Color256_231 -> code "48;5;231"
    Color256_232 -> code "48;5;232"
    Color256_233 -> code "48;5;233"
    Color256_234 -> code "48;5;234"
    Color256_235 -> code "48;5;235"
    Color256_236 -> code "48;5;236"
    Color256_237 -> code "48;5;237"
    Color256_238 -> code "48;5;238"
    Color256_239 -> code "48;5;239"
    Color256_240 -> code "48;5;240"
    Color256_241 -> code "48;5;241"
    Color256_242 -> code "48;5;242"
    Color256_243 -> code "48;5;243"
    Color256_244 -> code "48;5;244"
    Color256_245 -> code "48;5;245"
    Color256_246 -> code "48;5;246"
    Color256_247 -> code "48;5;247"
    Color256_248 -> code "48;5;248"
    Color256_249 -> code "48;5;249"
    Color256_250 -> code "48;5;250"
    Color256_251 -> code "48;5;251"
    Color256_252 -> code "48;5;252"
    Color256_253 -> code "48;5;253"
    Color256_254 -> code "48;5;254"
    Color256_255 -> code "48;5;255"

color256_0 :: Color Color256
color256_0 = Color Color256_0

color256_1 :: Color Color256
color256_1 = Color Color256_1

color256_2 :: Color Color256
color256_2 = Color Color256_2

color256_3 :: Color Color256
color256_3 = Color Color256_3

color256_4 :: Color Color256
color256_4 = Color Color256_4

color256_5 :: Color Color256
color256_5 = Color Color256_5

color256_6 :: Color Color256
color256_6 = Color Color256_6

color256_7 :: Color Color256
color256_7 = Color Color256_7

color256_8 :: Color Color256
color256_8 = Color Color256_8

color256_9 :: Color Color256
color256_9 = Color Color256_9

color256_10 :: Color Color256
color256_10 = Color Color256_10

color256_11 :: Color Color256
color256_11 = Color Color256_11

color256_12 :: Color Color256
color256_12 = Color Color256_12

color256_13 :: Color Color256
color256_13 = Color Color256_13

color256_14 :: Color Color256
color256_14 = Color Color256_14

color256_15 :: Color Color256
color256_15 = Color Color256_15

color256_16 :: Color Color256
color256_16 = Color Color256_16

color256_17 :: Color Color256
color256_17 = Color Color256_17

color256_18 :: Color Color256
color256_18 = Color Color256_18

color256_19 :: Color Color256
color256_19 = Color Color256_19

color256_20 :: Color Color256
color256_20 = Color Color256_20

color256_21 :: Color Color256
color256_21 = Color Color256_21

color256_22 :: Color Color256
color256_22 = Color Color256_22

color256_23 :: Color Color256
color256_23 = Color Color256_23

color256_24 :: Color Color256
color256_24 = Color Color256_24

color256_25 :: Color Color256
color256_25 = Color Color256_25

color256_26 :: Color Color256
color256_26 = Color Color256_26

color256_27 :: Color Color256
color256_27 = Color Color256_27

color256_28 :: Color Color256
color256_28 = Color Color256_28

color256_29 :: Color Color256
color256_29 = Color Color256_29

color256_30 :: Color Color256
color256_30 = Color Color256_30

color256_31 :: Color Color256
color256_31 = Color Color256_31

color256_32 :: Color Color256
color256_32 = Color Color256_32

color256_33 :: Color Color256
color256_33 = Color Color256_33

color256_34 :: Color Color256
color256_34 = Color Color256_34

color256_35 :: Color Color256
color256_35 = Color Color256_35

color256_36 :: Color Color256
color256_36 = Color Color256_36

color256_37 :: Color Color256
color256_37 = Color Color256_37

color256_38 :: Color Color256
color256_38 = Color Color256_38

color256_39 :: Color Color256
color256_39 = Color Color256_39

color256_40 :: Color Color256
color256_40 = Color Color256_40

color256_41 :: Color Color256
color256_41 = Color Color256_41

color256_42 :: Color Color256
color256_42 = Color Color256_42

color256_43 :: Color Color256
color256_43 = Color Color256_43

color256_44 :: Color Color256
color256_44 = Color Color256_44

color256_45 :: Color Color256
color256_45 = Color Color256_45

color256_46 :: Color Color256
color256_46 = Color Color256_46

color256_47 :: Color Color256
color256_47 = Color Color256_47

color256_48 :: Color Color256
color256_48 = Color Color256_48

color256_49 :: Color Color256
color256_49 = Color Color256_49

color256_50 :: Color Color256
color256_50 = Color Color256_50

color256_51 :: Color Color256
color256_51 = Color Color256_51

color256_52 :: Color Color256
color256_52 = Color Color256_52

color256_53 :: Color Color256
color256_53 = Color Color256_53

color256_54 :: Color Color256
color256_54 = Color Color256_54

color256_55 :: Color Color256
color256_55 = Color Color256_55

color256_56 :: Color Color256
color256_56 = Color Color256_56

color256_57 :: Color Color256
color256_57 = Color Color256_57

color256_58 :: Color Color256
color256_58 = Color Color256_58

color256_59 :: Color Color256
color256_59 = Color Color256_59

color256_60 :: Color Color256
color256_60 = Color Color256_60

color256_61 :: Color Color256
color256_61 = Color Color256_61

color256_62 :: Color Color256
color256_62 = Color Color256_62

color256_63 :: Color Color256
color256_63 = Color Color256_63

color256_64 :: Color Color256
color256_64 = Color Color256_64

color256_65 :: Color Color256
color256_65 = Color Color256_65

color256_66 :: Color Color256
color256_66 = Color Color256_66

color256_67 :: Color Color256
color256_67 = Color Color256_67

color256_68 :: Color Color256
color256_68 = Color Color256_68

color256_69 :: Color Color256
color256_69 = Color Color256_69

color256_70 :: Color Color256
color256_70 = Color Color256_70

color256_71 :: Color Color256
color256_71 = Color Color256_71

color256_72 :: Color Color256
color256_72 = Color Color256_72

color256_73 :: Color Color256
color256_73 = Color Color256_73

color256_74 :: Color Color256
color256_74 = Color Color256_74

color256_75 :: Color Color256
color256_75 = Color Color256_75

color256_76 :: Color Color256
color256_76 = Color Color256_76

color256_77 :: Color Color256
color256_77 = Color Color256_77

color256_78 :: Color Color256
color256_78 = Color Color256_78

color256_79 :: Color Color256
color256_79 = Color Color256_79

color256_80 :: Color Color256
color256_80 = Color Color256_80

color256_81 :: Color Color256
color256_81 = Color Color256_81

color256_82 :: Color Color256
color256_82 = Color Color256_82

color256_83 :: Color Color256
color256_83 = Color Color256_83

color256_84 :: Color Color256
color256_84 = Color Color256_84

color256_85 :: Color Color256
color256_85 = Color Color256_85

color256_86 :: Color Color256
color256_86 = Color Color256_86

color256_87 :: Color Color256
color256_87 = Color Color256_87

color256_88 :: Color Color256
color256_88 = Color Color256_88

color256_89 :: Color Color256
color256_89 = Color Color256_89

color256_90 :: Color Color256
color256_90 = Color Color256_90

color256_91 :: Color Color256
color256_91 = Color Color256_91

color256_92 :: Color Color256
color256_92 = Color Color256_92

color256_93 :: Color Color256
color256_93 = Color Color256_93

color256_94 :: Color Color256
color256_94 = Color Color256_94

color256_95 :: Color Color256
color256_95 = Color Color256_95

color256_96 :: Color Color256
color256_96 = Color Color256_96

color256_97 :: Color Color256
color256_97 = Color Color256_97

color256_98 :: Color Color256
color256_98 = Color Color256_98

color256_99 :: Color Color256
color256_99 = Color Color256_99

color256_100 :: Color Color256
color256_100 = Color Color256_100

color256_101 :: Color Color256
color256_101 = Color Color256_101

color256_102 :: Color Color256
color256_102 = Color Color256_102

color256_103 :: Color Color256
color256_103 = Color Color256_103

color256_104 :: Color Color256
color256_104 = Color Color256_104

color256_105 :: Color Color256
color256_105 = Color Color256_105

color256_106 :: Color Color256
color256_106 = Color Color256_106

color256_107 :: Color Color256
color256_107 = Color Color256_107

color256_108 :: Color Color256
color256_108 = Color Color256_108

color256_109 :: Color Color256
color256_109 = Color Color256_109

color256_110 :: Color Color256
color256_110 = Color Color256_110

color256_111 :: Color Color256
color256_111 = Color Color256_111

color256_112 :: Color Color256
color256_112 = Color Color256_112

color256_113 :: Color Color256
color256_113 = Color Color256_113

color256_114 :: Color Color256
color256_114 = Color Color256_114

color256_115 :: Color Color256
color256_115 = Color Color256_115

color256_116 :: Color Color256
color256_116 = Color Color256_116

color256_117 :: Color Color256
color256_117 = Color Color256_117

color256_118 :: Color Color256
color256_118 = Color Color256_118

color256_119 :: Color Color256
color256_119 = Color Color256_119

color256_120 :: Color Color256
color256_120 = Color Color256_120

color256_121 :: Color Color256
color256_121 = Color Color256_121

color256_122 :: Color Color256
color256_122 = Color Color256_122

color256_123 :: Color Color256
color256_123 = Color Color256_123

color256_124 :: Color Color256
color256_124 = Color Color256_124

color256_125 :: Color Color256
color256_125 = Color Color256_125

color256_126 :: Color Color256
color256_126 = Color Color256_126

color256_127 :: Color Color256
color256_127 = Color Color256_127

color256_128 :: Color Color256
color256_128 = Color Color256_128

color256_129 :: Color Color256
color256_129 = Color Color256_129

color256_130 :: Color Color256
color256_130 = Color Color256_130

color256_131 :: Color Color256
color256_131 = Color Color256_131

color256_132 :: Color Color256
color256_132 = Color Color256_132

color256_133 :: Color Color256
color256_133 = Color Color256_133

color256_134 :: Color Color256
color256_134 = Color Color256_134

color256_135 :: Color Color256
color256_135 = Color Color256_135

color256_136 :: Color Color256
color256_136 = Color Color256_136

color256_137 :: Color Color256
color256_137 = Color Color256_137

color256_138 :: Color Color256
color256_138 = Color Color256_138

color256_139 :: Color Color256
color256_139 = Color Color256_139

color256_140 :: Color Color256
color256_140 = Color Color256_140

color256_141 :: Color Color256
color256_141 = Color Color256_141

color256_142 :: Color Color256
color256_142 = Color Color256_142

color256_143 :: Color Color256
color256_143 = Color Color256_143

color256_144 :: Color Color256
color256_144 = Color Color256_144

color256_145 :: Color Color256
color256_145 = Color Color256_145

color256_146 :: Color Color256
color256_146 = Color Color256_146

color256_147 :: Color Color256
color256_147 = Color Color256_147

color256_148 :: Color Color256
color256_148 = Color Color256_148

color256_149 :: Color Color256
color256_149 = Color Color256_149

color256_150 :: Color Color256
color256_150 = Color Color256_150

color256_151 :: Color Color256
color256_151 = Color Color256_151

color256_152 :: Color Color256
color256_152 = Color Color256_152

color256_153 :: Color Color256
color256_153 = Color Color256_153

color256_154 :: Color Color256
color256_154 = Color Color256_154

color256_155 :: Color Color256
color256_155 = Color Color256_155

color256_156 :: Color Color256
color256_156 = Color Color256_156

color256_157 :: Color Color256
color256_157 = Color Color256_157

color256_158 :: Color Color256
color256_158 = Color Color256_158

color256_159 :: Color Color256
color256_159 = Color Color256_159

color256_160 :: Color Color256
color256_160 = Color Color256_160

color256_161 :: Color Color256
color256_161 = Color Color256_161

color256_162 :: Color Color256
color256_162 = Color Color256_162

color256_163 :: Color Color256
color256_163 = Color Color256_163

color256_164 :: Color Color256
color256_164 = Color Color256_164

color256_165 :: Color Color256
color256_165 = Color Color256_165

color256_166 :: Color Color256
color256_166 = Color Color256_166

color256_167 :: Color Color256
color256_167 = Color Color256_167

color256_168 :: Color Color256
color256_168 = Color Color256_168

color256_169 :: Color Color256
color256_169 = Color Color256_169

color256_170 :: Color Color256
color256_170 = Color Color256_170

color256_171 :: Color Color256
color256_171 = Color Color256_171

color256_172 :: Color Color256
color256_172 = Color Color256_172

color256_173 :: Color Color256
color256_173 = Color Color256_173

color256_174 :: Color Color256
color256_174 = Color Color256_174

color256_175 :: Color Color256
color256_175 = Color Color256_175

color256_176 :: Color Color256
color256_176 = Color Color256_176

color256_177 :: Color Color256
color256_177 = Color Color256_177

color256_178 :: Color Color256
color256_178 = Color Color256_178

color256_179 :: Color Color256
color256_179 = Color Color256_179

color256_180 :: Color Color256
color256_180 = Color Color256_180

color256_181 :: Color Color256
color256_181 = Color Color256_181

color256_182 :: Color Color256
color256_182 = Color Color256_182

color256_183 :: Color Color256
color256_183 = Color Color256_183

color256_184 :: Color Color256
color256_184 = Color Color256_184

color256_185 :: Color Color256
color256_185 = Color Color256_185

color256_186 :: Color Color256
color256_186 = Color Color256_186

color256_187 :: Color Color256
color256_187 = Color Color256_187

color256_188 :: Color Color256
color256_188 = Color Color256_188

color256_189 :: Color Color256
color256_189 = Color Color256_189

color256_190 :: Color Color256
color256_190 = Color Color256_190

color256_191 :: Color Color256
color256_191 = Color Color256_191

color256_192 :: Color Color256
color256_192 = Color Color256_192

color256_193 :: Color Color256
color256_193 = Color Color256_193

color256_194 :: Color Color256
color256_194 = Color Color256_194

color256_195 :: Color Color256
color256_195 = Color Color256_195

color256_196 :: Color Color256
color256_196 = Color Color256_196

color256_197 :: Color Color256
color256_197 = Color Color256_197

color256_198 :: Color Color256
color256_198 = Color Color256_198

color256_199 :: Color Color256
color256_199 = Color Color256_199

color256_200 :: Color Color256
color256_200 = Color Color256_200

color256_201 :: Color Color256
color256_201 = Color Color256_201

color256_202 :: Color Color256
color256_202 = Color Color256_202

color256_203 :: Color Color256
color256_203 = Color Color256_203

color256_204 :: Color Color256
color256_204 = Color Color256_204

color256_205 :: Color Color256
color256_205 = Color Color256_205

color256_206 :: Color Color256
color256_206 = Color Color256_206

color256_207 :: Color Color256
color256_207 = Color Color256_207

color256_208 :: Color Color256
color256_208 = Color Color256_208

color256_209 :: Color Color256
color256_209 = Color Color256_209

color256_210 :: Color Color256
color256_210 = Color Color256_210

color256_211 :: Color Color256
color256_211 = Color Color256_211

color256_212 :: Color Color256
color256_212 = Color Color256_212

color256_213 :: Color Color256
color256_213 = Color Color256_213

color256_214 :: Color Color256
color256_214 = Color Color256_214

color256_215 :: Color Color256
color256_215 = Color Color256_215

color256_216 :: Color Color256
color256_216 = Color Color256_216

color256_217 :: Color Color256
color256_217 = Color Color256_217

color256_218 :: Color Color256
color256_218 = Color Color256_218

color256_219 :: Color Color256
color256_219 = Color Color256_219

color256_220 :: Color Color256
color256_220 = Color Color256_220

color256_221 :: Color Color256
color256_221 = Color Color256_221

color256_222 :: Color Color256
color256_222 = Color Color256_222

color256_223 :: Color Color256
color256_223 = Color Color256_223

color256_224 :: Color Color256
color256_224 = Color Color256_224

color256_225 :: Color Color256
color256_225 = Color Color256_225

color256_226 :: Color Color256
color256_226 = Color Color256_226

color256_227 :: Color Color256
color256_227 = Color Color256_227

color256_228 :: Color Color256
color256_228 = Color Color256_228

color256_229 :: Color Color256
color256_229 = Color Color256_229

color256_230 :: Color Color256
color256_230 = Color Color256_230

color256_231 :: Color Color256
color256_231 = Color Color256_231

color256_232 :: Color Color256
color256_232 = Color Color256_232

color256_233 :: Color Color256
color256_233 = Color Color256_233

color256_234 :: Color Color256
color256_234 = Color Color256_234

color256_235 :: Color Color256
color256_235 = Color Color256_235

color256_236 :: Color Color256
color256_236 = Color Color256_236

color256_237 :: Color Color256
color256_237 = Color Color256_237

color256_238 :: Color Color256
color256_238 = Color Color256_238

color256_239 :: Color Color256
color256_239 = Color Color256_239

color256_240 :: Color Color256
color256_240 = Color Color256_240

color256_241 :: Color Color256
color256_241 = Color Color256_241

color256_242 :: Color Color256
color256_242 = Color Color256_242

color256_243 :: Color Color256
color256_243 = Color Color256_243

color256_244 :: Color Color256
color256_244 = Color Color256_244

color256_245 :: Color Color256
color256_245 = Color Color256_245

color256_246 :: Color Color256
color256_246 = Color Color256_246

color256_247 :: Color Color256
color256_247 = Color Color256_247

color256_248 :: Color Color256
color256_248 = Color Color256_248

color256_249 :: Color Color256
color256_249 = Color Color256_249

color256_250 :: Color Color256
color256_250 = Color Color256_250

color256_251 :: Color Color256
color256_251 = Color Color256_251

color256_252 :: Color Color256
color256_252 = Color Color256_252

color256_253 :: Color Color256
color256_253 = Color Color256_253

color256_254 :: Color Color256
color256_254 = Color Color256_254

color256_255 :: Color Color256
color256_255 = Color Color256_255
