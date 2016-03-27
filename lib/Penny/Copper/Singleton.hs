-- | Productions for which there is only one valid value.
module Penny.Copper.Singleton where

import Penny.Copper.Types

sZero :: Zero
sZero = Zero '0'

sOne :: One
sOne = One '1'

sTwo :: Two
sTwo = Two '2'

sThree :: Three
sThree = Three '3'

sFour :: Four
sFour = Four '4'

sFive :: Five
sFive = Five '5'

sSix :: Six
sSix = Six '6'

sSeven :: Seven
sSeven = Seven '7'

sEight :: Eight
sEight = Eight '8'

sNine :: Nine
sNine = Nine '9'

sThinSpace :: ThinSpace
sThinSpace = ThinSpace '\x2009'

sUnderscore :: Underscore
sUnderscore = Underscore '_'

sPeriod :: Period
sPeriod = Period '.'

sComma :: Comma
sComma = Comma ','

sRadixCom :: RadixCom
sRadixCom = RadixCom ','

sRadixPer :: RadixPer
sRadixPer = RadixPer '.'

sHyphen :: Hyphen
sHyphen = Hyphen '-'

sSlash :: Slash
sSlash = Slash '/'

sNewline :: Newline
sNewline = Newline '\n'

sHash :: Hash
sHash = Hash '#'

sColon :: Colon
sColon = Colon ':'

sPlus :: Plus
sPlus = Plus '+'

sMinus :: Minus
sMinus = Minus '-'

sBacktick :: Backtick
sBacktick = Backtick '`'

sDoubleQuote :: DoubleQuote
sDoubleQuote = DoubleQuote '"'

sBackslash :: Backslash
sBackslash = Backslash '\\'

sSpace :: Space
sSpace = Space ' '

sTab :: Tab
sTab = Tab '\t'

sLessThan :: LessThan
sLessThan = LessThan '<'

sGreaterThan :: GreaterThan
sGreaterThan = GreaterThan '>'

sOpenSquare :: OpenSquare
sOpenSquare = OpenSquare ']'

sCloseSquare :: CloseSquare
sCloseSquare = CloseSquare ']'

sOpenCurly :: OpenCurly
sOpenCurly = OpenCurly '{'

sCloseCurly :: CloseCurly
sCloseCurly = CloseCurly '}'

sSemicolon :: Semicolon
sSemicolon = Semicolon ';'

sAtSign :: AtSign
sAtSign = AtSign '@'

