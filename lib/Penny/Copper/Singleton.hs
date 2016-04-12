-- | Productions for which there is only one valid value.
module Penny.Copper.Singleton where

import Penny.Copper.Types

sZero :: Zero Char ()
sZero = Zero ('0', ())

sOne :: One Char ()
sOne = One ('1', ())

sTwo :: Two Char ()
sTwo = Two ('2', ())

sThree :: Three Char ()
sThree = Three ('3', ())

sFour :: Four Char ()
sFour = Four ('4', ())

sFive :: Five Char ()
sFive = Five ('5', ())

sSix :: Six Char ()
sSix = Six ('6', ())

sSeven :: Seven Char ()
sSeven = Seven ('7', ())

sEight :: Eight Char ()
sEight = Eight ('8', ())

sNine :: Nine Char ()
sNine = Nine ('9', ())

sThinSpace :: ThinSpace Char ()
sThinSpace = ThinSpace ('\x2009', ())

sUnderscore :: Underscore Char ()
sUnderscore = Underscore ('_', ())

sPeriod :: Period Char ()
sPeriod = Period ('.', ())

sComma :: Comma Char ()
sComma = Comma (',', ())

sRadixCom :: RadixCom Char ()
sRadixCom = RadixCom (',', ())

sRadixPer :: RadixPer Char ()
sRadixPer = RadixPer ('.', ())

sHyphen :: Hyphen Char ()
sHyphen = Hyphen ('-', ())

sSlash :: Slash Char ()
sSlash = Slash ('/', ())

sNewline :: Newline Char ()
sNewline = Newline ('\n', ())

sHash :: Hash Char ()
sHash = Hash ('#', ())

sColon :: Colon Char ()
sColon = Colon (':', ())

sPlus :: Plus Char ()
sPlus = Plus ('+', ())

sMinus :: Minus Char ()
sMinus = Minus ('-', ())

sBacktick :: Backtick Char ()
sBacktick = Backtick ('`', ())

sDoubleQuote :: DoubleQuote Char ()
sDoubleQuote = DoubleQuote ('"', ())

sBackslash :: Backslash Char ()
sBackslash = Backslash ('\\', ())

sSpace :: Space Char ()
sSpace = Space (' ', ())

sTab :: Tab Char ()
sTab = Tab ('\t', ())

sLessThan :: LessThan Char ()
sLessThan = LessThan ('<', ())

sGreaterThan :: GreaterThan Char ()
sGreaterThan = GreaterThan ('>', ())

sOpenSquare :: OpenSquare Char ()
sOpenSquare = OpenSquare (']', ())

sCloseSquare :: CloseSquare Char ()
sCloseSquare = CloseSquare (']', ())

sOpenCurly :: OpenCurly Char ()
sOpenCurly = OpenCurly ('{', ())

sCloseCurly :: CloseCurly Char ()
sCloseCurly = CloseCurly ('}', ())

sSemicolon :: Semicolon Char ()
sSemicolon = Semicolon (';', ())

sAtSign :: AtSign Char ()
sAtSign = AtSign ('@', ())

