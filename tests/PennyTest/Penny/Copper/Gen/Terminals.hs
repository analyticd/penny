module PennyTest.Penny.Copper.Gen.Terminals where

import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Penny.Copper.Terminals as T

invalid :: Gen Char
invalid = G.choose ('\xD800', '\xDFFF')

unicode :: Gen Char
unicode = G.suchThat (G.choose (minBound, maxBound)) (not . T.invalid)

newline :: Gen Char
newline = return '\x0A'

space :: Gen Char
space = return '\x20'

tab :: Gen Char
tab = return '\x09'

white :: Gen Char
white = G.oneof [space, tab]

nonNewline :: Gen Char
nonNewline = G.suchThat unicode (not . T.newline)

nonNewlineNonSpace :: Gen Char
nonNewlineNonSpace = G.suchThat nonNewline (not . T.white)

upperCaseAscii :: Gen Char
upperCaseAscii = G.choose ('A', 'Z')

lowerCaseAscii :: Gen Char
lowerCaseAscii = G.choose ('a', 'z')

digit :: Gen Char
digit = G.choose ('0', '9')

nonAscii :: Gen Char
nonAscii = G.suchThat nonNewline (> '\x7f')

letter :: Gen Char
letter = G.oneof [upperCaseAscii, lowerCaseAscii, nonAscii]

dollar :: Gen Char
dollar = return '$'

colon :: Gen Char
colon = return ':'

openCurly :: Gen Char
openCurly = return '{'

closeCurly :: Gen Char
closeCurly = return '}'

openSquare :: Gen Char
openSquare = return '['

closeSquare :: Gen Char
closeSquare = return ']'

doubleQuote :: Gen Char
doubleQuote = return '"'

period :: Gen Char
period = return '.'

hash :: Gen Char
hash = return '#'

thinSpace :: Gen Char
thinSpace = return '\x2009'

plus :: Gen Char
plus = return '+'

minus :: Gen Char
minus = return '-'

lessThan :: Gen Char
lessThan = return '<'

greaterThan :: Gen Char
greaterThan = return '>'

openParen :: Gen Char
openParen = return '('

closeParen :: Gen Char
closeParen = return ')'

semicolon :: Gen Char
semicolon = return ';'

apostrophe :: Gen Char
apostrophe = return '\x27'

tilde :: Gen Char
tilde = return '~'

underscore :: Gen Char
underscore = return '_'

asterisk :: Gen Char
asterisk = return '*'

lvl1AcctChar :: Gen Char
lvl1AcctChar = G.suchThat nonNewline (T.closeCurly !!! T.colon)

lvl2AcctOtherChar :: Gen Char
lvl2AcctOtherChar = G.suchThat nonNewline (T.white !!! T.colon)


lvl1CmdtyChar :: Gen Char
lvl1CmdtyChar = G.suchThat nonNewline (not . T.doubleQuote)

lvl2CmdtyFirstChar :: Gen Char
lvl2CmdtyFirstChar = G.oneof [letter, dollar]

lvl2CmdtyOtherChar :: Gen Char
lvl2CmdtyOtherChar = G.suchThat nonNewline (not . T.white)

(!!!) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(!!!) f1 f2 a = (not . f1 $ a) && (not . f2 $ a)

infixr 2 !!!

lvl3CmdtyChar :: Gen Char
lvl3CmdtyChar = G.oneof [letter, dollar]

flagChar :: Gen Char
flagChar = G.suchThat nonNewline (not . T.closeSquare)

numberChar :: Gen Char
numberChar = G.suchThat nonNewline (not . T.closeParen)

quotedPayeeChar :: Gen Char
quotedPayeeChar = G.suchThat nonNewline (not . T.tilde)

tagChar :: Gen Char
tagChar = G.suchThat nonNewline (not . T.asterisk)
