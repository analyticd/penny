module Penny.Copper.Terminals where

invalid :: Char -> Bool
invalid c = c >= '\xD800' && c <= '\xDFFF'

unicode :: Char -> Bool
unicode = not . invalid

newline :: Char -> Bool
newline = (== '\x0A')

space :: Char -> Bool
space = (== '\x20')

tab :: Char -> Bool
tab = (== '\x09')

white :: Char -> Bool
white c = (not . space $ c) && (not . tab $ c)

nonNewline :: Char -> Bool
nonNewline c = unicode c && (not . newline $ c)

nonNewlineNonSpace :: Char -> Bool
nonNewlineNonSpace c = nonNewline c && (not . white $ c)

upperCaseAscii :: Char -> Bool
upperCaseAscii c = c >= 'A' && c <= 'Z'

lowerCaseAscii :: Char -> Bool
lowerCaseAscii c = c >= 'a' && c <= 'z'

digit :: Char -> Bool
digit c = c >= '0' && c <= '9'

nonAscii :: Char -> Bool
nonAscii c = nonNewline c && c > '\x7F'

letter :: Char -> Bool
letter c = upperCaseAscii c || lowerCaseAscii c || nonAscii c

dollar :: Char -> Bool
dollar = (== '$')

colon :: Char -> Bool
colon = (== ':')

openCurly :: Char -> Bool
openCurly = (== '{')

closeCurly :: Char -> Bool
closeCurly = (== '}')

openSquare :: Char -> Bool
openSquare = (== '[')

closeSquare :: Char -> Bool
closeSquare = (== ']')

doubleQuote :: Char -> Bool
doubleQuote = (== '"')

period :: Char -> Bool
period = (== '.')

hash :: Char -> Bool
hash = (== '#')

thinSpace :: Char -> Bool
thinSpace = (== '\x2009')

dateSep :: Char -> Bool
dateSep c = c == '/' || c == '-'

plus :: Char -> Bool
plus = (== '+')

minus :: Char -> Bool
minus = (== '-')

lessThan :: Char -> Bool
lessThan = (== '<')

greaterThan :: Char -> Bool
greaterThan = (== '>')

openParen :: Char -> Bool
openParen = (== '(')

closeParen :: Char -> Bool
closeParen = (== ')')

semicolon :: Char -> Bool
semicolon = (== ';')

apostrophe :: Char -> Bool
apostrophe = (== '\x27')

tilde :: Char -> Bool
tilde = (== '~')

underscore :: Char -> Bool
underscore = (== '_')

asterisk :: Char -> Bool
asterisk = (== '*')

lvl1AcctChar :: Char -> Bool
lvl1AcctChar c = nonNewline c && (not . closeCurly $ c)
                 && (not . colon $ c)

lvl2AcctOtherChar :: Char -> Bool
lvl2AcctOtherChar c =
  nonNewline c && (not . white $ c) && (not . colon $ c)

lvl1CmdtyChar :: Char -> Bool
lvl1CmdtyChar c =
  nonNewline c && (not . doubleQuote $ c)

lvl2CmdtyFirstChar :: Char -> Bool
lvl2CmdtyFirstChar c = letter c || dollar c

lvl2CmdtyOtherChar :: Char -> Bool
lvl2CmdtyOtherChar c = nonNewline c && (not . white $ c)

lvl3CmdtyChar :: Char -> Bool
lvl3CmdtyChar c = letter c || dollar c

flagChar :: Char -> Bool
flagChar c = nonNewline c && (not . closeSquare $ c)

numberChar :: Char -> Bool
numberChar c = nonNewline c && (not . closeParen $ c)

quotedPayeeChar :: Char -> Bool
quotedPayeeChar c = nonNewline c && (not . tilde $ c)

tagChar :: Char -> Bool
tagChar c = nonNewlineNonSpace c && (not . asterisk $ c)
