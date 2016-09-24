{-# LANGUAGE OverloadedLists #-}

-- | Grammar for ledger files
--
-- If possible, whitespace should appear between adjacent
-- productions in a product.  Often, whitespace must appear
-- conditionally, in conjunction with another production that is
-- optional (either with 'opt' or with 'star' or 'plus'.)  In that
-- case, the whitespace should appear within the optional production
-- and should appear first.
--
-- The data types corresponding to the gramamr are in "Penny.Copper.Types".
module Penny.Copper.Grammar (allRules) where

import Pinchot (terminal, solo, union, plus, star, pariah, include,
  record, opt, nonTerminal, wrap, Rule, series)
import qualified Data.Char as Char
import Data.Monoid ((<>))
import Data.Sequence (Seq)

zero = terminal "Zero" (solo '0')
one = terminal "One" (solo '1')
two = terminal "Two" (solo '2')
three = terminal "Three" (solo '3')
four = terminal "Four" (solo '4')
five = terminal "Five" (solo '5')
six = terminal "Six" (solo '6')
seven = terminal "Seven" (solo '7')
eight = terminal "Eight" (solo '8')
nine = terminal "Nine" (solo '9')

-- # Digits
-- Digit from 1 through 9
d1'9 = union "D1'9"
  [one, two, three, four, five, six, seven, eight, nine]

-- Digit from 0 through 9
d0'9 = union "D0'9"
  [ zero, one, two, three, four, five, six, seven, eight, nine ]

-- Digit from 0 through 8
d0'8 = union "D0'8"
  [ zero, one, two, three, four, five, six, seven, eight ]

-- Digit from 0 through 1
d0'1 = union "D0'1" [zero, one]

-- Digit from 0 through 3
d0'3 = union "D0'3" [zero, one, two, three]

-- Digit from 0 through 5
d0'5 = union "D0'5" [zero, one, two, three, four, five]


thinSpace = terminal "ThinSpace" (solo '\x2009')
underscore = terminal "Underscore" (solo '_')
period = terminal "Period" (solo '.')
comma = terminal "Comma" (solo ',')
grouper = union "Grouper" [ thinSpace, underscore ]
grpRadCom = union "GrpRadCom" [period, grouper]
grpRadPer = union "GrpRadPer" [comma, grouper]

-- # Radix
radixCom = terminal "RadixCom" (solo ',')
radixPer = terminal "RadixPer" (solo '.')

-- # Groups of digits
digits = star d0'9
digitGroupRadCom = record "DigitGroupRadCom"
  [grpRadCom, d0'9, digits]
digitGroupRadPer = record "DigitGroupRadPer"
  [grpRadPer, d0'9, digits]
digitGroupsRadCom = star digitGroupRadCom
digitGroupsRadPer = star digitGroupRadPer

-- # Groupers
-- # Nil

maybeZero = opt zero
zeroes = star zero
radixZeroesRadCom = record "RadixZeroesRadCom" [radixCom, zeroes]
radixZeroesRadPer = record "RadixZeroesRadPer" [radixPer, zeroes]
maybeRadixZeroesRadCom = opt radixZeroesRadCom
maybeRadixZeroesRadPer = opt radixZeroesRadPer
zeroGroupRadCom = record "ZeroGroupRadCom"
  [grpRadCom, zero, zeroes]
zeroGroupRadPer = record "ZeroGroupRadPer"
  [grpRadPer, zero, zeroes]
zeroGroupRadCom1s = plus zeroGroupRadCom
zeroGroupRadPer1s = plus zeroGroupRadPer

nilGroupedRadCom = record "NilGroupedRadCom"
  [maybeZero, radixCom, zero, zeroes, zeroGroupRadCom1s]
nilGroupedRadPer = record "NilGroupedRadPer"
  [maybeZero, radixPer, zero, zeroes, zeroGroupRadPer1s]


nilUngroupedRadCom = nonTerminal "NilUngroupedRadCom"
  [ ("NUZeroRadCom", [zero, maybeRadixZeroesRadCom])
  , ("NURadixRadCom", [radixCom, zero, zeroes])
  ]
nilUngroupedRadPer = nonTerminal "NilUngroupedRadPer"
  [ ("NUZeroRadPer", [zero, maybeRadixZeroesRadPer])
  , ("NURadixRadPer", [radixPer, zero, zeroes])
  ]

nilRadCom = union "NilRadCom" [nilUngroupedRadCom, nilGroupedRadCom]
nilRadPer = union "NilRadPer" [nilUngroupedRadPer, nilGroupedRadPer]

-- # Brim

radixComDigits = record "RadixComDigits" [radixCom, digits]
radixPerDigits = record "RadixPerDigits" [radixPer, digits]
maybeRadixComDigits = opt radixComDigits
maybeRadixPerDigits = opt radixPerDigits
brimUngroupedRadCom = nonTerminal "BrimUngroupedRadCom"
  [ ("BUGreaterThanOneRadCom", [d1'9, digits, maybeRadixComDigits])
  , ("BULessThanOneRadCom", [maybeZero, radixCom, zeroes, d1'9, digits])
  ]
brimUngroupedRadPer = nonTerminal "BrimUngroupedRadPer"
  [ ("BUGreaterThanOneRadPer", [d1'9, digits, maybeRadixPerDigits])
  , ("BULessThanOneRadPer", [maybeZero, radixPer, zeroes, d1'9, digits])
  ]

-- # BrimGrouped

bg8RadCom = nonTerminal "BG8RadCom"
  [ ("BG8NovemRadCom", [d1'9, digits, digitGroupsRadCom])
  , ("BG8GroupRadCom", [grpRadCom, bg7RadCom])
  ]
bg8RadPer = nonTerminal "BG8RadPer"
  [ ("BG8NovemRadPer", [d1'9, digits, digitGroupsRadPer])
  , ("BG8GroupRadPer", [grpRadPer, bg7RadPer])
  ]

bg7RadCom = nonTerminal "BG7RadCom"
  [ ("BG7ZeroesRadCom", [zero, zeroes, bg8RadCom])
  , ("BG7NovemRadCom", [d1'9, digits, digitGroupsRadCom])
  ]
bg7RadPer = nonTerminal "BG7RadPer"
  [ ("BG7ZeroesRadPer", [zero, zeroes, bg8RadPer])
  , ("BG7NovemRadPer", [d1'9, digits, digitGroupsRadPer])
  ]

bg6RadCom = nonTerminal "BG6RadCom"
  [ ("BG6NovemRadCom", [d1'9, digits, grpRadCom, d0'9, digits,
                        digitGroupsRadCom])
  , ("BG6GroupRadCom", [grpRadCom, bg7RadCom])
  ]
bg6RadPer = nonTerminal "BG6RadPer"
  [ ("BG6NovemRadPer", [d1'9, digits, grpRadPer, d0'9, digits,
                        digitGroupsRadPer])
  , ("BG6GroupRadPer", [grpRadPer, bg7RadPer])
  ]

bg5RadCom = nonTerminal "BG5RadCom"
  [ ("BG5NovemRadCom", [ d1'9, digits, grpRadCom, d0'9, digits,
                         digitGroupsRadCom ])
  , ("BG5ZeroRadCom", [zero, zeroes, bg6RadCom])
  ]
bg5RadPer = nonTerminal "BG5RadPer"
  [ ("BG5NovemRadPer", [ d1'9, digits, grpRadPer, d0'9, digits,
                         digitGroupsRadPer ])
  , ("BG5ZeroRadPer", [zero, zeroes, bg6RadPer])
  ]

bg4RadCom = nonTerminal "BG4RadCom"
  [ ("BG4DigitRadCom", [d0'9, digits, digitGroupsRadCom])
  , ("BG4NilRadCom", [])
  ]
bg4RadPer = nonTerminal "BG4RadPer"
  [ ("BG4DigitRadPer", [d0'9, digits, digitGroupsRadPer])
  , ("BG4NilRadPer", [])
  ]

bg3RadCom = nonTerminal "BG3RadCom"
  [ ("BG3RadixRadCom", [radixCom, bg4RadCom])
  , ("BG3NilRadCom", [])
  ]
bg3RadPer = nonTerminal "BG3RadPer"
  [ ("BG3RadixRadPer", [radixPer, bg4RadPer])
  , ("BG3NilRadPer", [])
  ]

bg1RadCom = nonTerminal "BG1RadCom"
  [ ("BG1GroupOnLeftRadCom", [ grpRadCom, d0'9, digits, digitGroupsRadCom,
                               bg3RadCom ])
  , ("BG1GroupOnRightRadCom", [ radixCom, d0'9, digits, grpRadCom,
                                d0'9, digits, digitGroupsRadCom])
  ]
bg1RadPer = nonTerminal "BG1RadPer"
  [ ("BG1GroupOnLeftRadPer", [ grpRadPer, d0'9, digits, digitGroupsRadPer,
                               bg3RadPer ])
  , ("BG1GroupOnRightRadPer", [ radixPer, d0'9, digits, grpRadPer,
                                d0'9, digits, digitGroupsRadPer])
  ]

brimGroupedRadCom = nonTerminal "BrimGroupedRadCom"
  [ ("BGGreaterThanOneRadCom", [ d1'9, digits, bg1RadCom ])
  , ("BGLessThanOneRadCom", [ maybeZero, radixCom, bg5RadCom ])
  ]
brimGroupedRadPer = nonTerminal "BrimGroupedRadPer"
  [ ("BGGreaterThanOneRadPer", [ d1'9, digits, bg1RadPer ])
  , ("BGLessThanOneRadPer", [ maybeZero, radixPer, bg5RadPer ])
  ]

brimRadCom = union "BrimRadCom" [brimUngroupedRadCom, brimGroupedRadCom]
brimRadPer = union "BrimRadPer" [brimUngroupedRadPer, brimGroupedRadPer]

-- Aggregates of Nil and Brim.  These do not appear in the
-- WholeFile production but they can be useful when parsing
-- numbers standing alone, such as numbers from an OFX statement.
nilOrBrimRadCom = union "NilOrBrimRadCom" [nilRadCom, brimRadCom]
nilOrBrimRadPer = union "NilOrBrimRadPer" [nilRadPer, brimRadPer]

-- This production does not appear in WholeFile.  However, it can be
-- useful when parsing decimals that appear outside of ledger files,
-- such as on a command line or in an OFX statement.  Unlike most
-- productions, this one includes optional leading and trailing
-- whitespace.
decimalRadCom = record "DecimalRadCom"
  [ rWhite'Star, maybePluMin, rWhite'Star, nilOrBrimRadCom, rWhite'Star ]
decimalRadPer = record "DecimalRadPer"
  [ rWhite'Star, maybePluMin, rWhite'Star, nilOrBrimRadPer, rWhite'Star ]

-- # Dates
hyphen = terminal "Hyphen" (solo '-')
slash = terminal "Slash" (solo '/')
dateSep = union "DateSep" [hyphen, slash]
days28 = nonTerminal "Days28"
  [ ("D28'1to9", [zero, d1'9])
  , ("D28'10to19", [one, d0'9])
  , ("D28'20to28", [two, d0'8])
  ]

days30 = nonTerminal "Days30"
  [ ("D30'28", [days28])
  , ("D30'29", [two, nine])
  , ("D30'30", [three, zero])
  ]

days31 = nonTerminal "Days31"
  [ ("D31'30", [days30])
  , ("D31'31", [three, one])
  ]

monthDay = nonTerminal "MonthDay"
  [ ("Jan", [zero, one, dateSep, days31])
  , ("Feb", [zero, two, dateSep, days28])
  , ("Mar", [zero, three, dateSep, days31])
  , ("Apr", [zero, four, dateSep, days30])
  , ("May", [zero, five, dateSep, days31])
  , ("Jun", [zero, six, dateSep, days30])
  , ("Jul", [zero, seven, dateSep, days31])
  , ("Aug", [zero, eight, dateSep, days31])
  , ("Sep", [zero, nine, dateSep, days30])
  , ("Oct", [one, zero, dateSep, days31])
  , ("Nov", [one, one, dateSep, days30])
  , ("Dec", [one, two, dateSep, days31])
  ]

year = record "Year" [d0'9, d0'9, d0'9, d0'9]
nonLeapDay = record "NonLeapDay" [year, dateSep, monthDay]
mod4 = nonTerminal "Mod4"
  [ ("L04", [zero, four])
  , ("L08", [zero, eight])
  , ("L12", [one, two])
  , ("L16", [one, six])
  , ("L20", [two, zero])
  , ("L24", [two, four])
  , ("L28", [two, eight])
  , ("L32", [three, two])
  , ("L36", [three, six])
  , ("L40", [four, zero])
  , ("L44", [four, four])
  , ("L48", [four, eight])
  , ("L52", [five, two])
  , ("L56", [five, six])
  , ("L60", [six, zero])
  , ("L64", [six, four])
  , ("L68", [six, eight])
  , ("L72", [seven, two])
  , ("L76", [seven, six])
  , ("L80", [eight, zero])
  , ("L84", [eight, four])
  , ("L88", [eight, eight])
  , ("L92", [nine, two])
  , ("L96", [nine, six])
  ]

-- Leap Years:
-- We only care about whether it is a leap year for the purpose
-- of parsing February 29.
--
-- Leap years include all years:
-- Exactly divisible by 4 AND NOT exactly divisible by 100;
-- Exactly divisible by 4 AND divisible by 400.
--
-- The first set is captured by [d0'9, d0'9, mod4].
-- The second set is captured by [mod4, zero, zero].
-- A special exception is 0000, which is treated specially.

centuryLeapYear = nonTerminal "CenturyLeapYear"
  [ ("LeapYear0", [zero, zero, zero, zero])
  , ("LeapYearMod4", [mod4, zero, zero])
  ]
nonCenturyLeapYear = record "NonCenturyLeapYear" [d0'9, d0'9, mod4]

leapYear = union "LeapYear" [ centuryLeapYear, nonCenturyLeapYear ]
leapDay = record "LeapDay"
  [ leapYear, dateSep, zero, two, dateSep, two, nine ]

date = union "Date" [ nonLeapDay, leapDay ]

-- Main grammar
newline = terminal "Newline" $ solo '\n'

-- Comments
hash = terminal "Hash" $ solo '#'
commentChar = terminal "CommentChar" $ include minBound maxBound
  <> pariah '\n'
commentChars = star commentChar
comment = record "Comment" [hash, commentChars, newline]

-- Time
maybe0or1 = opt d0'1
n0'19 = record "N0'19" [maybe0or1, d0'9]
n20'23 = record "N20'23" [two, d0'3]
hours = union "Hours" [n0'19, n20'23]

n0'59 = record "N0'59" [d0'5, d0'9]
minutes = wrap "Minutes" n0'59
seconds = wrap "Seconds" n0'59
colon = terminal "Colon" $ solo ':'
colonSeconds = record "ColonSeconds" [colon, seconds]
maybeSeconds = opt colonSeconds
time = record "Time" [hours, colon, minutes, maybeSeconds]
plusSign = terminal "Plus" $ solo '+'
minus = terminal "Minus" $ solo '-'
pluMin = union "PluMin" [plusSign, minus]
zone = record "Zone" [pluMin, hours, colon, minutes]
backtick = terminal "Backtick" $ solo '`'

whitesZone = record "WhitesZone" [rWhite'Star, zone]
mayWhitesZone = opt whitesZone
whitesTime = record "WhitesTime" [rWhite'Plus, time]
timeAndMayZone = record "TimeAndMayZone" [whitesTime, mayWhitesZone]
mayTimeAndMayZone = opt timeAndMayZone
dateTimeZone = record "DateTimeZone" [date, mayTimeAndMayZone]

-- Whitespace and quoted strings
doubleQuote = terminal "DoubleQuote" $ solo '"'
backslash = terminal "Backslash" $ solo '\\'
space = terminal "Space" $ solo ' '
tab = terminal "Tab" $ solo '\t'
white = union "White" [space, tab, newline]
rWhite'Star = star white
rWhite'Plus = plus white
gap = record "Gap" [rWhite'Plus, backslash]
escPayload = union "EscPayload" [backslash, newline, doubleQuote, gap]
nonEscapedChar = terminal "NonEscapedChar" $ include minBound maxBound
  <> pariah '\\' <> pariah '\n' <> pariah '"'
escSeq = record "EscSeq" [backslash, escPayload]
quotedChar = union "QuotedChar" [nonEscapedChar, escSeq]
quotedChars = star quotedChar
quotedString = record "QuotedString"
  [doubleQuote, quotedChars, doubleQuote]

-- Semantic space - unlike rWhite'Star and rWhite'Plus, this space has
-- semantic meaning.
rSpace'Opt = opt space
rSemanticSpace = wrap "SemanticSpace" rSpace'Opt

-- Unquoted strings
unquotedStringNonDigitChar =
  terminal "UnquotedStringNonDigitChar" $
    include 'a' 'z' <> include 'A' 'Z' <> currencySymbols
  where
    currencySymbols = foldl addSym mempty
      . filter isCurr
      $ [minBound .. maxBound]
    addSym acc c = acc <> solo c
    isCurr c = Char.generalCategory c == Char.CurrencySymbol
unquotedStringEndChar = union "UnquotedStringNonFirstChar"
  [unquotedStringNonDigitChar, d0'9]
unquotedStringEndChars = star unquotedStringEndChar
unquotedString = record "UnquotedString"
  [digits, unquotedStringNonDigitChar, unquotedStringEndChars]

unquotedStringNonDigitChars1 = plus unquotedStringNonDigitChar

anyString = union "AnyString" [unquotedString, quotedString]

-- Commodities
unquotedCommodity = wrap "UnquotedCommodity"
  unquotedStringNonDigitChars1
quotedCommodity = wrap "QuotedCommodity" quotedString
commodity = union "Commodity" [unquotedCommodity, quotedCommodity]

-- Labels
-- label = record "Label" [apostrophe, unquotedString]

-- NonNeutral
nonNeutral = nonTerminal "NonNeutral"
  [ ("NonNeutralRadCom", [backtick, brimRadCom])
  , ("NonNeutralRadPer", [brimRadPer])
  ]

neutral = nonTerminal "Neutral"
  [ ("NeuCom", [backtick, nilRadCom])
  , ("NeuPer", [nilRadPer])
  ]

-- Whole
maybePluMin = opt pluMin
wholeNonZero = record "WholeNonZero" [maybePluMin, d1'9, digits]
wholeAny = union "WholeAny" [zero, wholeNonZero]

lessThan = terminal "LessThan" $ solo '<'
greaterThan = terminal "GreaterThan" $ solo '>'
debit = wrap "Debit" lessThan
credit = wrap "Credit" greaterThan
debitCredit = union "DebitCredit" [debit, credit]

-- Trio
--
-- If there is a debit or credit present, it always appears first.

-- If there is a debit or credit present, there can be a non-neutral
-- present.  Also, a commodity may be present.
t_DebitCredit = wrap "T_DebitCredit" debitCredit

t_DebitCredit_Commodity = record "T_DebitCredit_Commodity"
  [ debitCredit, rWhite'Star, commodity ]

t_DebitCredit_NonNeutral = record "T_DebitCredit_NonNeutral"
  [ debitCredit, rWhite'Star, nonNeutral]

t_DebitCredit_Commodity_NonNeutral =
  record "T_DebitCredit_Commodity_NonNeutral"
  [ debitCredit, rWhite'Star, commodity, rSemanticSpace, nonNeutral]
t_DebitCredit_NonNeutral_Commodity = record
  "T_DebitCredit_NonNeutral_Commodity"
  [ debitCredit, rWhite'Star, nonNeutral, rSemanticSpace, commodity]

-- If there is no debit or credit present, there may be only a commodity.
t_Commodity = wrap "T_Commodity" commodity

-- There can be no debit or credit, with a commodity and either a
-- neutral or non-neutral.

t_Commodity_Neutral = record "T_Commodity_Neutral"
  [ commodity, rSemanticSpace, neutral ]
t_Neutral_Commodity = record "T_Neutral_Commodity"
  [ neutral, rSemanticSpace, commodity ]
t_Commodity_NonNeutral = record "T_Commodity_NonNeutral"
  [ commodity, rSemanticSpace, nonNeutral ]
t_NonNeutral_Commodity = record "T_NonNeutral_Commodity"
  [ nonNeutral, rSemanticSpace, commodity ]

-- A neutral or non-neutral standing alone is possible.
t_Neutral = wrap "T_Neutral" neutral
t_NonNeutral = wrap "T_NonNeutral" nonNeutral

trio = union "Trio"
  [ t_DebitCredit,

    t_DebitCredit_Commodity,

    t_DebitCredit_NonNeutral,

    t_DebitCredit_Commodity_NonNeutral, t_DebitCredit_NonNeutral_Commodity,

    t_Commodity,

    t_Commodity_Neutral, t_Neutral_Commodity,
    t_Commodity_NonNeutral, t_NonNeutral_Commodity,

    t_Neutral, t_NonNeutral
  ]

-- There are four types of data fields available:
-- dateTimeZone
-- unquotedString
-- quotedString
-- wholeAny

-- Labels
lblOrigPayee = series "LblOrigPayee" "'origPayee'"
lblNumber = series "LblNumber" "'number'"
lblFitid = series "LblFitid" "'fitid'"
lblTags = series "LblTags" "'tags'"
lblUid = series "LblUid" "'uid'"
lblOfxTrn = series "LblOfxTrn" "'ofxtrn'"
lblOrigDate = series "LblOrigDate" "'origDate'"


-- Lists
openSquare = terminal "OpenSquare" $ solo '['
closeSquare = terminal "CloseSquare" $ solo ']'

rNextListItem = record "NextListItem" [rWhite'Plus, anyString]
rNextListItem'Star = star rNextListItem
rListItems = record "ListItems" [rWhite'Star, anyString, rNextListItem'Star]
rListItems'Opt = opt rListItems
bracketedList = record "BracketedList"
  [ openSquare, rListItems'Opt, rWhite'Star, closeSquare ]

-- TrnType
trnCredit = series "TCREDIT" "TCREDIT"
trnDebit = series "TDEBIT" "TDEBIT"
trnInt = series "TINT" "TINT"
trnDiv = series "TDIV" "TDIV"
trnFee = series "TFEE" "TFEE"
trnSrvChg = series "TSRVCHG" "TSRVCHG"
trnDep = series "TDEP" "TDEP"
trnAtm = series "TATM" "TATM"
trnPos = series "TPOS" "TPOS"
trnXfer = series "TXFER" "TXFER"
trnCheck = series "TCHECK" "TCHECK"
trnPayment = series "TPAYMENT" "TPAYMENT"
trnCash = series "TCASH" "TCASH"
trnDirectDep = series "TDIRECTDEP" "TDIRECTDEP"
trnDirectDebit = series "TDIRECTDEBIT" "TDIRECTDEBIT"
trnRepeatPmt = series "TREPEATPMT" "TREPEATPMT"
trnOther = series "TOTHER" "TOTHER"

ofxTrnData = union "OfxTrnData" [ trnCredit, trnDebit, trnInt, trnDiv,
  trnFee, trnSrvChg, trnDep, trnAtm, trnPos, trnXfer, trnCheck,
  trnPayment, trnCash, trnDirectDep, trnDirectDebit, trnRepeatPmt,
  trnOther ]

-- Fields

openParen = terminal "OpenParen" (solo '(')
closeParen = terminal "CloseParen" (solo ')')

-- Top line fields
dateField = wrap "DateField" dateTimeZone
payee = wrap "Payee" anyString
origPayee = record "OrigPayee" [rWhite'Star, lblOrigPayee, rWhite'Star, anyString]

-- Posting fields
number = record "Number" [lblNumber, rWhite'Star, wholeAny]
flag = record "Flag" [openParen, rWhite'Star, anyString, rWhite'Star, closeParen]
account = wrap "Account" bracketedList
fitid = record "Fitid" [lblFitid, rWhite'Star, anyString]
tags = record "Tags" [lblTags, rWhite'Star, bracketedList]
uid = record "Uid" [lblUid, rWhite'Star, anyString]
ofxTrn = record "OfxTrn" [lblOfxTrn, rWhite'Star, ofxTrnData]
origDay = record "OrigDate" [lblOrigDate, rWhite'Star, dateTimeZone]

postingField = union "PostingField" [number, flag, account, fitid,
  tags, uid, ofxTrn, origDay]

-- P suffix means "preceded by whitespace"

rOrigPayee'Opt = opt origPayee

rTopLineFields = record "TopLineFields"
  [dateField, rWhite'Plus, payee, rOrigPayee'Opt]

rPostingFieldP = record "PostingFieldP" [rWhite'Plus, postingField]
rPostingFieldP'Star = star rPostingFieldP
rPostingFields = record "PostingFields" [postingField, rPostingFieldP'Star]
rPostingFieldsP = record "PostingFieldsP" [rWhite'Star, rPostingFields]
optPostingFieldsP = opt rPostingFieldsP
trioMaybeFields = record "TrioMaybeFields"
  [ trio, optPostingFieldsP ]
posting = union "Posting" [trioMaybeFields, rPostingFields]

semicolon = terminal "Semicolon" $ solo ';'
openCurly = terminal "OpenCurly" $ solo '{'
closeCurly = terminal "CloseCurly" $ solo '}'

nextPosting = record "NextPosting" [rWhite'Star, semicolon, rWhite'Star, posting]
nextPostings = star nextPosting
postingList = record "PostingList" [rWhite'Star, posting, nextPostings]
maybePostingList = opt postingList
postings = record "Postings"
  [openCurly, maybePostingList, rWhite'Star, closeCurly, newline]
transaction = record "Transaction"
  [rTopLineFields, rWhite'Star, postings]

-- Prices
atSign = terminal "AtSign" $ solo '@'
pluMinNonNeutral = record "PluMinNonNeutral" [pluMin, rWhite'Star, nonNeutral]
exchNonNeu = union "ExchNonNeu" [pluMinNonNeutral, nonNeutral]
exch = union "Exch" [exchNonNeu, neutral]

cyExch = record "CyExch" [commodity, rWhite'Star, exch]
exchCy = record "ExchCy" [exch, rWhite'Star, commodity]
janus = union "Janus" [cyExch, exchCy]

price = record "Price"
  [ atSign, rWhite'Star, dateTimeZone, rWhite'Plus, commodity,
    rWhite'Plus, janus, newline ]

rFileItem = union "FileItem" [price, transaction, comment]
rFileItemP = record "FileItemP" [rWhite'Star, rFileItem]
rFileItemP'Star = star rFileItemP
rWholeFile = record "WholeFile" [rFileItemP'Star, rWhite'Star]

-- | All interesting rules in this file are either in this list or are
-- descendants of the items in this list.
allRules :: Seq (Rule Char)
allRules = [ rWholeFile, nilOrBrimRadCom, nilOrBrimRadPer,
  decimalRadCom, decimalRadPer ]
