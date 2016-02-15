{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Pinchot where

import Pinchot
import Data.Monoid ((<>))

grammar :: Pinchot Char (Rule Char)
grammar = mdo

  -- # Digits
  zero <- terminal "Zero" (solo '0')
  one <- terminal "One" (solo '1')
  two <- terminal "Two" (solo '2')
  three <- terminal "Three" (solo '3')
  four <- terminal "Four" (solo '4')
  five <- terminal "Five" (solo '5')
  six <- terminal "Six" (solo '6')
  seven <- terminal "Seven" (solo '7')
  eight <- terminal "Eight" (solo '8')
  nine <- terminal "Nine" (solo '9')

  -- Digit from 1 through 9
  d1'9 <- union "D1'9"
    [one, two, three, four, five, six, seven, eight, nine]

  -- Digit from 0 through 9
  d0'9 <- union "D0'9"
    [ zero, one, two, three, four, five, six, seven, eight, nine ]

  -- Digit from 0 through 8
  d0'8 <- union "D0'8"
    [ zero, one, two, three, four, five, six, seven, eight ]

  -- Digit from 0 through 1
  d0'1 <- union "D0'1" [zero, one]

  -- Digit from 0 through 2
  d0'2 <- union "D0'2" [zero, one, two]

  -- Digit from 0 through 3
  d0'3 <- union "D0'3" [zero, one, two, three]

  -- Digit from 0 through 5
  d0'5 <- union "D0'5" [zero, one, two, three, four, five]


  -- # Groupers
  thinSpace <- terminal "ThinSpace" (solo '\x2009')
  underscore <- terminal "Underscore" (solo '_')
  period <- terminal "Period" (solo '.')
  comma <- terminal "Comma" (solo ',')
  grouper <- union "Grouper" [ thinSpace, underscore ]
  grpRadCom <- union "GrpRadCom" [period, grouper]
  grpRadPer <- union "GrpRadPer" [comma, grouper]

  -- # Radix
  radixCom <- terminal "RadixCom" (solo ',')
  radixPer <- terminal "RadixPer" (solo '.')

  -- # Groups of digits
  digits <- list d0'9
  digitGroupRadCom <- record "DigitGroupRadCom"
    [grpRadCom, d0'9, digits]
  digitGroupRadPer <- record "DigitGroupRadPer"
    [grpRadPer, d0'9, digits]
  digitGroupsRadCom <- list digitGroupRadCom
  digitGroupsRadPer <- list digitGroupRadPer

  -- # Nil

  maybeZero <- option zero
  zeroes <- list zero
  radixZeroesRadCom <- record "RadixZeroesRadCom" [radixCom, zeroes]
  radixZeroesRadPer <- record "RadixZeroesRadPer" [radixPer, zeroes]
  maybeRadixZeroesRadCom <- option radixZeroesRadCom
  maybeRadixZeroesRadPer <- option radixZeroesRadPer
  zeroGroupRadCom <- record "ZeroGroupRadCom"
    [grpRadCom, zero, zeroes]
  zeroGroupRadPer <- record "ZeroGroupRadPer"
    [grpRadPer, zero, zeroes]
  zeroGroupRadCom1s <- list1 zeroGroupRadCom
  zeroGroupRadPer1s <- list1 zeroGroupRadPer

  nilGroupedRadCom <- record "NilGroupedRadCom"
    [maybeZero, radixCom, zero, zeroes, zeroGroupRadCom1s]
  nilGroupedRadPer <- record "NilGroupedRadPer"
    [maybeZero, radixPer, zero, zeroes, zeroGroupRadPer1s]


  nilUngroupedRadCom <- nonTerminal "NilUngroupedRadCom"
    [ ("NUZeroRadCom", [zero, maybeRadixZeroesRadCom])
    , ("NURadixRadCom", [radixCom, zero, zeroes])
    ]
  nilUngroupedRadPer <- nonTerminal "NilUngroupedRadPer"
    [ ("NUZeroRadPer", [zero, maybeRadixZeroesRadPer])
    , ("NURadixRadPer", [radixPer, zero, zeroes])
    ]

  nilRadCom <- union "NilRadCom" [nilUngroupedRadCom, nilGroupedRadCom]
  nilRadPer <- union "NilRadPer" [nilUngroupedRadPer, nilGroupedRadPer]

  -- # Brim

  radixComDigits <- record "RadixComDigits" [radixCom, digits]
  radixPerDigits <- record "RadixPerDigits" [radixPer, digits]
  maybeRadixComDigits <- option radixComDigits
  maybeRadixPerDigits <- option radixPerDigits
  brimUngroupedRadCom <- nonTerminal "BrimUngroupedRadCom"
    [ ("BUGreaterThanOneRadCom", [d1'9, digits, maybeRadixComDigits])
    , ("BULessThanOneRadCom", [maybeZero, radixCom, zeroes, d1'9, digits])
    ]
  brimUngroupedRadPer <- nonTerminal "BrimUngroupedRadPer"
    [ ("BUGreaterThanOneRadPer", [d1'9, digits, maybeRadixPerDigits])
    , ("BULessThanOneRadPer", [maybeZero, radixPer, zeroes, d1'9, digits])
    ]

  -- # BrimGrouped

  bg8RadCom <- nonTerminal "BG8RadCom"
    [ ("BG8NovemRadCom", [d1'9, digits, digitGroupsRadCom])
    , ("BG8GroupRadCom", [grpRadCom, bg7RadCom])
    ]
  bg8RadPer <- nonTerminal "BG8RadPer"
    [ ("BG8NovemRadPer", [d1'9, digits, digitGroupsRadPer])
    , ("BG8GroupRadPer", [grpRadPer, bg7RadPer])
    ]

  bg7RadCom <- nonTerminal "BG7RadCom"
    [ ("BG7ZeroesRadCom", [zero, zeroes, bg8RadCom])
    , ("BG7NovemRadCom", [d1'9, digits, digitGroupsRadCom])
    ]
  bg7RadPer <- nonTerminal "BG7RadPer"
    [ ("BG7ZeroesRadPer", [zero, zeroes, bg8RadPer])
    , ("BG7NovemRadPer", [d1'9, digits, digitGroupsRadPer])
    ]

  bg6RadCom <- nonTerminal "BG6RadCom"
    [ ("BG6NovemRadCom", [d1'9, digits, grpRadCom, d0'9, digits,
                          digitGroupsRadCom])
    , ("BG6GroupRadCom", [grpRadCom, bg7RadCom])
    ]
  bg6RadPer <- nonTerminal "BG6RadPer"
    [ ("BG6NovemRadPer", [d1'9, digits, grpRadPer, d0'9, digits,
                          digitGroupsRadPer])
    , ("BG6GroupRadPer", [grpRadPer, bg7RadPer])
    ]

  bg5RadCom <- nonTerminal "BG5RadCom"
    [ ("BG5NovemRadCom", [ d1'9, digits, grpRadCom, d0'9, digits,
                           digitGroupsRadCom ])
    , ("BG5ZeroRadCom", [zero, zeroes, bg6RadCom])
    ]
  bg5RadPer <- nonTerminal "BG5RadPer"
    [ ("BG5NovemRadPer", [ d1'9, digits, grpRadPer, d0'9, digits,
                           digitGroupsRadPer ])
    , ("BG5ZeroRadPer", [zero, zeroes, bg6RadPer])
    ]

  bg4RadCom <- nonTerminal "BG4RadCom"
    [ ("BG4DigitRadCom", [d0'9, digits, digitGroupsRadCom])
    , ("BG4NilRadCom", [])
    ]
  bg4RadPer <- nonTerminal "BG4RadPer"
    [ ("BG4DigitRadPer", [d0'9, digits, digitGroupsRadPer])
    , ("BG4NilRadPer", [])
    ]

  bg3RadCom <- nonTerminal "BG3RadCom"
    [ ("BG3RadixRadCom", [radixCom, bg4RadCom])
    , ("BG3NilRadCom", [])
    ]
  bg3RadPer <- nonTerminal "BG3RadPer"
    [ ("BG3RadixRadPer", [radixPer, bg4RadPer])
    , ("BG3NilRadPer", [])
    ]

  bg1RadCom <- nonTerminal "BG1RadCom"
    [ ("BG1GroupOnLeftRadCom", [ grpRadCom, d0'9, digits, digitGroupsRadCom,
                                 bg3RadCom ])
    , ("BG1GroupOnRightRadCom", [ radixCom, d0'9, digits, grpRadCom,
                                  d0'9, digits, digitGroupsRadCom])
    ]
  bg1RadPer <- nonTerminal "BG1RadPer"
    [ ("BG1GroupOnLeftRadPer", [ grpRadPer, d0'9, digits, digitGroupsRadPer,
                                 bg3RadPer ])
    , ("BG1GroupOnRightRadPer", [ radixPer, d0'9, digits, grpRadPer,
                                  d0'9, digits, digitGroupsRadPer])
    ]

  brimGroupedRadCom <- nonTerminal "BrimGroupedRadCom"
    [ ("BGGreaterThanOneRadCom", [ d1'9, digits, bg1RadCom ])
    , ("BGLessThanOneRadCom", [ maybeZero, radixCom, bg5RadCom ])
    ]
  brimGroupedRadPer <- nonTerminal "BrimGroupedRadPer"
    [ ("BGGreaterThanOneRadPer", [ d1'9, digits, bg1RadPer ])
    , ("BGLessThanOneRadPer", [ maybeZero, radixPer, bg5RadPer ])
    ]

  -- Brim
  brimRadCom <- union "BrimRadCom" [brimUngroupedRadCom, brimGroupedRadCom]
  brimRadPer <- union "BrimRadPer" [brimUngroupedRadPer, brimGroupedRadPer]

  -- # Dates
  hyphen <- terminal "Hyphen" (solo '-')
  slash <- terminal "Slash" (solo '/')
  dateSep <- union "DateSep" [hyphen, slash]
  days28 <- nonTerminal "Days28"
    [ ("D28'1to9", [zero, d1'9])
    , ("D28'10to19", [one, d0'9])
    , ("D28'20to28", [two, d0'8])
    ]

  days30 <- nonTerminal "Days30"
    [ ("D30'28", [days28])
    , ("D30'29", [two, nine])
    , ("D30'30", [three, zero])
    ]

  days31 <- nonTerminal "Days31"
    [ ("D31'30", [days30])
    , ("D31'31", [three, one])
    ]

  monthDay <- nonTerminal "MonthDay"
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

  year <- record "Year" [d0'9, d0'9, d0'9, d0'9]
  nonLeapDay <- record "NonLeapDay" [year, dateSep, monthDay]
  mod4 <- nonTerminal "Mod4"
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

  centuryLeapYear <- nonTerminal "CenturyLeapYear"
    [ ("LeapYear0", [zero, zero, zero, zero])
    , ("LeapYearMod4", [mod4, zero, zero])
    ]
  nonCenturyLeapYear <- record "NonCenturyLeapYear" [d0'9, d0'9, mod4]

  leapYear <- union "LeapYear" [ centuryLeapYear, nonCenturyLeapYear ]
  leapDay <- record "LeapDay"
    [ leapYear, dateSep, zero, two, dateSep, two, nine ]

  date <- union "Date" [ nonLeapDay, leapDay ]

  -- Main grammar
  newline <- terminal "Newline" $ solo '\n'

  -- Comments
  hash <- terminal "Hash" $ solo '#'
  commentChar <- terminal "CommentChar" $ include minBound maxBound
    <> pariah '\n'
  commentChars <- list commentChar
  comment <- record "Comment" [hash, commentChars, newline]

  -- Time
  maybe0or1 <- option d0'1
  n0'19 <- record "N0'19" [maybe0or1, d0'9]
  n20'23 <- record "N20'23" [two, d0'3]
  hours <- union "Hours" [n0'19, n20'23]

  n0'59 <- record "N0'59" [d0'5, d0'9]
  minutes <- wrap "Minutes" n0'59
  seconds <- wrap "Seconds" n0'59
  colon <- terminal "Colon" $ solo ':'
  colonSeconds <- record "ColonSeconds" [colon, seconds]
  maybeSeconds <- option colonSeconds
  time <- record "Time" [hours, colon, minutes, maybeSeconds]
  plus <- terminal "Plus" $ solo '+'
  minus <- terminal "Minus" $ solo '-'
  pluMin <- union "PluMin" [plus, minus]
  zoneHrsMins <- record "ZoneHrsMins" [pluMin, d0'2, d0'3, d0'9, d0'9]
  backtick <- terminal "Backtick" $ solo '`'
  zone <- record "Zone" [backtick, zoneHrsMins]

  -- Whitespace and quoted strings
  doubleQuote <- terminal "DoubleQuote" $ solo '"'
  backslash <- terminal "Backslash" $ solo '\\'
  space <- terminal "Space" $ solo ' '
  tab <- terminal "Tab" $ solo '\t'
  white <- union "White" [space, tab, newline, comment]
  whites <- list white
  whites1 <- list1 white
  gap <- record "Gap" [whites1, backslash]
  escPayload <- union "EscPayload" [backslash, newline, doubleQuote, gap]
  nonEscapedChar <- terminal "NonEscapedChar" $ include minBound maxBound
    <> pariah '\\' <> pariah '\n' <> pariah '"'
  escSeq <- record "EscSeq" [backslash, escPayload]
  quotedChar <- union "QuotedChar" [nonEscapedChar, escSeq]
  quotedChars <- list quotedChar
  quotedString <- record "QuotedString"
    [doubleQuote, quotedChars, doubleQuote]

  -- Unquoted strings
  unquotedStringNonDigitChar <-
    let pariahs = [ ' ', '\\', '\n', '\t', '{', '}', '[', ']', '\'', '"',
          '-', '+', '/', ':', '#', '@', '`', '<', '>', ';',
          '_', '\x2009', ',', '.' ] ++ ['0'..'9']
        interval = include minBound maxBound <> mconcat (map pariah pariahs)
    in terminal "UnquotedStringNonDigitChar" interval
  unquotedStringEndChar <- union "UnquotedStringNonFirstChar"
    [unquotedStringNonDigitChar, d0'9]
  unquotedStringEndChars <- list unquotedStringEndChar
  unquotedString <- record "UnquotedString"
    [digits, unquotedStringNonDigitChar, unquotedStringEndChars]

  unquotedStringNonDigitChars1 <- list1 unquotedStringNonDigitChar

  -- Commodities
  unquotedCommodity <- wrap "UnquotedCommodity"
    unquotedStringNonDigitChars1
  quotedCommodity <- wrap "QuotedCommmodity" quotedString
  commodity <- union "Commodity" [unquotedCommodity, quotedCommodity]

  -- NonNeutral
  nonNeutral <- nonTerminal "NonNeutral"
    [ ("NonNeutralRadCom", [backtick, brimRadCom])
    , ("NonNeutralRadPer", [brimRadPer])
    ]

  neutral <- nonTerminal "Neutral"
    [ ("NeuCom", [backtick, nilRadCom])
    , ("NeuPer", [nilRadPer])
    ]

  -- Whole
  maybePluMin <- option pluMin
  wholeNonZero <- record "WholeNonZero" [maybePluMin, d1'9, digits]
  wholeAny <- union "WholeAny" [zero, wholeNonZero]

  lessThan <- terminal "LessThan" $ solo '<'
  greaterThan <- terminal "GreaterThan" $ solo '>'
  debit <- wrap "Debit" lessThan
  credit <- wrap "Credit" greaterThan
  debitCredit <- union "DebitCredit" [debit, credit]

  -- Trio
  --
  -- If there is a debit or credit present, it always appears first.

  -- If there is a debit or credit present, there can be a non-neutral
  -- present.  Also, a commodity may be present.
  t_DebitCredit <- record "T_DebitCredit" [ debitCredit, whites ]

  t_DebitCredit_Commodity <- record "T_DebitCredit_Commodity"
    [ debitCredit, whites, commodity, whites ]

  t_DebitCredit_NonNeutral <- record "T_DebitCredit_NonNeutral"
    [ debitCredit, whites, nonNeutral, whites]

  t_DebitCredit_Commodity_NonNeutral <-
    record "T_DebitCredit_Commodity_NonNeutral"
    [ debitCredit, whites, commodity, whites, nonNeutral, whites]
  t_DebitCredit_NonNeutral_Commodity <- record
    "T_DebitCredit_NonNeutral_Commodity"
    [ debitCredit, whites, nonNeutral, whites, commodity, whites]

  -- If there is no debit or credit present, there may be only a commodity.
  t_Commodity <- record "T_Commodity" [ commodity, whites ]

  -- There can be no debit or credit, with a commodity and either a
  -- neutral or non-neutral.

  t_Commodity_Neutral <- record "T_Commodity_Neutral"
    [ commodity, whites, neutral, whites ]
  t_Neutral_Commodity <- record "T_Neutral_Commodity"
    [ neutral, whites, commodity, whites ]
  t_Commodity_NonNeutral <- record "T_Commodity_NonNeutral"
    [ commodity, whites, nonNeutral, whites ]
  t_NonNeutral_Commodity <- record "T_NonNeutral_Commodity"
    [ nonNeutral, whites, commodity, whites ]

  -- A neutral or non-neutral standing alone is possible.
  t_Neutral <- record "T_Neutral" [ neutral, whites ]
  t_NonNeutral <- record "T_NonNeutral" [ nonNeutral, whites ]

  trio <- union "Trio"
    [ t_DebitCredit,

      t_DebitCredit_Commodity,

      t_DebitCredit_NonNeutral,

      t_DebitCredit_Commodity_NonNeutral, t_DebitCredit_NonNeutral_Commodity,

      t_Commodity,

      t_Commodity_Neutral, t_Neutral_Commodity,
      t_Commodity_NonNeutral, t_NonNeutral_Commodity,

      t_Neutral, t_NonNeutral
    ]

  -- Scalar

  openSquare <- terminal "OpenSquare" $ solo '['
  closeSquare <- terminal "CloseSquare" $ solo ']'
  scalar <- union "Scalar" [unquotedString, quotedString, date, time,
    zone, wholeAny]
  maybeScalar <- option scalar

  -- Trees
  bracketedForest <- record "BracketedForest"
    [ openSquare, whites, forest, closeSquare, whites ]
  maybeBracketedForest <- option bracketedForest
  commaTree <- record "CommaTree" [comma, whites, tree, whites]
  commaTrees <- list commaTree
  forest <- record "Forest"
    [ tree, whites, commaTrees ]
  tree <- nonTerminal "Tree"
    [ ("TreeScalarFirst", [scalar, maybeBracketedForest])
    , ("TreeForestFirst", [bracketedForest, maybeScalar])
    ]

  topLine <- wrap "TopLine" forest
  posting <- nonTerminal "Posting"
    [ ("PostingTrioFirst", [trio, maybeBracketedForest])
    , ("PostingNoTrio", [bracketedForest])
    ]

  openCurly <- terminal "OpenCurly" $ solo '{'
  closeCurly <- terminal "CloseCurly" $ solo '}'
  semicolon <- terminal "Semicolon" $ solo ';'

  semiPosting <- record "SemiPosting" [semicolon, whites, posting]
  semiPostings <- list semiPosting
  postingList <- record "PostingList" [posting, semiPostings]
  maybePostingList <- option postingList
  postings <- record "Postings"
    [openCurly, whites, maybePostingList, closeCurly, whites]

  maybeTopLine <- option topLine
  transaction <- record "Transaction" [maybeTopLine, postings]

  atSign <- terminal "AtSign" $ solo '@'
  pluMinFs <- record "PluMinFs" [pluMin, whites]
  maybePluMinFs <- option pluMinFs
  exch <- nonTerminal "Exch"
    [ ("ExchNeutral", [neutral, whites])
    , ("ExchNonNeutral", [maybePluMinFs, nonNeutral, whites])
    ]
  cyExch <- nonTerminal "CyExch"
    [ ("CyExchCy", [commodity, whites, exch])
    , ("CyExchExch", [exch, commodity, whites])
    ]
  mayTimeWhites <- nonTerminal "TimeWhites'Optional"
    [ ("TimeWhitesYes", [time, whites])
    , ("TimeWhitesNo", [])
    ]
  mayZoneWhites <- nonTerminal "ZoneWhites'Optional"
    [ ("ZoneWhitesYes", [zone, whites])
    , ("ZoneWhitesNo", [])
    ]
  price <- record "Price"
    [ atSign, whites, date, whites, mayTimeWhites, mayZoneWhites,
      commodity, whites, cyExch ]
  fileItem <- union "FileItem" [price, transaction]
  fileItems <- list fileItem
  wholeFile <- record "WholeFile" [whites, fileItems]
  return wholeFile

