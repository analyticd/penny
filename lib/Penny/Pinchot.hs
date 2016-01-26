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
    [ ("D1'9_1", one), ("D1'9_2", two), ("D1'9_3", three),
      ("D1'9_4", four), ("D1'9_5", five), ("D1'9_6", six),
      ("D1'9_7", seven), ("D1'9_8", eight), ("D1'9_9", nine) ]

  -- Digit from 0 through 9
  d0'9 <- union "D0'9"
    [ ("D0'9_0", zero),
      ("D0'9_1", one), ("D0'9_2", two), ("D0'9_3", three),
      ("D0'9_4", four), ("D0'9_5", five), ("D0'9_6", six),
      ("D0'9_7", seven), ("D0'9_8", eight), ("D0'9_9", nine) ]

  -- Digit from 0 through 8
  d0'8 <- union "D0'8"
    [ ("D0'8_0", zero),
      ("D0'8_1", one), ("D0'8_2", two), ("D0'8_3", three),
      ("D0'8_4", four), ("D0'8_5", five), ("D0'8_6", six),
      ("D0'8_7", seven), ("D0'8_8", eight)
    ]

  -- # Groupers
  thinSpace <- terminal "ThinSpace" (solo '\x2009')
  underscore <- terminal "Underscore" (solo '_')
  period <- terminal "Period" (solo '.')
  comma <- terminal "Comma" (solo ',')
  grouper <- union "Grouper" [ ("GThinSpace", thinSpace),
                               ("GUnderscore", underscore) ]
  grpRadCom <- union "GrpRadCom" [("RCPeriod", period), ("RCGrouper", grouper)]
  grpRadPer <- union "GrpRadPer" [("RPComma", comma), ("RPGrouper", grouper)]

  -- # Radix
  radixCom <- terminal "RadixCom" (solo ',')
  radixPer <- terminal "RadixPer" (solo '.')

  debit <- terminal "Debit" (solo '<')
  credit <- terminal "Credit" (solo '>')

  -- # Groups of digits
  digits <- list "Digits" d0'9
  digitGroupRadCom <- record "DigitGroupRadCom"
    [grpRadCom, d0'9, digits]
  digitGroupRadPer <- record "DigitGroupRadPer"
    [grpRadPer, d0'9, digits]
  digitGroupsRadCom <- list "DigitGroupsRadCom" digitGroupRadCom
  digitGroupsRadPer <- list "DigitGroupsRadPer" digitGroupRadPer

  -- # Nil

  maybeZero <- option "MaybeZero" zero
  zeroes <- list "Zeroes" zero
  radixZeroesRadCom <- record "RadixZeroesRadCom" [radixCom, zeroes]
  radixZeroesRadPer <- record "RadixZeroesRadPer" [radixPer, zeroes]
  maybeRadixZeroesRadCom <- option "MaybeRadixZeroesRadCom" radixZeroesRadCom
  maybeRadixZeroesRadPer <- option "MaybeRadixZeroesRadPer" radixZeroesRadPer
  zeroGroupRadCom <- record "ZeroGroupRadCom"
    [grpRadCom, zero, zeroes]
  zeroGroupRadPer <- record "ZeroGroupRadPer"
    [grpRadPer, zero, zeroes]

  nilGroupedRadCom <- record "NilGroupedRadCom"
    [maybeZero, radixCom, zero, zeroes, grpRadCom, zero, zeroes,
     zeroGroupRadCom]
  nilGroupedRadPer <- record "NilGroupedRadPer"
    [maybeZero, radixPer, zero, zeroes, grpRadPer, zero, zeroes,
     zeroGroupRadPer]


  nilUngroupedRadCom <- nonTerminal "NilUngroupedRadCom"
    [ ("NUZeroRadCom", [zero, maybeRadixZeroesRadCom])
    , ("NURadixRadCom", [radixCom, zero, zeroes])
    ]
  nilUngroupedRadPer <- nonTerminal "NilUngroupedRadPer"
    [ ("NUZeroRadPer", [zero, maybeRadixZeroesRadPer])
    , ("NURadixRadPer", [radixPer, zero, zeroes])
    ]

  nilRadCom <- union "NilRadCom"
    [ ("NilRadComUngrouped", nilUngroupedRadCom)
    , ("NilRadComGrouped", nilGroupedRadCom) ]
  nilRadPer <- union "NilRadPer"
    [ ("NilRadPerUngrouped", nilUngroupedRadPer)
    , ("NilRadPerGrouped", nilGroupedRadPer) ]

  -- # Brim

  radixComDigits <- record "RadixComDigits" [radixCom, digits]
  radixPerDigits <- record "RadixPerDigits" [radixPer, digits]
  maybeRadixComDigits <- option "MaybeRadixComDigits" radixComDigits
  maybeRadixPerDigits <- option "MaybeRadixPerDigits" radixPerDigits
  brimUngroupedRadCom <- nonTerminal "BrimUngroupedRadCom"
    [ ("BUGreaterThanOneRadCom", [d0'9, digits, maybeRadixComDigits])
    , ("BULessThanOneRadCom", [maybeZero, radixCom, zeroes, d1'9, digits])
    ]
  brimUngroupedRadPer <- nonTerminal "BrimUngroupedRadPer"
    [ ("BUGreaterThanOneRadPer", [d0'9, digits, maybeRadixPerDigits])
    , ("BULessThanOneRadPer", [maybeZero, radixPer, zeroes, d1'9, digits])
    ]

  -- # BrimGrouped

  bg8RadCom <- nonTerminal "BG8RadCom"
    [ ("BG8NovemRadCom", [d0'9, digits, digitGroupsRadCom])
    , ("BG8GroupRadCom", [grpRadCom, bg7RadCom])
    ]
  bg8RadPer <- nonTerminal "BG8RadPer"
    [ ("BG8NovemRadPer", [d0'9, digits, digitGroupsRadPer])
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

  -- # Dates
  dateSep <- terminal "DateSep" (solo '/' <> solo '-')
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

  leapYear <- union "LeapYear" [ ("LYCentury", centuryLeapYear)
                               , ("LYNonCentury", nonCenturyLeapYear)
                               ]
  leapDay <- record "LeapDay"
    [ leapYear, dateSep, zero, two, dateSep, two, nine ]

  date <- union "Date" [ ("DateNonLeap", nonLeapDay)
                       , ("DateLeap", leapDay)
                       ]

  return d0'9

