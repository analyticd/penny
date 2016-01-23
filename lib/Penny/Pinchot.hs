{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedLists #-}
module Penny.Pinchot where

import Pinchot

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



  return d0'9

