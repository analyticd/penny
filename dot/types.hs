{-# LANGUAGE RecursiveDo #-}
module Main where


import qualified Types as T
import Dot
import Prelude hiding
  ( maybe
  , product
  )

main :: IO ()
main = putStr . showDot $ mdo
  -- Primitives
  bool <- T.bool
  int <- T.int
  string <- T.string
  unit <- T.unit

  -- Penny types
  arithmeticError <- T.arithmeticError string
  arrangement <- T.arrangement orient spaceBetween
  decem <- T.decem novem
  decems <- T.decems
  grouper_unit <- T.grouper unit
  maybe_zeroes <- T.maybe zeroes
  maybe_radun_unit <- T.maybe radun_unit
  ng1_unit <- T.ng1 radix_unit zeroes unit seq_ZGroup_unit
  nilUngrouped_unit <- T.nilUngrouped "()" znu1_unit radZ_unit
  nodecs3 <- T.nodecs3 novDecs seqDecs
  nonZero <- T.nonZero
  novDecs <- T.novDecs novem decems
  novem <- T.novem
  orient <- T.orient
  radix_unit <- T.radix "()"
  radun_unit <- T.radun "()" radix_unit maybe_zeroes
  radZ_unit <- T.radZ "()" radix_unit zeroes
  seq_ZGroup_unit <- T.seq "ZGroup.T ()"
  seqDecs <- T.seqDecs
  spaceBetween <- T.spaceBetween bool
  zero <- T.zero
  zeroes <- T.zeroes nonZero
  znu1_unit <- T.znu1 "()" zero maybe_radun_unit

  return ()
