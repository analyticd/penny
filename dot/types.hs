{-# LANGUAGE RecursiveDo #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
module Main where


import qualified Types as T
import Dot
import Prelude hiding
  ( maybe
  , product
  )


brim
  :: T.Tycon
  -- ^ NovDecs
  -> T.Tycon
  -- ^ Decems
  -> T.Tycon
  -- ^ DecDecs
  -> T.Tycon
  -- ^ Radix
  -> T.Tycon
  -- ^ Zero
  -> T.Tycon
  -- ^ Zeroes
  -> T.Tycon
  -- ^ Grouper
  -> T.Tycon
  -- ^ Maybe Zero
  -> Dot T.Tycon
brim novDecs decems decDecs radix zero zeroes grouper maybe_zero = mdo
  ungrouped <- brimUngrouped novDecs decems radix zero zeroes
  grouped <- brimGrouped novDecs decDecs grouper radix maybe_zero
    zeroes
  T.brim grouped ungrouped

brimGrouped
  :: T.Tycon
  -- ^ NovDecs
  -> T.Tycon
  -- ^ DecDecs r
  -> T.Tycon
  -- ^ Grouper
  -> T.Tycon
  -- ^ Radix r
  -> T.Tycon
  -- ^ Maybe Zero
  -> T.Tycon
  -- ^ Zeroes
  -> Dot T.Tycon
brimGrouped novDecs decDecs grouper radix maybe_zero zeroes = mdo
  brimGrouped <- T.brimGrouped novDecs bg1 bg4
  bg1 <- T.bg1 grouper decDecsMayGroups maybe_bg2 radix
  decDecsMayGroups <- T.decDecsMayGroups decDecs seqDecs
  seqDecs <- T.seqDecs (T.tyName grouper)
  maybe_bg2 <- T.maybe bg2
  bg2 <- T.bg2 radix maybe_decDecsMayGroups
  maybe_decDecsMayGroups <- T.maybe decDecsMayGroups
  bg4 <- T.bg4 maybe_zero radix bg5
  bg5 <- T.bg5 novSeqDecsNE zeroes bg6
  novSeqDecsNE <- T.novSeqDecsNE novDecs seqDecsNE
  seqDecsNE <- T.seqDecsNE decsGroup seqDecs
  decsGroup <- T.decsGroup grouper decDecs
  bg6 <- T.bg6 novSeqDecsNE grouper bg7
  bg7 <- T.bg7 zeroes grouper nodecs3
  nodecs3 <- T.nodecs3 novDecs seqDecs
  return brimGrouped


brimUngrouped
  :: T.Tycon
  -- ^ NovDecs
  -> T.Tycon
  -- ^ Decems
  -> T.Tycon
  -- ^ Radix r
  -> T.Tycon
  -- ^ Zero
  -> T.Tycon
  -- ^ Zeroes
  -> Dot T.Tycon
brimUngrouped novDecs decems radix zero zeroes = mdo
  brimUngrouped <- T.brimUngrouped nodbu bu2
  nodbu <- T.nodbu novDecs maybe_radem
  maybe_radem <- T.maybe radem
  radem <- T.radem radix decems
  bu2 <- T.bu2 zerabu radbu
  zerabu <- T.zerabu zero radix bu3
  bu3 <- T.bu3 zenod novDecs
  zenod <- T.zenod zeroes novDecs
  radbu <- T.radbu radix bu3
  return brimUngrouped

nil
  :: T.Tycon
  -- ^ Radix r
  -> T.Tycon
  -- ^ Zeroes
  -> T.Tycon
  -- ^ Grouper
  -> T.Tycon
  -- ^ Zero
  -> T.Tycon
  -- ^ Maybe Zeroes
  -> Dot T.Tycon
nil radix zeroes grouper zero maybe_zeroes = mdo
  grouped <- nilGrouped radix zeroes grouper zero
  ungrouped <- nilUngrouped radix zero zeroes maybe_zeroes
  T.nil ungrouped grouped

nilGrouped
  :: T.Tycon
  -- ^ Radix a
  -> T.Tycon
  -- ^ Zeroes
  -> T.Tycon
  -- ^ Grouper
  -> T.Tycon
  -- ^ Zero
  -> Dot T.Tycon
nilGrouped radix zeroes grouper zero = mdo
  nilGrouped <- T.nilGrouped zng ng1
  zng <- T.zng zero ng1
  ng1 <- T.ng1 radix zeroes grouper seq_groups
  seq_groups <- T.seq (T.copyTy "ZGroup.T" grouper)
  return nilGrouped

nilUngrouped
  :: T.Tycon
  -- ^ Radix a
  -> T.Tycon
  -- ^ Zero
  -> T.Tycon
  -- ^ Zeroes
  -> T.Tycon
  -- ^ Maybe Zeroes
  -> Dot T.Tycon
nilUngrouped radix zero zeroes maybe_zeroes = mdo
  nilUngrouped <- T.nilUngrouped znu1 radZ
  znu1 <- T.znu1 zero maybe_radun
  radun <- T.radun radix maybe_zeroes
  maybe_radun <- T.maybe radun
  radZ <- T.radZ radix zeroes
  return nilUngrouped

anna
  :: T.Tycon
  -- ^ Zeroes
  -> T.Tycon
  -- ^ Zero
  -> T.Tycon
  -- ^ Maybe Zeroes
  -> T.Tycon
  -- ^ NovDecs
  -> T.Tycon
  -- ^ Decmes
  -> T.Tycon
  -- ^ DecDecs
  -> T.Tycon
  -- ^ Maybe Zero
  -> T.Tycon
  -- ^ Radix
  -> T.Tycon
  -- ^ Grouper
  -> Dot T.Tycon
anna zeroes zero maybe_zeroes novDecs decems
  decDecs maybe_zero radix grouper = do
  annaNil <- nil radix zeroes grouper zero maybe_zeroes
  annaBrim <- brim novDecs decems decDecs radix zero zeroes grouper
    maybe_zero
  T.anna annaNil annaBrim

main :: IO ()
main = putStr . showDot $ mdo
  -- Primitives
  bool <- T.bool
  int <- T.int
  string <- T.string
  unit <- T.unit
  let unit_name = T.tyName unit

  -- Penny types
  anna_unit <- T.anna nil_unit brim_unit
  arithmeticError <- T.arithmeticError string
  arrangement <- T.arrangement orient spaceBetween
  bg1_unit <- T.bg1 unit decDecsMayGroups_unit maybe_bg2_unit radix_unit
  bg2_unit <- T.bg2 radix_unit maybe_decDecsMayGroups_unit
  bg4_unit <- T.bg4 maybe_zero radix_unit bg5_unit
  bg5_unit <- T.bg5 novSeqDecsNE_unit zeroes bg6_unit
  bg6_unit <- T.bg6 novSeqDecsNE_unit unit bg7_unit
  bg7_unit <- T.bg7 zeroes unit nodecs3_unit
  brim_unit <- T.brim brimGrouped_unit brimUngrouped_unit
  brimGrouped_unit <- T.brimGrouped novDecs bg1_unit bg4_unit
  brimUngrouped_unit <- T.brimUngrouped nodbu_unit bu2_unit
  bu2_unit <- T.bu2 zerabu_unit radbu_unit
  bu3 <- T.bu3 zenod novDecs
  decDecs <- T.decDecs decem decems
  decDecsMayGroups_unit <- T.decDecsMayGroups decDecs seqDecs_unit
  decem <- T.decem novem
  decems <- T.decems
  decsGroup_unit <- T.decsGroup unit decDecs
  grouper_unit <- T.grouper unit
  maybe_bg2_unit <- T.maybe bg2_unit
  maybe_decDecsMayGroups_unit <- T.maybe decDecsMayGroups_unit
  maybe_radem_unit <- T.maybe radem_unit
  maybe_radun_unit <- T.maybe radun_unit
  maybe_zero <- T.maybe zero
  maybe_zeroes <- T.maybe zeroes
  ng1_unit <- T.ng1 radix_unit zeroes unit seq_ZGroup_unit
  nil_unit <- T.nil nilUngrouped_unit nilGrouped_unit
  nilGrouped_unit <- T.nilGrouped zng_unit ng1_unit
  nilUngrouped_unit <- T.nilUngrouped znu1_unit radZ_unit
  nodbu_unit <- T.nodbu novDecs maybe_radem_unit
  nodecs3_unit <- T.nodecs3 novDecs seqDecs_unit
  nonZero <- T.nonZero
  novDecs <- T.novDecs novem decems
  novem <- T.novem
  novSeqDecsNE_unit <- T.novSeqDecsNE novDecs seqDecsNE_unit
  orient <- T.orient
  radbu_unit <- T.radbu radix_unit bu3
  radem_unit <- T.radem radix_unit decems
  radix_unit <- T.radix unit_name
  radun_unit <- T.radun radix_unit maybe_zeroes
  radZ_unit <- T.radZ radix_unit zeroes
  seq_ZGroup_unit <- T.seq (T.Typename "ZGroup.T" [unit_name])
  seqDecs_unit <- T.seqDecs unit_name
  seqDecsNE_unit <- T.seqDecsNE decsGroup_unit seqDecs_unit
  spaceBetween <- T.spaceBetween bool
  zenod <- T.zenod zeroes novDecs
  zerabu_unit <- T.zerabu zero radix_unit bu3
  zero <- T.zero
  zeroes <- T.zeroes nonZero
  zng_unit <- T.zng zero ng1_unit
  znu1_unit <- T.znu1 zero maybe_radun_unit

  return ()
