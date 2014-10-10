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
  -- ^ Decems
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

  -- Penny types - Anna
  arithmeticError <- T.arithmeticError string
  arrangement <- T.arrangement orient spaceBetween
  decDecs <- T.decDecs decem decems
  decem <- T.decem novem
  decems <- T.decems
  maybe_zero <- T.maybe zero
  maybe_zeroes <- T.maybe zeroes
  nonZero <- T.nonZero
  novDecs <- T.novDecs novem decems
  novem <- T.novem
  orient <- T.orient
  radCom <- T.radCom
  radPer <- T.radPer
  radix_radCom <- T.radix (T.tyName radCom)
  radix_radPer <- T.radix (T.tyName radPer)
  spaceBetween <- T.spaceBetween bool
  zenod <- T.zenod zeroes novDecs
  zero <- T.zero
  zeroes <- T.zeroes nonZero

  let mkAnna = anna zeroes zero maybe_zeroes novDecs decems
                    decDecs maybe_zero
  anna_radCom <- mkAnna radix_radCom radCom
  adda_radPer <- mkAnna radix_radPer radPer

  -- Penny types - Concrete
  concrete <- T.concrete

  -- Balance
  

  return ()
