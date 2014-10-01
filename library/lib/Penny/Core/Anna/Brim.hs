module Penny.Core.Anna.Brim where

import qualified Penny.Core.Anna.BrimGrouped as BG
import qualified Penny.Core.Anna.BrimUngrouped as BU
import qualified Penny.Core.Anna.BG1 as BG1
import qualified Penny.Core.Anna.BG2 as BG2
import qualified Penny.Core.Anna.BG4 as BG4
import qualified Penny.Core.Anna.BG5 as BG5
import qualified Penny.Core.Anna.BG6 as BG6
import qualified Penny.Core.Anna.BG7 as BG7
import qualified Penny.Core.Anna.Zeroes as Zeroes
import qualified Penny.Core.Anna.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.Core.Anna.DecDecsMayGroups as DecDecsMayGroups
import qualified Penny.Core.Exp as Exp
import qualified Penny.Core.NovDecs as NovDecs
import qualified Penny.Natural.Unsigned as Unsigned
import qualified Penny.Natural.NonZero as NonZero
import qualified Penny.Core.Anna.Nodecs3 as Nodecs3
import qualified Penny.Core.DecDecs as DecDecs
import qualified Penny.Core.Anna.DecsGroup as DecsGroup
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.Nodbu as Nodbu
import qualified Penny.Core.Anna.Radem as Radem
import qualified Penny.Core.Decems as Decems
import qualified Penny.Core.Anna.BU2 as BU2
import qualified Penny.Core.Anna.BU3 as BU3
import qualified Penny.Core.Anna.Zenod as Zenod
import Data.Monoid
import qualified Penny.Core.CoeffExp as CoeffExp

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

toCoeffExp :: T r -> CoeffExp.T
toCoeffExp (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups Nothing))) = CoeffExp.T nd' Exp.Zero
  where
    nd' = novDecs `NovDecs.appendDecems`
      (DecDecsMayGroups.toDecems ddMayGroups)

toCoeffExp (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups (Just (BG2.T _rdx Nothing))))) =
  CoeffExp.T nd' Exp.Zero
  where
    nd' = novDecs `NovDecs.appendDecems`
      (DecDecsMayGroups.toDecems ddMayGroups)

toCoeffExp (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups (Just (BG2.T _rdx (Just ddmg2)))))) =
  CoeffExp.T nd' . Exp.fromUnsigned . Unsigned.fromNonZero
  . DecDecsMayGroups.numDigits $ ddmg2
  where
    nd' = novDecs `NovDecs.appendDecems`
      ( DecDecsMayGroups.toDecems ddMayGroups
        <> DecDecsMayGroups.toDecems ddmg2 )


toCoeffExp (Grouped (BG.Masuno novDecs
  (BG1.GroupOnRight _rdx decDecs decsGroup seqDecs)))
  = CoeffExp.T nd'
  . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . NonZero.add (DecDecs.numDigits decDecs)
  . NonZero.addUnsigned (DecsGroup.numDigits decsGroup)
  . SeqDecs.numDigits
  $ seqDecs
  where
    nd' = novDecs
      `NovDecs.appendDecems` (DecDecs.toDecems decDecs)
      `NovDecs.appendDecems` (DecsGroup.toDecems decsGroup)
      `NovDecs.appendDecems` (SeqDecs.toDecems seqDecs)

toCoeffExp (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Novem nsdne))))
  = CoeffExp.T (NovSeqDecsNE.toNovDecs nsdne)
  . Exp.fromUnsigned . Unsigned.fromNonZero
  . NovSeqDecsNE.numDigits $ nsdne


toCoeffExp (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Zeroes zs1
  (BG6.Novem nsdne)))))
  = CoeffExp.T (NovSeqDecsNE.toNovDecs nsdne)
  . Exp.fromUnsigned . Unsigned.fromNonZero
  . NonZero.add (Zeroes.numDigits zs1)
  . NovSeqDecsNE.numDigits
  $ nsdne


toCoeffExp (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Zeroes zs1
  (BG6.Group _ bg7))))) =
  CoeffExp.T (Nodecs3.toNovDecs nodecs3)
  . Exp.fromUnsigned . Unsigned.fromNonZero
  . NonZero.add (Zeroes.numDigits zs1)
  $ bg7NumDigits
  where
    bg7NumDigits = countDigits bg7
      where
        countDigits (BG7.LeadZeroes zs (Left (_, b7')))
          = NonZero.add (Zeroes.numDigits zs) (countDigits b7')
        countDigits (BG7.LeadZeroes zs (Right nd3))
          = NonZero.add (Zeroes.numDigits zs) (Nodecs3.numDigits nd3)
        countDigits (BG7.LeadNovem nd3) = Nodecs3.numDigits nd3
    nodecs3 = getNodecs3 bg7
      where
        getNodecs3 (BG7.LeadZeroes _ (Left (_, b7')))
          = getNodecs3 b7'
        getNodecs3 (BG7.LeadZeroes _ (Right nd3)) = nd3
        getNodecs3 (BG7.LeadNovem nd3) = nd3

toCoeffExp (Ungrouped (BU.Masuno (Nodbu.T nd Nothing)))
  = CoeffExp.T nd Exp.Zero


toCoeffExp (Ungrouped (BU.Masuno (Nodbu.T nd (Just (Radem.T _rdx decems)))))
  = CoeffExp.T (nd `NovDecs.appendDecems` decems)
  . Exp.fromUnsigned
  . Decems.numDigits
  $ decems


toCoeffExp (Ungrouped (BU.Fracuno (BU2.T _ _rdx
  (BU3.Zeroes (Zenod.T zs nd))))) =
  CoeffExp.T nd
  . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . NonZero.add (Zeroes.numDigits zs)
  . NovDecs.numDigits
  $ nd

toCoeffExp (Ungrouped (BU.Fracuno (BU2.T _ _rdx
  (BU3.NoZeroes nd)))) =
  CoeffExp.T nd
  . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . NovDecs.numDigits
  $ nd

