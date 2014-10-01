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
import qualified Penny.Core.Gravel as Gravel
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

data T r
  = Grouped (BG.T r)
  | Ungrouped (BU.T r)
  deriving (Eq, Ord, Show)

toGravel :: T r -> Gravel.T ()
toGravel (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups Nothing))) = Gravel.T (Just ((), nd')) Exp.Zero
  where
    nd' = novDecs `NovDecs.appendDecems`
      (DecDecsMayGroups.toDecems ddMayGroups)

toGravel (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups (Just (BG2.T _rdx Nothing))))) =
  Gravel.T (Just ((), nd')) Exp.Zero
  where
    nd' = novDecs `NovDecs.appendDecems`
      (DecDecsMayGroups.toDecems ddMayGroups)

toGravel (Grouped (BG.Masuno novDecs (BG1.GroupOnLeft _
  ddMayGroups (Just (BG2.T _rdx (Just ddmg2)))))) =
  Gravel.T (Just ((), nd')) . Exp.fromUnsigned . Unsigned.fromNonZero
  . DecDecsMayGroups.numDigits $ ddmg2
  where
    nd' = novDecs `NovDecs.appendDecems`
      ( DecDecsMayGroups.toDecems ddMayGroups
        <> DecDecsMayGroups.toDecems ddmg2 )


toGravel (Grouped (BG.Masuno novDecs
  (BG1.GroupOnRight _rdx decDecs decsGroup seqDecs)))
  = Gravel.T (Just ((), nd'))
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

toGravel (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Novem nsdne))))
  = Gravel.T (Just ((), NovSeqDecsNE.toNovDecs nsdne))
  . Exp.fromUnsigned . Unsigned.fromNonZero
  . NovSeqDecsNE.numDigits $ nsdne


toGravel (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Zeroes zs1
  (BG6.Novem nsdne)))))
  = Gravel.T (Just ((), NovSeqDecsNE.toNovDecs nsdne))
  . Exp.fromUnsigned . Unsigned.fromNonZero
  . NonZero.add (Zeroes.numDigits zs1)
  . NovSeqDecsNE.numDigits
  $ nsdne


toGravel (Grouped (BG.Fracuno (BG4.T _ _rdx (BG5.Zeroes zs1
  (BG6.Group _ bg7))))) =
  Gravel.T (Just ((), Nodecs3.toNovDecs nodecs3))
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

toGravel (Ungrouped (BU.Masuno (Nodbu.T nd Nothing)))
  = Gravel.T (Just ((), nd)) Exp.Zero


toGravel (Ungrouped (BU.Masuno (Nodbu.T nd (Just (Radem.T _rdx decems)))))
  = Gravel.T (Just ((), nd `NovDecs.appendDecems` decems))
  . Exp.fromUnsigned
  . Decems.numDigits
  $ decems


toGravel (Ungrouped (BU.Fracuno (BU2.T _ _rdx
  (BU3.Zeroes (Zenod.T zs nd))))) =
  Gravel.T (Just ((), nd))
  . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . NonZero.add (Zeroes.numDigits zs)
  . NovDecs.numDigits
  $ nd

toGravel (Ungrouped (BU.Fracuno (BU2.T _ _rdx
  (BU3.NoZeroes nd)))) =
  Gravel.T (Just ((), nd))
  . Exp.fromUnsigned
  . Unsigned.fromNonZero
  . NovDecs.numDigits
  $ nd

