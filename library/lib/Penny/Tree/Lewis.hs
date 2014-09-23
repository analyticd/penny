{-# LANGuAGE ViewPatterns #-}
module Penny.Tree.Lewis where

import Data.Sequence (ViewL(..), (<|))
import qualified Data.Sequence as S
import qualified Penny.Tree.Masuno1 as Masuno1
import qualified Penny.Tree.Masuno1Radix1 as Masuno1Radix1
import qualified Penny.Core.Anna.Zero as Zero
import qualified Penny.Tree.LZ1 as LZ1
import qualified Penny.Tree.LZ2 as LZ2
import qualified Penny.Tree.LZ3 as LZ3
import qualified Penny.Tree.LZ6 as LZ6
import qualified Penny.Tree.LZ6.Runner as Runner
import qualified Penny.Tree.LZ6.Collector as Collector
import qualified Penny.Tree.LZ6.Zero as LZ6.Zero
import qualified Penny.Tree.LR1 as LR1
import Text.Parsec.Text
import Control.Applicative
import qualified Penny.Core.Anna.Radix as Radix
import qualified Penny.Core.Anna.Radun as Radun
import qualified Penny.Core.Anna.Radbu as Radbu
import qualified Penny.Core.Anna as Anna
import qualified Penny.Core.Anna.BrimUngrouped as BrimU
import qualified Penny.Core.Anna.BrimGrouped as BrimG
import qualified Penny.Core.Anna.Brim as Brim
import qualified Penny.Core.Anna.NovDecs as NovDecs
import qualified Penny.Core.Anna.Nodbu as Nodbu
import qualified Penny.Core.Anna.Decems as Decems
import qualified Penny.Core.Anna.Radem as Radem
import qualified Penny.Core.Anna.RadZ as RadZ
import qualified Penny.Core.Anna.DecDecs as DecDecs
import qualified Penny.Core.Anna.BG1 as BG1
import qualified Penny.Core.Anna.BG4 as BG4
import qualified Penny.Core.Anna.BG5 as BG5
import qualified Penny.Core.Anna.BG6 as BG6
import qualified Penny.Core.Anna.BG7 as BG7
import qualified Penny.Core.Anna.NovSeqDecsNE as NovSeqDecsNE
import qualified Penny.Core.Anna.SeqDecsNE as SeqDecsNE
import qualified Penny.Core.Anna.SeqDecs as SeqDecs
import qualified Penny.Core.Anna.Nodecs3 as Nodecs3
import qualified Penny.Core.Anna.Nil.Ungrouped as NilU
import qualified Penny.Core.Anna.Nil.Grouped as NilG
import qualified Penny.Core.Anna.Znu1 as Znu1
import qualified Penny.Core.Anna.Zng as Zng
import qualified Penny.Core.Anna.NG1 as NG1
import qualified Penny.Core.Anna.Nil as Nil
import qualified Penny.Core.Anna.BU2 as BU2
import qualified Penny.Core.Anna.BU3 as BU3
import qualified Penny.Core.Anna.Zerabu as Zerabu
import qualified Penny.Core.Anna.Zenod as Zenod
import qualified Penny.Core.Anna.ZGroup as ZGroup

-- | The root of parse trees for number representations.
data T a
  = Novem NovDecs.T (Maybe (Masuno1.T a))
  | Zero Zero.T (Maybe (LZ1.T a))
  | Radix (Radix.T a) (LR1.T a)
  deriving (Eq, Ord, Show)

parser :: Parser (Radix.T a) -> Parser a -> Parser (T a)
parser pr pa =
  Novem <$> NovDecs.parser
        <*> optional (Masuno1.parser pr pa)

  <|> Zero <$> Zero.parser <*> optional (LZ1.parser pr pa)
  <|> Radix <$> pr <*> LR1.parser pa

toAnna :: T a -> Anna.T a

toAnna (Radix radix (LR1.Zero zeroes Nothing)) =
  Anna.Nil . Nil.Ungrouped . NilU.NoLeadingZero
  . RadZ.T radix $ zeroes

toAnna (Radix radix (LR1.Zero zeroes
  (Just (LZ3.Novem novDecs Nothing)))) =
  Anna.Brim . Brim.Ungrouped . BrimU.Fracuno . BU2.NoLeadingZero
  . Radbu.T radix . BU3.Zeroes . Zenod.T zeroes $ novDecs

toAnna (Radix radix (LR1.Zero zeroes (Just (LZ3.Novem novDecs
  (Just seqDecsNE))))) =
  Anna.Brim . Brim.Grouped . BrimG.Fracuno . BG4.T Nothing radix
  . BG5.Zeroes zeroes . BG6.Novem . NovSeqDecsNE.T novDecs $ seqDecsNE

-- The view pattern in the go function will emit a bogus
-- non-exhaustive pattern warning - see GHC version 7.8.3 manual
-- section 14.2.1
toAnna (Radix radix (LR1.Zero zeroes (Just (LZ3.Group grpr1
  (Runner.runFold -> Collector.T sqz (LZ6.Zero.Novem nd Nothing)))))) =
  Anna.Brim . Brim.Grouped . BrimG.Fracuno . BG4.T Nothing radix
  . BG5.Zeroes zeroes . BG6.Group grpr1 . go $ sqz
  where
    go (S.viewl -> EmptyL) = BG7.LeadNovem . Nodecs3.T nd $ SeqDecs.empty
    go (S.viewl -> (zeroes', grpr') :< rest) =
      BG7.LeadZeroes zeroes' (Left (grpr', go rest))

-- The view pattern in the go function will emit a bogus
-- non-exhaustive pattern warning - see GHC version 7.8.3 manual
-- section 14.2.1
toAnna (Radix radix (LR1.Zero zeroes (Just (LZ3.Group grpr1
  (Runner.runFold -> Collector.T sqz (LZ6.Zero.Novem nd (Just grps))))))) =
  Anna.Brim . Brim.Grouped . BrimG.Fracuno . BG4.T Nothing radix
  . BG5.Zeroes zeroes . BG6.Group grpr1 . go $ sqz
  where
    go (S.viewl -> EmptyL) = BG7.LeadNovem . Nodecs3.T nd
      . SeqDecsNE.toSeqDecs $ grps
    go (S.viewl -> (zeroes', grpr') :< rest) =
      BG7.LeadZeroes zeroes' (Left (grpr', go rest))

toAnna (Radix radix (LR1.Zero zeroes (Just (LZ3.Group grpr1
  (Runner.runFold -> Collector.T sqz (LZ6.Zero.ZeroOnly zeroes2)))))) =
  Anna.Nil . Nil.Grouped . NilG.NoLeadingZero
  . uncurry (NG1.T radix zeroes grpr1) . go $ sqz
  where
    go = undefined


toAnna (Radix radix (LR1.Novem novDecs Nothing)) =
  Anna.Brim . Brim.Ungrouped . BrimU.Fracuno . BU2.NoLeadingZero
  . Radbu.T radix . BU3.NoZeroes $ novDecs

toAnna (Radix radix (LR1.Novem novDecs (Just seqDecsNE))) =
  Anna.Brim . Brim.Grouped . BrimG.Fracuno . BG4.T Nothing radix
  . BG5.Novem . NovSeqDecsNE.T novDecs $ seqDecsNE

toAnna (Novem nd1 Nothing) = Anna.Brim . Brim.Ungrouped . BrimU.Masuno
  $ Nodbu.T nd1 Nothing

toAnna (Novem nd1 (Just (Masuno1.T rdx Nothing))) =
  Anna.Brim . Brim.Ungrouped . BrimU.Masuno
  $ Nodbu.T nd1 (Just (Radem.T rdx Decems.empty))

toAnna (Novem nd1 (Just (Masuno1.T rdx
  (Just (Masuno1Radix1.T dds (S.viewl -> EmptyL)))))) =
  Anna.Brim . Brim.Ungrouped . BrimU.Masuno
  $ Nodbu.T nd1 (Just (Radem.T rdx (DecDecs.toDecems dds)))

toAnna (Novem nd1 (Just (Masuno1.T rdx
  (Just (Masuno1Radix1.T dds (S.viewl -> grp1 :< grpRest)))))) =
  Anna.Brim . Brim.Grouped $ BrimG.Masuno nd1
  (BG1.GroupOnRight rdx dds grp1 grpRest)

toAnna (Zero z1 Nothing) = Anna.Nil . Nil.Ungrouped
  . NilU.LeadingZero $ Znu1.T z1 Nothing

toAnna (Zero z1 (Just (LZ1.T rdx Nothing))) = Anna.Nil
  . Nil.Ungrouped . NilU.LeadingZero
  $ Znu1.T z1 (Just (Radun.T rdx Nothing))

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs Nothing))))) =
  Anna.Nil . Nil.Ungrouped . NilU.LeadingZero
  $ Znu1.T z1 (Just (Radun.T rdx (Just zs)))

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs (Just
  (LZ3.Novem nd Nothing))))))) = Anna.Brim . Brim.Ungrouped
  . BrimU.Fracuno . BU2.LeadingZero
  $ Zerabu.T z1 rdx (BU3.Zeroes (Zenod.T zs nd))

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs (Just
  (LZ3.Novem nd (Just seqDecsNE)))))))) = Anna.Brim . Brim.Grouped
  . BrimG.Fracuno
  $ BG4.T (Just z1) rdx (BG5.Zeroes zs
    (BG6.Novem (NovSeqDecsNE.T nd seqDecsNE)))

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs (Just
  (LZ3.Group g (Runner.runFold -> Collector.T sqq
  (LZ6.Zero.Novem nd sq))))))))) =
  Anna.Brim
  . Brim.Grouped
  . BrimG.Fracuno
  . BG4.T (Just z1) rdx
  . BG5.Zeroes zs
  . BG6.Group g
  $ go sqq
  where
    go sq' = case S.viewl sq' of
      EmptyL -> BG7.LeadNovem . Nodecs3.T nd
        . maybe SeqDecs.empty SeqDecsNE.toSeqDecs $ sq
      (zeroes, grp) :< xs -> BG7.LeadZeroes zeroes
        . Left $ (grp, go xs)

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs (Just
  (LZ3.Group g (Runner.runFold -> Collector.T sqq
  (LZ6.Zero.ZeroOnly zeroes))))))))) =
  Anna.Nil
  . Nil.Grouped
  . NilG.LeadingZero
  . Zng.T z1
  . uncurry (NG1.T rdx zs g)
  $ case S.viewl sqq of
      EmptyL -> (zeroes, S.empty)
      (zgroupA, gA) :< xs -> (zgroupA, go gA xs)
        where
          go gr groups = case S.viewl groups of
            EmptyL -> S.singleton (ZGroup.T gr zeroes)
            (zgroupB, gB) :< bs ->
              ZGroup.T gr zgroupB <| go gB bs

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Zero zs (Just
  (LZ3.Group g (Runner.runFold -> Collector.T sqq
  (LZ6.Zero.ZeroNovSeq zeroes novDecs maySeqDecsNE))))))))) =
  Anna.Brim
  . Brim.Grouped
  . BrimG.Fracuno
  . BG4.T (Just z1) rdx
  . BG5.Zeroes zs
  . BG6.Group g
  $ go sqq
  where
    go sq' = case S.viewl sq' of
      EmptyL -> BG7.LeadZeroes zeroes
        . Right
        . Nodecs3.T novDecs . maybe SeqDecs.empty SeqDecsNE.toSeqDecs
        $ maySeqDecsNE
      (zeroes', grp') :< xs -> BG7.LeadZeroes zeroes'
        . Left . (,) grp' $ go xs

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Novem nd Nothing))))) =
  Anna.Brim
  . Brim.Ungrouped
  . BrimU.Fracuno
  . BU2.LeadingZero
  . Zerabu.T z1 rdx
  . BU3.NoZeroes $ nd

toAnna (Zero z1 (Just (LZ1.T rdx (Just (LZ2.Novem nd
  (Just seqDecsNE)))))) =
  Anna.Brim
  . Brim.Grouped
  . BrimG.Fracuno
  . BG4.T (Just z1) rdx
  . BG5.Novem
  . NovSeqDecsNE.T nd
  $ seqDecsNE
