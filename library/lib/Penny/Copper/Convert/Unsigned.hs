module Penny.Copper.Convert.Unsigned where

import qualified Penny.Copper.Tree.Unsigned as T
import qualified Penny.Numbers.Abstract.Unsigned as U
import Penny.Numbers.Natural (NE(..), lengthNE)
import Deka.Native.Abstract
import Penny.Numbers.Abstract.RadGroup
import Data.Sequence (Seq, viewl, ViewL(..), (|>))
import qualified Data.Sequence as S

toUnsigned :: T.Start r -> U.Unsigned r
toUnsigned s = case s of
  T.Masuno ne m -> U.Brim $ masuno ne m
  T.LeadZero _ lz1 -> leadZero lz1
  T.LeadRadix rdx lr1 -> leadRadix rdx lr1

masuno :: NE Novem Decem -> T.Masuno1 r -> U.Brim r
masuno ne m1 = case m1 of
  T.Masuno1End -> U.BrimUngrouped $ U.BUMasuno ne U.BU1End
  T.Masuno1Radix rdx m1r1 -> masuno1radix1 ne rdx m1r1

masuno1radix1
  :: NE Novem Decem
  -> Radix r
  -> T.Masuno1Radix1 r
  -> U.Brim r
masuno1radix1 nd rdx m1r1 = case m1r1 of
  T.Masuno1Radix1End -> U.BrimUngrouped
    $ U.BUMasuno nd (U.BU1Radix rdx S.empty)
  T.Masuno1Radix1Digs g1 gs -> U.BrimGrouped $ U.BGMasuno nd b1
    where
      b1 = U.BG1GroupOnRight rdx g1 gs

leadZero :: T.LZ1 r -> U.Unsigned r
leadZero lz1 = case lz1 of
  T.LZ1End -> U.Nil . U.NilUngrouped
    $ U.NULeadingZero U.Zero U.NU1End
  T.LZ1Radix rdx lz2 -> procLZ2 rdx lz2

procLZ2
  :: Radix r
  -> T.LZ2 r
  -> U.Unsigned r
procLZ2 rdx lz2 = case lz2 of
  T.LZ2End -> U.Nil . U.NilUngrouped $ U.NULeadingZero U.Zero U.NU1End
  T.LZ2Zeroes zs lz3 -> procLZ3 rdx zs lz3
  T.LZ2NonZero nd lz4 -> procLZ4 rdx nd lz4

procLZ3
  :: Radix r
  -> NE U.Zero U.Zero
  -> T.LZ3 r
  -> U.Unsigned r
procLZ3 rdx zs lz3 = case lz3 of
  T.LZ3End -> U.Nil . U.NilUngrouped . U.NULeadingZero U.Zero
    . U.NU1Radix rdx . U.NU2Zeroes . U.Zeroes . lengthNE $ zs
  T.LZ3NovDecs nd lz5 -> procLZ5 rdx zs nd lz5
  T.LZ3Group r lz6 -> procLZ6 rdx zs r lz6

procLZ4
  :: Radix r
  -> NE Novem Decem
  -> T.LZ4 r
  -> U.Unsigned r
procLZ4 rdx nd lz4 = case lz4 of
  T.LZ4End -> U.Brim . U.BrimUngrouped . U.BUFracuno
    . U.BU2LeadingZero U.Zero rdx $ U.BU3NoZeroes nd
  T.LZ4Groups grp g1 gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 (Just U.Zero) rdx . U.BG5Novem nd $ NE (grp, g1) gs

procLZ5
  :: Radix r
  -> NE U.Zero U.Zero
  -> NE Novem Decem
  -> T.LZ5 r
  -> U.Unsigned r
procLZ5 rdx zs ne lz5 = case lz5 of
  T.LZ5End -> U.Brim . U.BrimUngrouped . U.BUFracuno . U.BU2LeadingZero
    U.Zero rdx . flip U.BU3Zeroes ne . toUZeroes $ zs
  T.LZ5Groups r g1 gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 (Just U.Zero) rdx . U.BG5Zeroes (toUZeroes zs)
    $ U.BG6Novem ne (NE (r, g1) gs)

procLZ6
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> T.LZ6 r
  -> U.Unsigned r

procLZ6 rdx zs r lz6 = case lz6 of
  T.LZ6NovDecs nd gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 (Just U.Zero) rdx . U.BG5Zeroes (toUZeroes zs)
    . U.BG6Group r $ U.BG7Novem nd gs

  T.LZ6Zeroes grpZs lz7 -> procLZ7 rdx zs r grpZs S.empty lz7

toUZeroes :: NE U.Zero U.Zero -> U.Zeroes
toUZeroes = U.Zeroes . lengthNE

procLZ6Recurse
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> NE U.Zero U.Zero
  -> Seq (r, NE U.Zero U.Zero)
  -> r
  -> T.LZ6 r
  -> U.Unsigned r
procLZ6Recurse rdx zs1 gr1 zs2 zss gr2 lz6 = case lz6 of
  T.LZ6NovDecs nd gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 (Just U.Zero) rdx . U.BG5Zeroes (toUZeroes zs1)
    . U.BG6Group gr1 . U.BG7Zeroes (toUZeroes zs2)
    $ makeZeroGroups zss gr2 nd gs
  T.LZ6Zeroes zs lz7 -> procLZ7 rdx zs1 gr1 zs2 (zss |> (gr2, zs)) lz7

makeZeroGroups
  :: Seq (r, NE U.Zero U.Zero)
  -> r
  -> NE Novem Decem
  -> Seq (r, NE Decem Decem)
  -> U.BG8 r
makeZeroGroups sqz grp nd sqg = U.BG8Group grp $ go sqz
  where
    go sq = case viewl sq of
      EmptyL -> U.BG7Novem nd sqg
      (grp', zs) :< sq' -> U.BG7Zeroes (toUZeroes zs)
        . U.BG8Group grp' $ go sq'

procLZ7
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> NE U.Zero U.Zero
  -> Seq (r, NE U.Zero U.Zero)
  -> T.LZ7 r
  -> U.Unsigned r
procLZ7 rdx zs grp grpZs sq lz7 = case lz7 of
  T.LZ7End -> U.Nil . U.NilGrouped . U.NGLeadingZero U.Zero
    . U.NG1 rdx (toUZeroes zs) grp (toUZeroes grpZs) $ sq'
    where
      sq' = fmap (\(r, zz) -> (r, toUZeroes zz)) sq
  T.LZ7Group r lz6 -> procLZ6Recurse rdx zs grp grpZs sq r lz6

  T.LZ7NovDecs nd sq2 -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 (Just U.Zero) rdx . U.BG5Zeroes (toUZeroes zs)
    . U.BG6Group grp . U.BG7Zeroes (toUZeroes grpZs)
    $ makeZeroGroupsThenNovDecs sq nd sq2

makeZeroGroupsThenNovDecs
  :: Seq (r, NE U.Zero U.Zero)
  -> NE Novem Decem
  -> Seq (r, NE Decem Decem)
  -> U.BG8 r
makeZeroGroupsThenNovDecs sqz ne sqd = go sqz
  where
    go sq = case S.viewl sq of
      EmptyL -> U.BG8Novem ne sqd
      (r, nez) :< sq' -> U.BG8Group r . U.BG7Zeroes (toUZeroes nez)
        . go $ sq'

leadRadix :: Radix r -> T.LR1 r -> U.Unsigned r
leadRadix rdx lr1 = case lr1 of
  T.LR1Zero zs lz3 -> execLZ3 rdx zs lz3
  T.LR1NonZero nd lz4 -> execLZ4 rdx nd lz4

-- | Processes an LZ3 where there is a leading radix.
execLZ3
  :: Radix r
  -> NE U.Zero U.Zero
  -> T.LZ3 r
  -> U.Unsigned r
execLZ3 rdx zs lz3 = case lz3 of
  T.LZ3End -> U.Nil . U.NilUngrouped . U.NUNoLeadingZero rdx
    . toUZeroes $ zs
  T.LZ3NovDecs nd lz5 -> execLZ5 rdx zs nd lz5
  T.LZ3Group r lz6 -> execLZ6 rdx zs r lz6

execLZ4
  :: Radix r
  -> NE Novem Decem
  -> T.LZ4 r
  -> U.Unsigned r
execLZ4 rdx ne lz4 = case lz4 of
  T.LZ4End -> U.Brim . U.BrimUngrouped . U.BUFracuno
    . U.BU2NoLeadingZero rdx . U.BU3NoZeroes $ ne
  T.LZ4Groups r g1 gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx
    $ U.BG5Novem ne (NE (r, g1) gs)

execLZ5
  :: Radix r
  -> NE U.Zero U.Zero
  -> NE Novem Decem
  -> T.LZ5 r
  -> U.Unsigned r
execLZ5 rdx zs nd lz5 = case lz5 of
  T.LZ5End -> U.Brim . U.BrimUngrouped . U.BUFracuno
    . U.BU2NoLeadingZero rdx . U.BU3Zeroes (toUZeroes zs) $ nd
  T.LZ5Groups r g1 gs -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx . U.BG5Zeroes (toUZeroes zs)
    . U.BG6Novem nd $ NE (r, g1) gs


execLZ6
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> T.LZ6 r
  -> U.Unsigned r
execLZ6 rdx zs r lz6 = case lz6 of
  T.LZ6NovDecs nd sq -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx . U.BG5Zeroes (toUZeroes zs)
    . U.BG6Group r . U.BG7Novem nd $ sq
  T.LZ6Zeroes zs2 lz7 -> execLZ7 rdx zs r zs2 lz7

execLZ7
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> NE U.Zero U.Zero
  -> T.LZ7 r
  -> U.Unsigned r
execLZ7 rdx zs1 r zs2 lz7 = case lz7 of
  T.LZ7End -> U.Nil . U.NilGrouped . U.NGNoLeadingZero
    . U.NG1 rdx (toUZeroes zs1) r (toUZeroes zs2) $ S.empty
  T.LZ7NovDecs nd sq -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx . U.BG5Zeroes (toUZeroes zs1)
    . U.BG6Group r . U.BG7Zeroes (toUZeroes zs2)
    . U.BG8Novem nd $ sq
  T.LZ7Group r2 lz6 -> execLZ6Recurse rdx zs1 r zs2 S.empty r2 lz6

execLZ6Recurse
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> NE U.Zero U.Zero
  -> Seq (r, NE U.Zero U.Zero)
  -> r
  -> T.LZ6 r
  -> U.Unsigned r
execLZ6Recurse rdx zs1 r1 zs2 sqz r2 lz6 = case lz6 of
  T.LZ6NovDecs nd sqd -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx . U.BG5Zeroes (toUZeroes zs1)
    . U.BG6Group r1 . U.BG7Zeroes (toUZeroes zs2)
    . U.BG8Group r2 $ execBG7Zeroes sqz nd sqd
  T.LZ6Zeroes nez lz7 -> execLZ7Recurse rdx zs1 r1 zs2
    (sqz |> (r2, nez)) lz7

execBG7Zeroes
  :: Seq (r, NE U.Zero U.Zero)
  -> NE Novem Decem
  -> Seq (r, NE Decem Decem)
  -> U.BG7 r
execBG7Zeroes sqz ne sqd = go sqz
  where
    go sq = case viewl sq of
      EmptyL -> U.BG7Novem ne sqd
      (r, zs) :< sq' -> U.BG7Zeroes (toUZeroes zs)
        . U.BG8Group r $ go sq'


execLZ7Recurse
  :: Radix r
  -> NE U.Zero U.Zero
  -> r
  -> NE U.Zero U.Zero
  -> Seq (r, NE U.Zero U.Zero)
  -> T.LZ7 r
  -> U.Unsigned r
execLZ7Recurse rdx zs1 r zs2 sqz lz7 = case lz7 of
  T.LZ7End -> U.Nil . U.NilGrouped . U.NGNoLeadingZero
    . U.NG1 rdx (toUZeroes zs1) r (toUZeroes zs2)
    . fmap (\(x, ne) -> (x, toUZeroes ne)) $ sqz
  T.LZ7NovDecs nd sqd -> U.Brim . U.BrimGrouped . U.BGFracuno
    . U.BG4 Nothing rdx . U.BG5Zeroes (toUZeroes zs1)
    . U.BG6Group r . U.BG7Zeroes (toUZeroes zs2)
    $ execBG8Zeroes sqz nd sqd
  T.LZ7Group r2 lz6 -> execLZ6Recurse rdx zs1 r zs2 sqz r2 lz6


execBG8Zeroes
  :: Seq (r, NE U.Zero U.Zero)
  -> NE Novem Decem
  -> Seq (r, NE Decem Decem)
  -> U.BG8 r
execBG8Zeroes sqz ned sqd = go sqz
  where
    go sq = case viewl sq of
      EmptyL -> U.BG8Novem ned sqd
      (r, nez) :< sq' -> U.BG8Group r . U.BG7Zeroes (toUZeroes nez)
        . go $ sq'
