{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Penny.Lincoln.Properties where

import Penny.Lincoln
import Penny.Lincoln.Instances ()
import Test.Tasty.QuickCheck
import qualified Data.Sequence as Seq
import Test.Tasty

-- These tests are organized so that modules at the bottom of the
-- import graph are tested first.

tests = testGroup "Penny.Lincoln.Properties"
  [ testGroupNatural
  ]

testGroupNatural :: TestTree
testGroupNatural = testGroup "Penny.Lincoln.Natural"
  [
  ]

-- Natural

prop_novDecsToPositive'positiveDigits d1 ds
  = let pos = novDecsToPositive d1 (Seq.fromList ds)
        (d1', ds') = positiveDigits pos
    in (d1', ds') === (d1, ds)

prop_repUngroupedDecimal dec
  = let rep = repUngroupedDecimal (Radix :: Radix ()) dec
        res = toDecimal rep
  in counterexample ("rep: " ++ show rep ++ " res: " ++ show res)
     $ res == dec

prop_repUngroupedDecNonZero dec
  = pos === dec
  where
    pos = c'DecNonZero'DecPositive pm . toDecPositive $ bu
    (bu, pm) = repUngroupedDecNonZero (Radix :: Radix ()) dec

prop_repUngroupedDecUnsigned dec
  = case (decomposeDecUnsigned dec, rep) of
    (Left z, Center nu) -> toDecZero nu === z
    (Right p, OffCenter bu ()) -> toDecPositive bu === p
    _ -> property False
  where
    rep = repUngroupedDecUnsigned (Radix :: Radix ()) dec

seconds :: Testable prop => Int -> prop -> Property
seconds i = within (i * 10 ^ (6 :: Int))

prop_repUngroupedDecZeroSucceeds dz
  = length (show (repUngroupedDecZero (Radix :: Radix ()) dz)) /= 0

prop_repUngroupedDecZero dec
  = seconds 10
  $ toDecZero (repUngroupedDecZero (Radix :: Radix ()) dec)
  === dec

-- Grouping

prop_groupBrimUngrouped'ungroup bu
  = case groupBrimUngrouped () bu of
      Nothing -> property Discard
      Just a -> property $ ungroupBrimGrouped a == bu

