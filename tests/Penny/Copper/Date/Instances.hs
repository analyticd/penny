{-# OPTIONS_GHC -fno-warn-orphans #-}
module Penny.Copper.Date.Instances where

import Control.Monad
import Control.Applicative
import Penny.Lincoln.Instances ()
import Penny.Copper.Date
import Test.QuickCheck
import Penny.Lincoln

instance Arbitrary DateSep where
  arbitrary = elements [Slash, Hyphen]

instance Arbitrary One where arbitrary = return One
instance Arbitrary Two where arbitrary = return Two
instance Arbitrary Three where arbitrary = return Three
instance Arbitrary Four where arbitrary = return Four
instance Arbitrary Five where arbitrary = return Five
instance Arbitrary Six where arbitrary = return Six
instance Arbitrary Seven where arbitrary = return Seven
instance Arbitrary Eight where arbitrary = return Eight
instance Arbitrary Nine where arbitrary = return Nine

instance Arbitrary Days28 where
  arbitrary = oneof
    [ D28'1to9 <$> arbitrary <*> arbitrary
    , D28'10to19 <$> arbitrary <*> arbitrary
    , D28'20to28 <$> arbitrary <*> arbitrary
    ]
  shrink (D28'1to9 _ D9'1) = []
  shrink _ = [D28'1to9 D0z'0 D9'1]

instance Arbitrary Days30 where
  arbitrary = oneof
    [ D30'28 <$> arbitrary
    , D30'29 <$> arbitrary <*> arbitrary
    , D30'30 <$> arbitrary <*> arbitrary
    ]
  shrink (D30'28 d28) = map D30'28 $ shrink d28
  shrink _ = [D30'28 (D28'1to9 D0z'0 D9'1)]

instance Arbitrary Days31 where
  arbitrary = oneof
    [ D31'30 <$> arbitrary
    , D31'31 <$> arbitrary <*> arbitrary
    ]
  shrink (D31'30 d30) = fmap D31'30 $ shrink d30
  shrink _ = [D31'30 (D30'28 (D28'1to9 D0z'0 D9'1))]

instance Arbitrary MonthDay where
  arbitrary = oneof
    [ Jan <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Feb <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Mar <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Apr <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , May <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Jun <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Jul <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Aug <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Sep <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Oct <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Nov <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    , Dec <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    ]

instance Arbitrary Year where
  arbitrary = liftM4 Year arbitrary arbitrary arbitrary arbitrary

instance Arbitrary NonLeapDay where
  arbitrary = liftM3 NonLeapDay arbitrary arbitrary arbitrary

instance Arbitrary Mod4 where
  arbitrary = elements
    [ L04 Zero Four
    , L08 Zero Eight
    , L12 One Two
    , L16 One Six
    , L20 Two Zero
    , L24 Two Four
    , L28 Two Eight
    , L32 Three Two
    , L36 Three Six
    , L40 Four Zero
    , L44 Four Four
    , L48 Four Eight
    , L52 Five Two
    , L56 Five Six
    , L60 Six Zero
    , L64 Six Four
    , L68 Six Eight
    , L72 Seven Two
    , L76 Seven Six
    , L80 Eight Zero
    , L84 Eight Four
    , L88 Eight Eight
    , L92 Nine Two
    , L96 Nine Six
    ]

instance Arbitrary CenturyLeapYear where
  arbitrary = liftM3 CenturyLeapYear arbitrary arbitrary arbitrary

instance Arbitrary NonCenturyLeapYear where
  arbitrary = liftM3 NonCenturyLeapYear arbitrary arbitrary arbitrary

instance Arbitrary LeapDay where
  arbitrary = LeapDay <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary DateA where
  arbitrary = DateA <$> arbitrary
