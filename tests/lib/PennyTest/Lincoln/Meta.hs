module PennyTest.Lincoln.Meta where

import qualified Penny.Lincoln.Meta as M

import Control.Applicative (pure, (<$>), (<*>))
import Data.Text (pack)
import Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary)

instance Arbitrary M.Side where
  arbitrary = oneof [ pure M.CommodityOnLeft
                    , pure M.CommodityOnRight ]

instance Arbitrary M.SpaceBetween where
  arbitrary = oneof [ pure M.SpaceBetween
                    , pure M.NoSpaceBetween ]

instance Arbitrary M.Format where
  arbitrary = M.Format <$> arbitrary <*> arbitrary

instance Arbitrary M.Line where
  arbitrary = M.Line <$> arbitrary

instance Arbitrary M.Filename where
  arbitrary = M.Filename <$> (pack <$> arbitrary)

instance Arbitrary M.Column where
  arbitrary = M.Column <$> arbitrary

instance Arbitrary M.PriceLine where
  arbitrary = M.PriceLine <$> arbitrary

instance Arbitrary M.PostingLine where
  arbitrary = M.PostingLine <$> arbitrary

instance Arbitrary M.TopMemoLine where
  arbitrary = M.TopMemoLine <$> arbitrary

instance Arbitrary M.TopLineLine where
  arbitrary = M.TopLineLine <$> arbitrary

instance Arbitrary M.PriceMeta where
  arbitrary = M.PriceMeta <$> arbitrary <*> arbitrary

instance Arbitrary M.PostingMeta where
  arbitrary = M.PostingMeta <$> arbitrary <*> arbitrary

instance Arbitrary M.TopLineMeta where
  arbitrary = M.TopLineMeta <$> arbitrary <*> arbitrary <*> arbitrary
