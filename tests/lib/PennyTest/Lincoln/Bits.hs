module PennyTest.Lincoln.Bits where

import Control.Applicative ((<$>), (<*>), pure)
import Data.Decimal ()
import Data.Maybe (fromJust)
import qualified Data.Time as DT
import qualified Penny.Lincoln.Bits as B
import PennyTest.Lincoln.TextNonEmpty ()
import qualified Test.QuickCheck as Q
import Test.QuickCheck (arbitrary, Arbitrary)

instance Q.Arbitrary B.SubAccountName where
  arbitrary = B.SubAccountName <$> arbitrary

instance Q.Arbitrary B.Account where
  arbitrary = B.Account <$> arbitrary

instance Q.Arbitrary B.Amount where
  arbitrary = B.Amount <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.SubCommodity where
  arbitrary = B.SubCommodity <$> arbitrary

instance Q.Arbitrary B.Commodity where
  arbitrary = B.Commodity <$> arbitrary


instance Q.Arbitrary DT.Day where
  arbitrary = DT.ModifiedJulianDay <$> (Q.suchThat arbitrary (>= 0))

instance Q.Arbitrary DT.DiffTime where
  arbitrary = DT.secondsToDiffTime
              <$> (Q.suchThat arbitrary (\s -> s >= 0 && s < 86400))

instance Q.Arbitrary DT.UTCTime where
  arbitrary = DT.UTCTime <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.DateTime where
  arbitrary = B.DateTime <$> arbitrary

instance Arbitrary B.DrCr where
  arbitrary = Q.oneof [pure B.Debit, pure B.Credit]

instance Arbitrary B.Entry where
  arbitrary = B.Entry <$> arbitrary <*> arbitrary

instance Arbitrary B.Flag where
  arbitrary = B.Flag <$> arbitrary

instance Arbitrary B.Memo where
  arbitrary = B.Memo <$> arbitrary

instance Arbitrary B.Number where
  arbitrary = B.Number <$> arbitrary

instance Arbitrary B.Payee where
  arbitrary = B.Payee <$> arbitrary

instance Arbitrary B.From where
  arbitrary = B.From <$> arbitrary

instance Arbitrary B.To where
  arbitrary = B.To <$> arbitrary

instance Arbitrary B.CountPerUnit where
  arbitrary = B.CountPerUnit <$> arbitrary

instance Arbitrary B.Price where
  arbitrary = mkPrice <$> Q.suchThat g p where
    g = (,,) <$> arbitrary <*> arbitrary <*> arbitrary
    p (f, t, c) = case B.newPrice f t c of
      Nothing -> False
      Just _ -> True
    mkPrice (f, t, c) = fromJust $ B.newPrice f t c

instance Arbitrary B.PricePoint where
  arbitrary = B.PricePoint <$> arbitrary <*> arbitrary

instance Q.Arbitrary B.Qty where
  arbitrary = let
    p d = case B.newQty d of
      Nothing -> False
      Just _ -> True
    in B.partialNewQty <$> (Q.suchThat arbitrary p)
                    
instance Arbitrary B.Tag where
  arbitrary = B.Tag <$> arbitrary

instance Arbitrary B.Tags where
  arbitrary = B.Tags <$> arbitrary
