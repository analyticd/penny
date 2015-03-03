{-# OPTIONS_GHC -fno-warn-orphans #-}
module Penny.Lincoln.Instances where

import Control.Applicative
import Penny.Lincoln
import Control.Monad
import Test.QuickCheck hiding (Positive, NonZero)
import qualified Test.QuickCheck as Q
import qualified Data.Text as X
import qualified Data.Map as M
import Data.Monoid
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.DeriveTH

------------------------------------------------------------
-- From Other Packages
------------------------------------------------------------

instance Arbitrary a => Arbitrary (Seq a) where
  arbitrary = Seq.fromList <$> arbitrary

instance Arbitrary X.Text where arbitrary = X.pack <$> arbitrary

-- Amount

instance Arbitrary Amount where
  arbitrary = Amount <$> arbitrary <*> arbitrary

-- Balances
instance Arbitrary Balances where
  arbitrary = Balances <$> (M.fromList <$> arbitrary)

instance Arbitrary Imbalances where
  arbitrary = Imbalances <$> (M.fromList <$> arbitrary)

-- Commodity

instance Arbitrary Commodity where
  arbitrary = Commodity <$> (X.pack <$> arbitrary)

-- DateTime

instance Arbitrary Date where
  arbitrary = do
    y <- choose (1900, 2100)
    m <- choose (1, 12)
    d <- choose (1, 28)
    case fromGregorian y m d of
      Nothing -> fail "could not generate Date"
      Just r -> return r

$(derive makeArbitrary ''Time)
$(derive makeArbitrary ''DateTime)
$(derive makeArbitrary ''Hours)
$(derive makeArbitrary ''Minutes)
$(derive makeArbitrary ''Seconds)
$(derive makeArbitrary ''Zone)
$(derive makeArbitrary ''ZeroTo59)

-- Decimal

-- | Selects Decimal where the significand ranges over all of the
-- range of Int, but smaller significands are generated more than
-- large significands.  The exponent is always between 0 and 4.
instance Arbitrary Decimal where
  arbitrary = liftM2 Decimal genInt genExpt
    where
      genInt = fmap fI arbitrarySizedBoundedIntegral
      fI = fromIntegral :: Int -> Integer
      genExpt = sized $ \s -> do
        let maxExpt = min 4 s
        ex <- choose (0, fromIntegral maxExpt)
        case integerToNatural ex of
          Nothing -> fail "unable to generate Decimal"
          Just r -> return r

instance Arbitrary Semantic where
  arbitrary = Semantic <$> arbitrary

instance Arbitrary DecNonZero where
  arbitrary = oneof [large, anySize]
    where
      large = sized $ \s -> do
        nzi <- arbitrarySizedBoundedIntegral `suchThat` (/= (0 :: Int))
        dnz <- case integerToNonZero . fromIntegral $ nzi of
          Nothing -> fail "could not generate NonZero"
          Just r -> return r
        uni <- choose (0, s)
        expt <- case integerToNatural . fromIntegral $ uni of
          Nothing -> fail "could not generate Unsigned"
          Just r -> return r
        return $ DecNonZero dnz expt
      anySize = liftM2 DecNonZero arbitrary arbitrary

instance Arbitrary DecUnsigned where
  arbitrary = oneof [ large, anySize ]
    where
      anySize = liftM2 DecUnsigned arbitrary arbitrary
      large = sized $ \s -> do
        usi <- arbitrarySizedBoundedIntegral `suchThat` (>= (0 :: Int))
        usn <- case integerToNatural . fromIntegral $ usi of
          Nothing -> fail "could not generate Unsigned"
          Just r -> return r
        uni <- choose (0, s)
        expt <- case integerToNatural . fromIntegral $ uni of
          Nothing -> fail "could not generate exponent"
          Just i -> return i
        return $ DecUnsigned usn expt

instance Arbitrary DecPositive where
  arbitrary = oneof [ large, anySize ]
    where
      anySize = liftM2 DecPositive arbitrary arbitrary
      large = sized $ \s -> do
        usi <- arbitrarySizedBoundedIntegral `suchThat` (> (0 :: Int))
        usn <- case integerToNatural . fromIntegral $ usi of
          Nothing -> fail "could not generate Positive"
          Just r -> return r
        uni <- choose (0, s)
        expt <- case integerToNatural . fromIntegral $ uni of
          Nothing -> fail "could not generate exponent"
          Just i -> return i
        return $ DecPositive usn expt

instance Arbitrary DecZero where
  arbitrary = fmap DecZero arbitrary


-- Ent

instance Arbitrary a => Arbitrary (Ent a) where
  arbitrary = Ent <$> arbitrary <*> arbitrary

-- Ents
instance Arbitrary a => Arbitrary (Balanced a) where
  arbitrary = do
    cy <- arbitrary
    s <- arbitrary
    sq <- arbitrary
    mt <- arbitrary
    return $ fmap fst $ restrictedBalanced cy s sq mt

instance Arbitrary a => Arbitrary (View a) where
  arbitrary = sized go
    where
      go sz = do
        bal <- arbitrary
        let vs = allViews bal
        if Seq.length vs == 0
          then resize (sz + 1) arbitrary
          else do
            i <- choose (0, Seq.length vs - 1)
            return $ vs `Seq.index` i

instance Arbitrary a => Arbitrary (Ents a) where
  arbitrary = sized $ \s -> do
    len <- choose (0, s)
    go len
    where
      go sz
        | sz == 0 = return mempty
        | otherwise = do
            ent <- arbitrary
            rest <- go (sz - 1)
            return $ prependEnt ent rest

$(derive makeArbitrary ''ImbalancedError)

-- Exch
$(derive makeArbitrary ''Exch)

-- Field
$(derive makeArbitrary ''Scalar)
$(derive makeArbitrary ''Realm)

instance Arbitrary Tree where
  arbitrary = sized go
    where
      go sz = do
        sc <- arbitrary
        rlm <- arbitrary
        cs <- resize (sz `div` 2) arbitrary
        return $ Tree rlm sc cs

-- Natural
instance Arbitrary Positive where
  arbitrary = do
    Q.Positive i <- arbitrary
    case integerToNatural i of
      Nothing -> fail "could not create Positive."
      Just r -> return r

instance Arbitrary Unsigned where
  arbitrary = do
    Q.NonNegative i <- arbitrary
    case integerToNatural i of
      Nothing -> fail "could not generate Unsigned."
      Just r -> return r

-- NonZero
instance Arbitrary NonZero where
  arbitrary = do
    Q.NonZero i <- arbitrary
    case integerToNonZero i of
      Nothing -> fail "could not generate NonZero."
      Just r -> return r

-- PluMin
$(derive makeArbitrary ''PluMin)

-- Prices
$(derive makeArbitrary ''FromCy)
$(derive makeArbitrary ''ToCy)

instance Arbitrary FromTo where
  arbitrary = sized go
    where
      go sz = do
        fr <- arbitrary
        to <- arbitrary
        case fromTo fr to of
          Nothing -> go (sz + 1)
          Just r -> return r

$(derive makeArbitrary ''Price)

instance Arbitrary PriceDb where
  arbitrary = sized $ \s -> do
    s' <- choose (0, s)
    go s'
    where
      go sz
        | sz == 0 = return emptyDb
        | otherwise = do
            pr <- arbitrary
            rest <- go (sz - 1)
            return (addPriceToDb rest pr)

$(derive makeArbitrary ''ExchLookupError)

-- Qty
instance Arbitrary Qty where
  arbitrary = fmap Qty arbitrary

instance Arbitrary QtyNonZero where
  arbitrary = fmap QtyNonZero arbitrary

instance Arbitrary QtyUnsigned where
  arbitrary = fmap QtyUnsigned arbitrary

-- Rep
instance Arbitrary (Radix a) where
  arbitrary = return Radix

$(derive makeArbitrary ''Grouper)

instance Arbitrary RadCom where
  arbitrary = oneof [ return Period, RCGrouper <$> arbitrary ]

instance Arbitrary RadPer where
  arbitrary = oneof [ return Comma, RPGrouper <$> arbitrary ]

instance Arbitrary Zero where arbitrary = return Zero

instance Arbitrary r => Arbitrary (Nil r) where
  arbitrary = oneof [ NilU <$> arbitrary, NilG <$> arbitrary ]

instance Arbitrary r => Arbitrary (NilGrouped r) where
  arbitrary = NilGrouped <$> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary <*> arbitrary <*> arbitrary
    <*> arbitrary
    <*> arbitrary

$(derive makeArbitrary ''NilUngrouped )
$(derive makeArbitrary ''Brim)
$(derive makeArbitrary ''BrimGrouped)
$(derive makeArbitrary ''BrimUngrouped)
$(derive makeArbitrary ''BG1)
$(derive makeArbitrary ''BG5)
$(derive makeArbitrary ''BG6)
$(derive makeArbitrary ''BG7)
$(derive makeArbitrary ''BG8)
$(derive makeArbitrary ''CenterOrOffCenter)
$(derive makeArbitrary ''NilOrBrimPolar)
$(derive makeArbitrary ''NilOrBrimScalar)
$(derive makeArbitrary ''NilOrBrimScalarAnyRadix)
$(derive makeArbitrary ''RepNonNeutralNoSide)
$(derive makeArbitrary ''QtyRep)
$(derive makeArbitrary ''QtyRepAnyRadix)
$(derive makeArbitrary ''ExchRep)
$(derive makeArbitrary ''ExchRepAnyRadix)

-- Rep.Digits
$(derive makeArbitrary ''D1z)
$(derive makeArbitrary ''D2)
$(derive makeArbitrary ''D2z)
$(derive makeArbitrary ''D3)
$(derive makeArbitrary ''D3z)
$(derive makeArbitrary ''D4)
$(derive makeArbitrary ''D4z)
$(derive makeArbitrary ''D5)
$(derive makeArbitrary ''D5z)
$(derive makeArbitrary ''D6)
$(derive makeArbitrary ''D6z)
$(derive makeArbitrary ''D7)
$(derive makeArbitrary ''D7z)
$(derive makeArbitrary ''D8)
$(derive makeArbitrary ''D8z)
$(derive makeArbitrary ''D9)
$(derive makeArbitrary ''D9z)

-- Side
$(derive makeArbitrary ''Side)

-- Transaction
$(derive makeArbitrary ''TopLine)
$(derive makeArbitrary ''PstgMeta)
$(derive makeArbitrary ''Transaction)
$(derive makeArbitrary ''TransactionError)
$(derive makeArbitrary ''Bundle)

-- Trio
$(derive makeArbitrary ''Orient)
$(derive makeArbitrary ''SpaceBetween)
$(derive makeArbitrary ''Arrangement)
$(derive makeArbitrary ''Trio)
$(derive makeArbitrary ''TrioError)
