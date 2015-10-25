{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
module Main where

import Control.Lens ((<|), over)
import Data.Derive.Arbitrary (makeArbitrary)
import Data.DeriveTH (derive)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Text (Text, pack)
import Data.Time (Day (ModifiedJulianDay), TimeOfDay (TimeOfDay))
import Rainbow.Types
  ( Color(Color), Radiant(Radiant)
  , Enum8 (E0, E1, E2, E3, E4, E5, E6, E7)
  )
import Test.Tasty.TH (testGroupGenerator)
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.QuickCheck (testProperty)

import Penny.Amount
import Penny.Arrangement
import Penny.Balance
import Penny.Clatch
import Penny.Colors
import Penny.Columns
import Penny.Decimal
import Penny.Digit
import Penny.Natural
import Penny.NonZero
import Penny.Polar
import Penny.Popularity
import Penny.Realm
import Penny.Representation
import Penny.Scalar
import Penny.SeqUtil
import Penny.Serial
import Penny.Shortcut
import Penny.Tree
import Penny.Troika

import Test.QuickCheck
  ( Arbitrary (arbitrary)
  , choose
  , scale
  )
import qualified Test.QuickCheck as Q

-- # Instances

-- ## Standard

instance Arbitrary a => Arbitrary (Seq.Seq a) where
  arbitrary = fmap Seq.fromList arbitrary

instance Arbitrary Text where
  arbitrary = fmap pack arbitrary

instance (Arbitrary k, Ord k, Arbitrary v) => Arbitrary (Map.Map k v) where
  arbitrary = fmap Map.fromList arbitrary

-- ## Numbers

instance Arbitrary Positive where
  arbitrary = do
    Q.Positive p <- arbitrary
    case integerToNatural p of
      Nothing -> error "could not generate Positive"
      Just r -> return r

instance Arbitrary Unsigned where
  arbitrary = do
    Q.NonNegative p <- arbitrary
    case integerToNatural p of
      Nothing -> error "could not generate Unsigned"
      Just r -> return r

instance Arbitrary NonZero where
  arbitrary = do
    Q.Positive x <- arbitrary
    b <- arbitrary
    let changeSign = if b then negate else id
        i = changeSign x
    case integerToNonZero i of
      Nothing -> error "could not generate NonZero"
      Just r -> return r

-- ## Polar

$( derive makeArbitrary ''Pole )
$( derive makeArbitrary ''Polarized )
$( derive makeArbitrary ''Moderated )

-- ## Serset

$( derive makeArbitrary ''Serset )
$( derive makeArbitrary ''Serpack )

-- ## Decimal

$( derive makeArbitrary ''Exponential )

-- ## Digit

$( derive makeArbitrary ''Zero )
$( derive makeArbitrary ''One )
$( derive makeArbitrary ''Two )
$( derive makeArbitrary ''Three )
$( derive makeArbitrary ''Four )
$( derive makeArbitrary ''Five )
$( derive makeArbitrary ''Six )
$( derive makeArbitrary ''Seven )
$( derive makeArbitrary ''Eight )
$( derive makeArbitrary ''Nine )
$( derive makeArbitrary ''D1z )
$( derive makeArbitrary ''D2z )
$( derive makeArbitrary ''D3z )
$( derive makeArbitrary ''D4z )
$( derive makeArbitrary ''D5z )
$( derive makeArbitrary ''D6z )
$( derive makeArbitrary ''D7z )
$( derive makeArbitrary ''D8z )
$( derive makeArbitrary ''D9z )
$( derive makeArbitrary ''D2 )
$( derive makeArbitrary ''D3 )
$( derive makeArbitrary ''D4 )
$( derive makeArbitrary ''D5 )
$( derive makeArbitrary ''D6 )
$( derive makeArbitrary ''D7 )
$( derive makeArbitrary ''D8 )
$( derive makeArbitrary ''D9 )

-- # Representation

$( derive makeArbitrary ''Radix )
$( derive makeArbitrary ''Grouper )
$( derive makeArbitrary ''RadCom )
$( derive makeArbitrary ''RadPer )
$( derive makeArbitrary ''NilGrouped )
$( derive makeArbitrary ''NilUngrouped )
$( derive makeArbitrary ''Nil )

$( derive makeArbitrary ''BrimUngrouped )
$( derive makeArbitrary ''BG1 )
$( derive makeArbitrary ''BG5 )
$( derive makeArbitrary ''BG6 )
$( derive makeArbitrary ''BG7 )
$( derive makeArbitrary ''BG8 )
$( derive makeArbitrary ''BrimGrouped )
$( derive makeArbitrary ''Brim )

-- ## Arrangement

$( derive makeArbitrary ''Orient )
$( derive makeArbitrary ''Arrangement )

-- ## Troika

$( derive makeArbitrary ''Troiload )
$( derive makeArbitrary ''Troika )

-- ## Scalar

$( derive makeArbitrary ''Day )
$( derive makeArbitrary ''TimeOfDay )
$( derive makeArbitrary ''Scalar )

-- ## Realm

$( derive makeArbitrary ''Realm )

-- ## Tree

instance Arbitrary Tree where
  arbitrary = do
    realm <- arbitrary
    scalar <- arbitrary
    children <- scale (`div` 10) arbitrary
    return $ Tree realm scalar children

-- ## Clatch

$( derive makeArbitrary ''Core )

-- ## Amount

$( derive makeArbitrary ''Amount )

-- ## Slice

$( derive makeArbitrary ''Slice )

-- ## Balance

$( derive makeArbitrary ''Balance )
$( derive makeArbitrary ''Imbalance )

-- ## Popularity

$( derive makeArbitrary ''History )

-- ## Radiant

$( derive makeArbitrary ''Enum8 )
$( derive makeArbitrary ''Color )
$( derive makeArbitrary ''Radiant )

-- ## Colors

$( derive makeArbitrary ''Colors )


-- ## Columns

$( derive makeArbitrary ''Env )

-- # Properties

-- account finds an account where it should.
prop_shortcutAccount
  :: Text
  -- ^ First sub-account name
  -> Seq Text
  -- ^ Following sub-account names
  -> Sliced ()
  -- ^ Posting
  -> Bool
prop_shortcutAccount a1 as pstg = account sliced == (a1 <| as)
  where
    sliced = over (posting . trees) adder pstg
    adder old = accountTree a1 as <| old

tests :: TestTree
tests = $( testGroupGenerator )

main :: IO ()
main = defaultMain tests
