{-# LANGUAGE DeriveGeneric, CPP, GeneralizedNewtypeDeriving #-}

module Penny.Lincoln.Bits.Price (
    From ( From, unFrom )
  , To ( To, unTo )
  , CountPerUnit ( CountPerUnit, unCountPerUnit )
  , Price ( from, to, countPerUnit )
  , newPrice

#ifdef test
  , tests
#endif
  ) where

import Data.Monoid (mconcat)
import qualified Penny.Lincoln.Equivalent as Ev
import Penny.Lincoln.Equivalent ((==~))
import qualified Penny.Lincoln.Bits.Open as O
import Penny.Lincoln.Bits.Qty (Qty)
import GHC.Generics (Generic)
import qualified Data.Binary as B

#ifdef test
import Data.Maybe (isJust, isNothing)
import Test.QuickCheck (Arbitrary, arbitrary)
import qualified Test.QuickCheck as Q
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
#endif

newtype From = From { unFrom :: O.Commodity }
  deriving (Eq, Ord, Show, Generic
#ifdef test
  , Arbitrary)
#else
  )
#endif

instance B.Binary From

newtype To = To { unTo :: O.Commodity }
  deriving (Eq, Ord, Show, Generic
#ifdef test
  , Arbitrary
#endif
  )

instance B.Binary To

newtype CountPerUnit = CountPerUnit { unCountPerUnit :: Qty }
  deriving (Eq, Ord, Show, Generic
#ifdef test
  , Arbitrary
#endif
  )

instance Ev.Equivalent CountPerUnit where
  equivalent (CountPerUnit x) (CountPerUnit y) = x ==~ y
  compareEv (CountPerUnit x) (CountPerUnit y) = Ev.compareEv x y

instance B.Binary CountPerUnit

data Price = Price { from :: From
                   , to :: To
                   , countPerUnit :: CountPerUnit }
             deriving (Eq, Ord, Show, Generic)

#ifdef test
instance Arbitrary Price where
  arbitrary = do
    (f, t) <- Q.suchThat arbitrary (\(f, t) -> unFrom f /= unTo t)
    c <- arbitrary
    return $ Price f t c

-- | All Prices have from and to commodities that are different.
prop_price :: Price -> Bool
prop_price (Price f t _) = unFrom f /= unTo t

#endif

instance B.Binary Price

-- | Two Price are equivalent if the From and To are equal and the
-- CountPerUnit is equivalent.

instance Ev.Equivalent Price where
  equivalent (Price xf xt xc) (Price yf yt yc) =
    xf == yf && xt == yt && xc ==~ yc

  compareEv (Price xf xt xc) (Price yf yt yc) = mconcat
    [ compare xf yf
    , compare xt yt
    , Ev.compareEv xc yc
    ]

-- | Succeeds only if From and To are different commodities.
newPrice :: From -> To -> CountPerUnit -> Maybe Price
newPrice f t cpu =
  if unFrom f == unTo t
  then Nothing
  else Just $ Price f t cpu

#ifdef test

-- | newPrice succeeds if From and To are different
prop_newPriceDifferent :: CountPerUnit -> Q.Property
prop_newPriceDifferent cpu =
  Q.forAll (Q.suchThat arbitrary (\(From f, To t) -> f /= t)) $
  \(f, t) -> isJust (newPrice f t cpu)

-- | newPrice fails if From and To are the same
prop_newPriceSame :: From -> CountPerUnit -> Bool
prop_newPriceSame (From fr) cpu =
  isNothing (newPrice (From fr) (To fr) cpu)

tests :: Test
tests = testGroup "Penny.Lincoln.Bits.Price"
  [ testProperty "prop_price" prop_price
  , testProperty "prop_newPriceDifferent" prop_newPriceDifferent
  , testProperty "prop_newPriceSame" prop_newPriceSame
  ]
#endif
