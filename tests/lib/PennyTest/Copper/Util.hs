module PennyTest.Copper.Util where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Penny.Lincoln.TextNonEmpty as TNE
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()

import Control.Applicative ((<$>), (<*>), pure)
import qualified Data.Text as X
import Test.QuickCheck (listOf, Gen, oneof)


wrapTextNonEmptyList ::
  Gen Char
  -- ^ Generates the first character of the list

  -> Gen Char
  -- ^ Generates every other character in the list

  -> (TNE.TextNonEmpty -> a)
  -- ^ Wraps the inner items

  -> (NE.NonEmpty a -> b)
  -- ^ Wraps the outer items

  -> Gen b
wrapTextNonEmptyList gf gr wi wo = let
  firstWord =
    wi <$> (TNE.TextNonEmpty <$> gf <*> (X.pack <$> listOf gr))
  otherWord =
    wi <$> (TNE.TextNonEmpty <$> gr <*> (X.pack <$> listOf gr))
  restWords = (listOf otherWord)
  in wo <$> ( (:|) <$> firstWord <*> restWords)

-- | Generate a TextNonEmpty with given generators for the first
-- character and for the rest of the characters.
genTextNonEmpty :: Gen Char -> Gen Char -> Gen TNE.TextNonEmpty
genTextNonEmpty gf gr = TNE.TextNonEmpty
                        <$> gf
                        <*> (X.pack <$> listOf gr)

-- | Turns a generator into a generator of maybes.
genMaybe :: Gen a -> Gen (Maybe a)
genMaybe g = oneof [pure Nothing, Just <$> g]

