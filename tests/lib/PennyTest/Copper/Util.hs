module PennyTest.Copper.Util where

import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))

import qualified Penny.Copper.Commodity as C
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.TextNonEmpty as TNE
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()

import Control.Applicative ((<$>), (<*>), (<$), (<*),
                            (*>))
import qualified Data.Text as X
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, elements, suchThat,
                        listOf, Gen)


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
