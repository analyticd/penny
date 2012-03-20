module PennyTest.Copper.Commodity where

import qualified Penny.Copper.Commodity as C
import qualified Penny.Lincoln.Bits as B
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Copper.Util (wrapTextNonEmptyList)

import Control.Applicative ((<$>), (<*))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, suchThat, Gen, oneof)

-- | Generates one of any of the renderable commodities (levels 1, 2,
-- or 3).
genRCmdty :: Gen B.Commodity
genRCmdty = oneof [genLvl1Cmdty, genLvl2Cmdty, genLvl3Cmdty]

-- | Randomly picks one of any of the renderable commodities (levels
-- 1, 2, or 3).
newtype RCmdty = RCmdty { unRCmdty :: B.Commodity }
                 deriving (Show, Eq)

instance Arbitrary RCmdty where
  arbitrary = RCmdty <$> genRCmdty

-- | Level 1 commodities.
newtype Lvl1Cmdty =
  Lvl1Cmdty { unRCommodity :: B.Commodity }
  deriving (Show, Eq)

genLvl1Cmdty :: Gen B.Commodity
genLvl1Cmdty = wrapTextNonEmptyList (min 3)
               (suchThat arbitrary C.lvl1Char)
               (suchThat arbitrary C.lvl1Char) B.SubCommodity
               B.Commodity

instance Arbitrary Lvl1Cmdty where
  arbitrary = Lvl1Cmdty <$> genLvl1Cmdty

-- | Parsing a rendered Commodity should yield the same thing.
prop_parseRenderable :: Lvl1Cmdty -> Bool
prop_parseRenderable (Lvl1Cmdty c) =
  case C.renderQuotedLvl1 c of
    Nothing -> False
    Just t -> case P.parse (C.quotedLvl1Cmdty <* P.eof) "" t of
      Left _ -> False
      Right c' -> c == c'

test_parseRenderable :: Test
test_parseRenderable = testProperty s prop_parseRenderable where
  s = "Parsing a renderable Commodity should yield same thing"

newtype Lvl2Cmdty =
  Lvl2Cmdty { unLvl2Cmdty :: B.Commodity }
  deriving (Eq, Show)

genLvl2Cmdty :: Gen B.Commodity
genLvl2Cmdty = wrapTextNonEmptyList (min 3)
               (suchThat arbitrary C.lvl2FirstChar)
               (suchThat arbitrary C.lvl2OtherChars) B.SubCommodity
               B.Commodity
 

instance Arbitrary Lvl2Cmdty where
  arbitrary = Lvl2Cmdty <$> genLvl2Cmdty
              
-- | Parsing a rendered Lvl 2 commodity should yield same thing.
prop_parseLvl2 :: Lvl2Cmdty -> Bool
prop_parseLvl2 (Lvl2Cmdty c) =
  case C.renderLvl2 c of
    Nothing -> False
    Just t -> case P.parse (C.lvl2Cmdty <* P.eof) "" t of
      Left _ -> False
      Right c' -> c == c'

test_parseLvl2 :: Test
test_parseLvl2 = testProperty s prop_parseLvl2 where
  s = "Parsing a level 2 Commodity should yield same thing"

newtype Lvl3Cmdty =
  Lvl3Cmdty { unLvl3Cmdty :: B.Commodity }
  deriving (Eq, Show)

genLvl3Cmdty :: Gen B.Commodity
genLvl3Cmdty =
  wrapTextNonEmptyList (min 3) f r B.SubCommodity B.Commodity where
    f = suchThat arbitrary C.lvl3FirstChar
    r = suchThat arbitrary C.lvl3OtherChars

instance Arbitrary Lvl3Cmdty where
  arbitrary = Lvl3Cmdty <$> genLvl3Cmdty


-- | Parsing a rendered Lvl 2 commodity should yield same thing.
prop_parseLvl3 :: Lvl3Cmdty -> Bool
prop_parseLvl3 (Lvl3Cmdty c) =
  case C.renderLvl3 c of
    Nothing -> False
    Just t -> case P.parse (C.lvl3Cmdty <* P.eof) "" t of
      Left _ -> False
      Right c' -> c == c'

test_parseLvl3 :: Test
test_parseLvl3 = testProperty s prop_parseLvl3 where
  s = "Parsing a level 3 Commodity should yield same thing"


tests :: Test
tests = testGroup "Commodity"
        [ test_parseRenderable
        , test_parseLvl2
        , test_parseLvl3 ]
