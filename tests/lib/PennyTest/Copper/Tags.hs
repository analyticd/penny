module PennyTest.Copper.Tags where

import Control.Applicative ((<$>), (<*))
import qualified Test.QuickCheck as QC
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P

import qualified Penny.Copper.Tags as T
import qualified Penny.Lincoln.Bits as B
import PennyTest.Copper.Util (genTextNonEmpty)

-- | Generates a renderable Tag.
genRTag :: QC.Gen B.Tag
genRTag = B.Tag <$> genTextNonEmpty p p where
  p = QC.suchThat QC.arbitrary T.isTagChar

genRTags :: QC.Gen B.Tags
genRTags = B.Tags <$> QC.listOf genRTag

newtype RTags = RTags B.Tags deriving (Show, Eq)
instance QC.Arbitrary RTags where
  arbitrary = RTags <$> genRTags

-- | Parsing rendered Tags should yield same thing.
prop_parseRendered :: RTags -> Bool
prop_parseRendered (RTags ts) = case T.render ts of
  Nothing -> False
  Just txt -> case P.parse (T.tags <* P.eof) "" txt of
    Left _ -> False
    Right ts' -> ts == ts'

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered Tags should yield same Tags"

tests :: Test
tests = testGroup "Tags"
        [ test_parseRendered ]
