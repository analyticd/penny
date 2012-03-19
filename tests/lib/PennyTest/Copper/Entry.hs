module PennyTest.Copper.Entry where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Entry as E
import qualified Penny.Copper.Qty as Q
import qualified Penny.Lincoln.Meta as M
import qualified PennyTest.Copper.Amount as TA
-- Import orphan instances of Arbitrary
import PennyTest.Lincoln.Bits ()
import PennyTest.Lincoln.Meta ()
import PennyTest.Copper.Qty ()

import Control.Applicative ((<$>), (<*), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)

-- | A renderable Entry. The commodities in the Amount are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
genREntry :: Gen B.Entry
genREntry = B.Entry <$> arbitrary <*> TA.genRAmount

-- | A renderable Entry. The commodities in the Amount are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
newtype REntry = REntry { unREntry :: B.Entry }
                 deriving (Show, Eq)
instance Arbitrary REntry where
  arbitrary = REntry <$> genREntry

-- | Parsing a rendered renderable should yield the same Entry.
prop_parseEntry ::
  Q.GroupingSpec -- ^ Grouping to the left of radix point
  -> Q.GroupingSpec -- ^ Grouping to the right of radix point
  -> Q.RadGroup
  -> M.Format
  -> REntry
  -> Bool
prop_parseEntry gl gr rg f (REntry e) =
  case E.render gl gr rg f e of
    Nothing -> False
    Just txt -> case P.parse (E.entry rg <* P.eof) "" txt of
      Left _ -> False
      Right (e', f') -> e' == e && f' == f

test_parseEntry :: Test
test_parseEntry = testProperty s prop_parseEntry where
  s = "Parsing renderable Entry should yield same Entry"

tests :: Test
tests = testGroup "Entry"
        [ test_parseEntry ]
