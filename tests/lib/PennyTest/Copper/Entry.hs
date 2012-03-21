module PennyTest.Copper.Entry where

import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Entry as E
import qualified PennyTest.Copper.Amount as TA
import qualified PennyTest.Lincoln.Bits as TB
import qualified PennyTest.Lincoln.Meta as TM
import qualified PennyTest.Copper.Qty as TQ

import Control.Applicative ((<$>), (<*), (<*>))
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as P
import Test.Framework (Test, testGroup)
import Test.QuickCheck (Arbitrary, arbitrary, Gen)

-- | A renderable Entry. The commodities in the Amount are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
genREntry :: Gen B.Entry
genREntry = B.Entry <$> TB.genDrCr <*> TA.genRAmount

-- | A renderable Entry. The commodities in the Amount are distributed
-- evenly between Level 1, Level 2, and Level 3 commodities.
newtype REntry = REntry { unREntry :: B.Entry }
                 deriving (Show, Eq)
instance Arbitrary REntry where
  arbitrary = REntry <$> genREntry

-- | Parsing a rendered renderable should yield the same Entry.
prop_parseEntry ::
  TQ.AnySpecPair
  -> TQ.AnyRadGroup
  -> TM.Format
  -> REntry
  -> Bool
prop_parseEntry (TQ.AnySpecPair gs) (TQ.AnyRadGroup rg)
  (TM.Format f) (REntry e) =
  case E.render gs rg f e of
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
