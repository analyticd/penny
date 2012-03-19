module PennyTest.Copper.Memos.Transaction where

import Control.Applicative ((<$>), (<*))
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Memos.Transaction as T

import Test.QuickCheck (Gen, suchThat, arbitrary, listOf,
                        Arbitrary, arbitrary, sized, resize)
import PennyTest.Copper.Util (genTextNonEmpty)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as Parsec

-- | Generate renderable transaction memos.
genRMemo :: Gen B.Memo
genRMemo =
  B.Memo
  <$> (sized $ \s -> resize (min 5 s) (listOf genRMemoLine))

genRMemoLine :: Gen B.MemoLine
genRMemoLine = B.MemoLine <$> genTextNonEmpty p p where
  p = suchThat arbitrary T.isCommentChar

newtype RMemo = RMemo B.Memo deriving (Eq, Show)
instance Arbitrary RMemo where
  arbitrary = RMemo <$> genRMemo

-- | Parsing rendered Memo should yield same Memo.
prop_parseRendered :: RMemo -> Bool
prop_parseRendered (RMemo m) = case T.render m of
  Nothing -> False
  Just t ->
    case Parsec.parse (T.memo <* Parsec.eof) "" t of
      Left _ -> False
      Right (m', _) -> m == m'

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "parsing rendered Memo yields same Memo"

tests :: Test
tests = testGroup "Transaction"
        [ test_parseRendered ]
