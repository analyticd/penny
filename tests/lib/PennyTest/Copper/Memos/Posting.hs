module PennyTest.Copper.Memos.Posting where

import Control.Applicative ((<$>))
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Copper.Memos.Posting as P

import Test.QuickCheck (Gen, suchThat, arbitrary, listOf,
                        Arbitrary, arbitrary)
import PennyTest.Copper.Util (genTextNonEmpty)
import Test.Framework (Test, testGroup)
import qualified Text.Parsec as Parsec

-- | Generate renderable memos.
genRMemo :: Gen B.Memo
genRMemo = B.Memo <$> listOf genRMemoLine

genRMemoLine :: Gen B.MemoLine
genRMemoLine = B.MemoLine <$> genTextNonEmpty p p where
  p = suchThat arbitrary (P.isCommentChar)

newtype RMemo = RMemo B.Memo deriving (Eq, Show)
instance Arbitrary RMemo where
  arbitrary = RMemo <$> genRMemo

-- | Parsing rendered Memo should yield same Memo.
prop_parseRendered :: RMemo -> Bool
prop_parseRendered (RMemo m) 
