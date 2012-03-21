module PennyTest.Copper.Posting where

import Control.Applicative ((<$>), (<*>), pure)

import Test.QuickCheck (Gen, Arbitrary, arbitrary, oneof)
import Test.Framework (Test, testGroup)
import qualified PennyTest.Copper.Account as TAc
import qualified PennyTest.Copper.Entry as TEn
import qualified PennyTest.Copper.Flag as TFl
import qualified PennyTest.Copper.Payees as TPa
import qualified PennyTest.Copper.Memos.Posting as TMe
import qualified PennyTest.Copper.Number as TNu
import PennyTest.Copper.Qty ()
import PennyTest.Lincoln.Meta ()
import qualified PennyTest.Copper.Tags as TTa
import PennyTest.Copper.Util (genMaybe)
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Meta as M

-- | Generate renderable unverified Postings with metadata. The
-- Accounts are variably Level 1 and Level 2. The Commodities in the
-- Amount are distributed between Levels 1, 2, and 3.
genRUPosting :: Gen (U.Posting, M.PostingMeta)
genRUPosting = do
  fl <- genMaybe TFl.genRFlag
  nu <- genMaybe TNu.genRNumber
  pa <- genMaybe (oneof [TPa.genNoQuotePayee, TPa.genNeedsQuotePayee])
  ac <- oneof [TAc.genLvl1Account, TAc.genLvl2Account]
  ta <- TTa.genRTags
  en <- genMaybe TEn.genREntry
  me <- TMe.genRMemo
  meta <- case en of
    Nothing -> M.PostingMeta <$> arbitrary <*> pure Nothing
    Just _ -> M.PostingMeta <$> arbitrary <*> arbitrary
  let uPo = U.Posting pa nu fl ac ta en me
  return (uPo, meta)

newtype RUPosting = RUPosting (U.Posting, M.PostingMeta)
                    deriving (Eq, Show)
instance Arbitrary RUPosting where
  arbitrary = RUPosting <$> genRUPosting

-- | Parsing rendered Posting yields the same thing.
{-
prop_parseRendered ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> RUPosting
  -> Bool
prop_parseRendered gs rg (RUPosting p) =
  Ex.switch error id $ do
    txt <- case P.render gs rg p of
      Nothing -> Ex.throw "Failed to render"
      Just x -> return x
    let parser = Parsec.many (Parsec.char ' ')
               *> P.posting rg
               <* Parsec.eof
    pAndMeta <- case Parsec.parse parser "" txt of
        Left e -> Ex.throw $ "parse error: " ++ show e
                  ++ " rendered: " ++ show txt
        Right pair -> return pair
    p' <- case P.unverifiedWithMeta pAndMeta of
      Nothing -> Ex.throw "unverifiedWithMeta failed"
      Just r -> return r
    if p /= p'
      then Ex.throw "postings not equal"
      else return True

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered Posting yields the same thing"
-}
tests :: Test
tests = testGroup "Posting" []
--        [ test_parseRendered ]
