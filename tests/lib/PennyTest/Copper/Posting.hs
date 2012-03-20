module PennyTest.Copper.Posting where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper.Posting as P
import qualified Penny.Copper.Qty as Qt

import Test.QuickCheck (Gen, Arbitrary, arbitrary, oneof)
import Test.Framework (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import qualified Text.Parsec as Parsec
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

-- | Generate renderable unverified Postings with metadata. The
-- Accounts are variably Level 1 and Level 2. The Commodities in the
-- Amount are distributed between Levels 1, 2, and 3.
genRUPosting :: Gen P.UnverifiedWithMeta
genRUPosting =
  P.UnverifiedWithMeta
  <$> genMaybe TFl.genRFlag
  <*> genMaybe TNu.genRNumber
  <*> genMaybe (oneof [TPa.genNoQuotePayee, TPa.genNeedsQuotePayee])
  <*> oneof [TAc.genLvl1Account, TAc.genLvl2Account]
  <*> TTa.genRTags
  <*> genMaybe ((,) <$> TEn.genREntry <*> arbitrary)
  <*> TMe.genRMemo

newtype RUPosting = RUPosting P.UnverifiedWithMeta
                    deriving (Eq, Show)
instance Arbitrary RUPosting where
  arbitrary = RUPosting <$> genRUPosting

-- | Parsing rendered Posting yields the same thing.
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

tests :: Test
tests = testGroup "Posting"
        [ test_parseRendered ]
