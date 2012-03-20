module PennyTest.Copper.Posting where

import Control.Applicative ((<$>), (<*>), (<*), (*>))
import Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper.Posting as P
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Transaction.Unverified as U

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

-- | Generate renderable unverified Postings. The Accounts are
-- variably Level 1 and Level 2. The Commodities in the Amount are
-- distributed between Levels 1, 2, and 3.
genRUPosting :: Gen U.Posting
genRUPosting =
  U.Posting
  <$> genMaybe (oneof [TPa.genNoQuotePayee, TPa.genNeedsQuotePayee])
  <*> genMaybe TNu.genRNumber
  <*> genMaybe TFl.genRFlag
  <*> oneof [TAc.genLvl1Account, TAc.genLvl2Account]
  <*> TTa.genRTags
  <*> genMaybe TEn.genREntry
  <*> TMe.genRMemo

newtype RUPosting = RUPosting U.Posting
                    deriving (Eq, Show)
instance Arbitrary RUPosting where
  arbitrary = RUPosting <$> genRUPosting

-- | Parsing rendered Posting yields the same thing.
prop_parseRendered ::
  (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> M.Format
  -> RUPosting
  -> Bool
prop_parseRendered (gl, gr) rg fmt (RUPosting p) =
  Ex.switch error id $ do
    txt <- case P.render p (gl, gr) rg fmt of
      Nothing -> Ex.throw "Failed to render"
      Just x -> return x
    let parser = Parsec.many (Parsec.char ' ')
               *> P.posting rg
               <* Parsec.eof
    (p', pm) <- case Parsec.parse parser "" txt of
        Left e -> Ex.throw $ "parse error: " ++ show e
                  ++ " rendered: " ++ show txt
        Right pair -> return pair
    _ <- case M.postingFormat pm of
      Nothing -> case U.entry p of
        Just _ -> Ex.throw "no metadata"
        Nothing -> return ()
      Just fmt' ->
        if fmt /= fmt'
        then Ex.throw "formats not equal"
        else return ()
    if p /= p'
      then Ex.throw "postings not equal"
      else return True

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered Posting yields the same thing"

tests :: Test
tests = testGroup "Posting"
        [ test_parseRendered ]
