module PennyTest.Copper.Transaction where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ((<$>), (<*>), pure, (<*))
import Data.Foldable (toList)
import Data.Traversable (traverse)
import PennyTest.Copper.Util (genMaybe)
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Copper.Transaction as T
import qualified PennyTest.Copper.Account as TAc
import qualified PennyTest.Copper.Commodity as TCy
import PennyTest.Copper.DateTime ()
import qualified PennyTest.Copper.Flag as TFl
import qualified PennyTest.Copper.Payees as TPa
import qualified PennyTest.Copper.Memos.Posting as TPostingMemo
import qualified PennyTest.Copper.Memos.Transaction as TTransactionMemo
import qualified PennyTest.Copper.Number as TNu
import qualified PennyTest.Copper.Tags as TTa
import qualified PennyTest.Lincoln.Bits as TB
import PennyTest.Lincoln.Meta ()
import PennyTest.Lincoln.Family.Family ()
import qualified PennyTest.Lincoln.Transaction as TLT
import Test.QuickCheck (
  Arbitrary, arbitrary, Gen, oneof, sized, resize, Property,
  property)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)
import qualified Text.Parsec as Parsec
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Transaction as Txn
import qualified Penny.Lincoln.Transaction.Unverified as U
import Penny.Lincoln.Family (orphans, adopt, marry)
import qualified Penny.Lincoln.Meta as M
import qualified Penny.Lincoln.Family.Family as Fam

-- | The data needed to generate a single renderable unverified
-- transaction.
rTransInputs :: TLT.TransInputs
rTransInputs =
  TLT.TransInputs {
    TLT.dateTime = arbitrary
    , TLT.tFlag = genMaybe TFl.genRFlag
    , TLT.tNumber = genMaybe TNu.genRNumber
    , TLT.tPayee =
      genMaybe $ oneof [TPa.genNoQuotePayee, TPa.genNeedsQuotePayee]
    , TLT.tMemo = TTransactionMemo.genRMemo
    , TLT.pPayee =
      genMaybe $ oneof [TPa.genNoQuotePayee, TPa.genNeedsQuotePayee]
    , TLT.pNumber = genMaybe TNu.genRNumber
    , TLT.pFlag = genMaybe TFl.genRFlag
    , TLT.pAccount = oneof [TAc.genLvl1Account, TAc.genLvl2Account]
    , TLT.pTags = TTa.genRTags
    , TLT.pMemo = TPostingMemo.genRMemo
    , TLT.pCmdty = TCy.genRCmdty
    , TLT.pEntries = TB.randEntries }

-- | Generate random renderable transactions.
randomRenderable :: Gen ((Fam.Family U.TopLine U.Posting),
                         (Fam.Family M.TopLineMeta M.PostingMeta))
randomRenderable = sized $ \s -> resize (min s 6) $ do
  t <- TLT.randomUnverifiedTransactions rTransInputs
  let uPstgs = orphans t
      toMeta uPo = case U.entry uPo of
        Nothing -> M.PostingMeta <$> arbitrary <*> pure Nothing
        Just _ -> M.PostingMeta <$> arbitrary <*> (Just <$> arbitrary)
  pstgMetas <- traverse toMeta uPstgs
  topLineMeta <- arbitrary
  return (t, adopt topLineMeta pstgMetas)

newtype RUnverified =
  RUnverified ((Fam.Family U.TopLine U.Posting),
               (Fam.Family M.TopLineMeta M.PostingMeta))
  deriving (Show, Eq)
instance Arbitrary RUnverified where
  arbitrary = RUnverified <$> randomRenderable

-- | Parsing random renderable Posting gives the same thing.
prop_parseRendered ::
  M.Filename
  -> DT.DefaultTimeZone
  -> (Qt.GroupingSpec, Qt.GroupingSpec)
  -> Qt.RadGroup
  -> RUnverified
  -> Property
prop_parseRendered fn dtz gs rg (RUnverified (tFam, metaFam)) =
  case Txn.transaction tFam of
    Ex.Exception _ -> error "making transaction failed"
    Ex.Success txn ->
      let box = Boxes.transactionBox txn
                (Just (M.TransactionMeta metaFam))
      in case T.render dtz gs rg box of
        Nothing -> error "render failed"
        Just x -> let
          parser = T.transaction fn dtz rg <* Parsec.eof
          in case Parsec.parse parser "" x of
            Left e -> error $ "parse failed. error: " ++ show e
            Right box' ->
                if not $ boxesEqual box box'
                then error "boxes not equal"
                else property True

boxesEqual :: Boxes.TransactionBox -> Boxes.TransactionBox -> Bool
boxesEqual b1 b2 = let
  (t1, t2) = (Boxes.transaction b1, Boxes.transaction b2)
  in maybe False id $ do
    m1 <- M.unTransactionMeta <$> Boxes.transactionMeta b1
    m2 <- M.unTransactionMeta <$> Boxes.transactionMeta b2
    let married = marry m1 m2
        os = orphans married
        sameMeta (pm1, pm2) =
          M.postingFormat pm1 == M.postingFormat pm2
    return ((and . toList . fmap sameMeta $ os)
            && t1 == t2)

  

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered Transaction yields same Transaction"

tests :: Test
tests = testGroup "Transaction"
        [ test_parseRendered ]
