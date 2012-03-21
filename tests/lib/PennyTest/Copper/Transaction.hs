module PennyTest.Copper.Transaction where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ((<$>), (<*>), pure, (<*))
import Data.Foldable (toList)
import Data.Traversable (traverse)
import PennyTest.Copper.Util (genMaybe)
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Transaction as T
import qualified PennyTest.Copper.Account as TAc
import qualified PennyTest.Copper.Commodity as TCy
import qualified PennyTest.Copper.DateTime as TDT
import qualified PennyTest.Copper.Flag as TFl
import qualified PennyTest.Copper.Payees as TPa
import qualified PennyTest.Copper.Qty as TQ
import qualified PennyTest.Copper.Memos.Posting as TPostingMemo
import qualified PennyTest.Copper.Memos.Transaction as TTransactionMemo
import qualified PennyTest.Copper.Number as TNu
import PennyTest.Copper.Price (genDT)
import qualified PennyTest.Copper.Tags as TTa
import qualified PennyTest.Lincoln.Bits as TB
import PennyTest.Lincoln.Meta as TM
import PennyTest.Lincoln.Family.Family ()
import qualified PennyTest.Lincoln.Transaction as TLT
import Test.QuickCheck (
  Gen, oneof, sized, resize, Property,
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
rTransInputs :: DT.DefaultTimeZone -> TLT.TransInputs
rTransInputs dtz =
  TLT.TransInputs {
    TLT.dateTime = genDT dtz
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
randomRenderable ::
  DT.DefaultTimeZone
  -> Gen ((Fam.Family U.TopLine U.Posting),
          (Fam.Family M.TopLineMeta M.PostingMeta))
randomRenderable dtz = sized $ \s -> resize (min s 6) $ do
  t <- TLT.randomUnverifiedTransactions (rTransInputs dtz)
  let uPstgs = orphans t
      toMeta uPo = case U.entry uPo of
        Nothing -> M.PostingMeta
                   <$> genMaybe TM.genPostingLine
                   <*> pure Nothing
        Just _ -> M.PostingMeta
                  <$> genMaybe TM.genPostingLine
                  <*> (Just <$> TM.genFormat)
  pstgMetas <- traverse toMeta uPstgs
  topLineMeta <- TM.genTopLineMeta
  return (t, adopt topLineMeta pstgMetas)

-- | Parsing random renderable Posting gives the same thing.
prop_parseRendered ::
  TDT.AnyTimeZone
  -> TM.AnyFilename
  -> TQ.AnySpecPair
  -> TQ.AnyRadGroup
  -> Property
prop_parseRendered (TDT.AnyTimeZone dtz) (TM.AnyFilename fn)
  (TQ.AnySpecPair gs) (TQ.AnyRadGroup rg) = do
  (tFam, metaFam) <- randomRenderable dtz
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
