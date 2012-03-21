module PennyTest.Copper.Item where

import Control.Applicative ((<$>), (<*>), pure)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Penny.Copper.Item as I
import Test.QuickCheck (Gen, arbitrary, Arbitrary, oneof)
import qualified PennyTest.Copper.Transaction as TT
import PennyTest.Lincoln.Meta ()
import qualified PennyTest.Copper.Price as TP
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Bits as B
import qualified Penny.Lincoln.Boxes as Boxes
import qualified Penny.Lincoln.Meta as M

-- | Generate renderable Items.
genRItem :: Gen I.Item
genRItem = oneof [genTrans, genPrice, genCom, genBlank]
  
-- | Generate renderable Transactions.
genTrans :: Gen I.Item
genTrans = do
  (transFam, metaFam) <- TT.randomRenderable
  txn <- case T.transaction transFam of
    Ex.Exception e ->
      error $ "genRItem: making transaction failed: " ++ show e
    Ex.Success tr -> return tr
  let meta = M.TransactionMeta metaFam
      box = Boxes.transactionBox txn (Just meta)
  return $ I.Transaction box

-- | Generate renderable Prices. BROKEN needs to account for
-- defaulttimezone. Also need to adjust transactions for DTZ.
genPrice :: Gen I.Item
genPrice = do
  ppd <- TP.genRPricePointData
  pr <- case B.newPrice (TP.from ppd)
             (TP.to ppd) (TP.countPerUnit ppd) of
          Nothing -> error $ "genRItem: making price failed"
          Just p -> return p
  let pp = B.PricePoint (TP.dateTime ppd) pr
      result = Boxes.PriceBox <$> pure pp <*> (Just <$> arbitrary)
  I.Price <$> result
  
-- | Genereate renderable Comments.
genCom :: Gen I.Item
genCom = undefined

-- | Generate blank lines.
genBlank :: Gen I.Item
genBlank = undefined
