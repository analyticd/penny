module PennyTest.Penny.Lincoln.Transaction where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (when)
import Control.Monad.Trans.Class (lift)
import qualified PennyTest.Penny.Copper.Gen.Parsers as GP
import PennyTest.Penny.Copper.Gen.Parsers (GenT)
import qualified Penny.Lincoln as L
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Test.QuickCheck.Gen as G
import Test.QuickCheck.Gen (Gen)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Test.QuickCheck.Property as P
import qualified Test.Framework.Providers.QuickCheck2 as Q
import qualified Test.Framework as TF
import qualified Data.Text as X

tests :: TF.Test
tests = TF.testGroup "PennyTest.Penny.Lincoln.Transaction"
  [ propToTest "transaction invariants hold" prop_transaction
  , propToTest "rTransaction invariants hold" prop_rTransaction
  ]

propToTest :: String -> GenT () -> TF.Test
propToTest s g = Q.testProperty s g'
  where
    g' = do
      r <- Ex.runExceptionalT g
      case r of
        Ex.Exception e -> return e
        Ex.Success () -> return P.succeeded

genMaybe :: Gen (a, b) -> GenT (Maybe a)
genMaybe g = lift $ G.oneof [return Nothing, fmap (Just . fst) g]

genNMaybe :: Gen (a, b) -> GenT (Maybe (Maybe a))
genNMaybe g = lift $ G.oneof
  [ return Nothing
   , return (Just Nothing)
   , fmap (Just . Just . fst) g ]

genTMaybe :: GenT (a, b) -> GenT (Maybe a)
genTMaybe g = do
  mkR <- lift $ G.elements [const Nothing, Just]
  a <- fmap fst g
  return $ mkR a

genTNMaybe :: GenT (a, b) -> GenT (Maybe (Maybe a))
genTNMaybe g = do
  mkR <- lift $ G.elements [ const Nothing
                           , const (Just Nothing)
                           , Just . Just ]
  a <- fmap fst g
  return $ mkR a

genTopLineChangeData :: GenT L.TopLineChangeData
genTopLineChangeData =
  L.TopLineChangeData
  <$> genTMaybe GP.dateTime
  <*> genNMaybe GP.flag
  <*> genNMaybe GP.number
  <*> genNMaybe GP.lvl1Payee
  <*> genNMaybe GP.transactionMemo
  <*> genNMaybe (genPair genTopLineLine)
  <*> genNMaybe (genPair genTopMemoLine)
  <*> genNMaybe (genPair genFilename)
  <*> genNMaybe (genPair genGlobalTransaction)
  <*> genNMaybe (genPair genFileTransaction)

genFilename :: Gen L.Filename
genFilename = fmap L.Filename genNonEmptyText

genTopLineLine :: Gen L.TopLineLine
genTopLineLine = fmap L.TopLineLine (G.choose (1, maxBound))

genTopMemoLine :: Gen L.TopMemoLine
genTopMemoLine = fmap L.TopMemoLine (G.choose (1, maxBound))

genGlobalTransaction :: Gen L.GlobalTransaction
genGlobalTransaction = fmap L.GlobalTransaction genSerial

genFileTransaction :: Gen L.FileTransaction
genFileTransaction = fmap L.FileTransaction genSerial

genNonEmptyText :: Gen X.Text
genNonEmptyText = fmap X.pack $
  G.listOf1 (G.choose (' ', '~'))


genPair :: Gen a -> Gen (a, b)
genPair ga = fmap (\a -> (a, undefined)) ga

genPostingChangeData :: GenT L.PostingChangeData
genPostingChangeData =
  L.PostingChangeData
  <$> genNMaybe GP.lvl1Payee
  <*> genNMaybe GP.number
  <*> genNMaybe GP.flag
  <*> genMaybe GP.lvl1Acct
  <*> genMaybe GP.tags
  <*> genNMaybe GP.transactionMemo
  <*> genNMaybe (genPair genSide)
  <*> genNMaybe (genPair genSpaceBetween)
  <*> genNMaybe (genPair genPostingLine)
  <*> genNMaybe (genPair genGlobalPosting)
  <*> genNMaybe (genPair genFilePosting)


genSide :: Gen L.Side
genSide = G.elements [L.CommodityOnLeft, L.CommodityOnRight]

genSpaceBetween :: Gen L.SpaceBetween
genSpaceBetween = G.elements [L.SpaceBetween, L.NoSpaceBetween]

genPostingLine :: Gen L.PostingLine
genPostingLine = fmap L.PostingLine (G.choose (1, maxBound))

genGlobalPosting :: Gen L.GlobalPosting
genGlobalPosting = fmap L.GlobalPosting genSerial

genFilePosting :: Gen L.FilePosting
genFilePosting = fmap L.FilePosting genSerial

genChangeDataFamily
  :: GenT (L.Family L.TopLineChangeData L.PostingChangeData)
genChangeDataFamily =
  L.Family
  <$> genTopLineChangeData
  <*> genPostingChangeData
  <*> genPostingChangeData
  <*> GP.listOf genPostingChangeData


checkTopLine
  :: L.TopLine
  -> L.TopLineChangeData
  -> L.TopLine
  -> GenT ()
checkTopLine t c r = do
  checkField "tcDateTime" (L.tDateTime t) (L.tcDateTime c)
    (L.tDateTime r)
  checkField "tcFlag" (L.tFlag t) (L.tcFlag c) (L.tFlag r)
  checkField "number" (L.tNumber t) (L.tcNumber c) (L.tNumber r)
  checkField "payee" (L.tPayee t) (L.tcPayee c) (L.tPayee r)
  checkField "memo" (L.tMemo t) (L.tcMemo c) (L.tMemo r)
  checkField "topLineLine" (L.tTopLineLine t) (L.tcTopLineLine c)
    (L.tTopLineLine r)
  checkField "topMemoLine" (L.tTopMemoLine t) (L.tcTopMemoLine c)
    (L.tTopMemoLine r)
  checkField "filename" (L.tFilename t) (L.tcFilename c)
    (L.tFilename r)
  checkField "globalTransaction" (L.tGlobalTransaction t)
    (L.tcGlobalTransaction c) (L.tGlobalTransaction r)
  checkField "fileTransaction" (L.tFileTransaction t)
    (L.tcFileTransaction c) (L.tFileTransaction r)

checkPosting
  :: L.Posting
  -> L.PostingChangeData
  -> L.Posting
  -> GenT ()
checkPosting p c r = do
  checkField "payee" (L.pPayee p) (L.pcPayee c) (L.pPayee r)
  checkField "number" (L.pNumber p) (L.pcNumber c) (L.pNumber r)
  checkField "flag" (L.pFlag p) (L.pcFlag c) (L.pFlag r)
  checkField "account" (L.pAccount p) (L.pcAccount c) (L.pAccount r)
  checkField "tags" (L.pTags p) (L.pcTags c) (L.pTags r)
  checkField "memo" (L.pMemo p) (L.pcMemo c) (L.pMemo r)
  checkField "side" (L.side . L.amount . L.pEntry $ p) (L.pcSide c)
    (L.side . L.amount . L.pEntry $ r)
  checkField "spaceBetween" (L.spaceBetween . L.amount . L.pEntry $ p)
    (L.pcSpaceBetween c) (L.spaceBetween . L.amount . L.pEntry $ r)
  checkField "postingLine" (L.pPostingLine p)
    (L.pcPostingLine c) (L.pPostingLine r)
  checkField "globalPosting" (L.pGlobalPosting p)
    (L.pcGlobalPosting c) (L.pGlobalPosting r)
  checkField "filePosting" (L.pFilePosting p)
    (L.pcFilePosting c) (L.pFilePosting r)


checkField
  :: (Eq a, Show a)
  => String
  -- ^ Description

  -> a
  -- ^ Original

  -> Maybe a
  -- ^ Change data

  -> a
  -- ^ Result

  -> GenT ()
checkField s o m r =
  let f txt = Ex.throwT $ P.failed { P.reason = re }
        where re = "checkField: " ++ s ++ " failure: " ++ txt
                  ++ " original: " ++ show o
                  ++ " change data: " ++ show m
                  ++ " result: " ++ show r
  in case m of
      Nothing ->
        if r == o
        then return ()
        else f "original and result should match but don't"
      Just n ->
        if n == r
        then return ()
        else f "unexpected result"

genFail :: String -> GenT ()
genFail s = Ex.throwT P.failed { P.reason = s }

prop_transaction :: GenT ()
prop_transaction = GP.maxSize 15 $ do
  orig <- fmap fst GP.transaction
  let L.Family tl p1 p2 ps = L.unTransaction orig
  cd@(L.Family ctl cp1 cp2 cps) <- genChangeDataFamily
  let r = L.changeTransaction cd orig
      L.Family rtl rp1 rp2 rps = L.unTransaction r
  when (length rps /= length ps)
    $ genFail "families are different size."
  when (not $ noCriticalChange orig r)
    $ genFail "drCr, qty, or commodity changed."
  checkTopLine tl ctl rtl
  checkPosting p1 cp1 rp1
  checkPosting p2 cp2 rp2
  fmap (const ()) . sequence $ zipWith3 checkPosting ps
             (cps ++ repeat L.emptyPostingChangeData) rps


-- | Checks to ensure that the DrCr, Qty, and Commodity have not
-- changed between two transactions.
noCriticalChange :: L.Transaction -> L.Transaction -> Bool
noCriticalChange x y =
  let L.Family _ x1 x2 xs = L.unTransaction x
      L.Family _ y1 y2 ys = L.unTransaction y
      toTrip p = ( L.drCr . L.pEntry $ p
                 , L.qty . L.amount . L.pEntry $ p
                 , L.commodity . L.amount . L.pEntry $ p )
  in map toTrip (x1:x2:xs) == map toTrip (y1:y2:ys)

-- | Generate a random Serial.
genSerial :: Gen L.Serial
genSerial = G.sized $ \s -> do
  i <- G.choose (1, max 1 s)
  G.elements (L.nSerials i)

liftPair :: Gen (a, b) -> GenT a
liftPair = fmap fst . lift

singleMaybe :: Gen a -> Gen (Maybe a)
singleMaybe g = do
  b <- G.elements [True, False]
  if b
    then fmap Just g
    else return Nothing


genRPosting :: GenT U.RPosting
genRPosting =
  U.RPosting
  <$> genMaybe GP.lvl1Payee
  <*> genMaybe GP.number
  <*> genMaybe GP.flag
  <*> liftPair GP.lvl1Acct
  <*> liftPair GP.tags
  <*> fmap fst GP.quantity
  <*> genMaybe GP.postingMemo
  <*> (lift . singleMaybe $ genPostingLine)
  <*> (lift . singleMaybe $ genGlobalPosting)
  <*> (lift . singleMaybe $ genFilePosting)

genIPosting :: GenT U.IPosting
genIPosting =
  U.IPosting
  <$> genMaybe GP.lvl1Payee
  <*> genMaybe GP.number
  <*> genMaybe GP.flag
  <*> liftPair GP.lvl1Acct
  <*> liftPair GP.tags
  <*> genMaybe GP.postingMemo
  <*> (lift . singleMaybe $ genPostingLine)
  <*> (lift . singleMaybe $ genGlobalPosting)
  <*> (lift . singleMaybe $ genFilePosting)

genRTransactionData :: GenT L.RTransaction
genRTransactionData =
  L.RTransaction
  <$> lift (fmap fst GP.lvl1Cmdty)
  <*> (lift . singleMaybe $ genSide)
  <*> (lift . singleMaybe $ genSpaceBetween)
  <*> lift (fmap fst GP.drCr)
  <*> fmap fst GP.topLine
  <*> genRPosting
  <*> GP.listOf genRPosting
  <*> genIPosting

prop_rTransaction :: GenT ()
prop_rTransaction = do
  rt <- genRTransactionData
  let txn = L.rTransaction rt
  GP.checkTransaction txn
