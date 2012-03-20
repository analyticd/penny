module PennyTest.Copper.Transaction where

import qualified Control.Monad.Exception.Synchronous as Ex
import Control.Applicative ((<$>), (<*>), pure, (<*))
import Data.Traversable (traverse)
import PennyTest.Copper.Util (genMaybe)
import qualified Penny.Copper.DateTime as DT
import qualified Penny.Copper.Qty as Qt
import qualified Penny.Copper.Posting as P
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
import qualified PennyTest.Lincoln.Transaction as TLT
import Test.QuickCheck (
  Arbitrary, arbitrary, Gen, oneof, sized, resize, Property,
  property)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)
import qualified Text.Parsec as Parsec
import qualified Penny.Lincoln.Transaction.Unverified as U
import Penny.Lincoln.Family (orphans, adopt)
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
randomRenderable :: Gen (Fam.Family U.TopLine P.UnverifiedWithMeta)
randomRenderable = sized $ \s -> resize (min s 3) $ do
  t <- TLT.randomUnverifiedTransactions rTransInputs
  let unverifieds = orphans t
  unvWithMetas <- traverse toUnvWithMeta unverifieds
  return (adopt (Fam.parent t) unvWithMetas)

newtype RUnverified =
  RUnverified (Fam.Family U.TopLine P.UnverifiedWithMeta)
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
prop_parseRendered fn dtz gs rg (RUnverified fam) =
  case T.render dtz gs rg fam of
    Nothing -> error "render failed"
    Just x -> let
      parser = T.transaction fn dtz rg <* Parsec.eof
      in case Parsec.parse parser "" x of
        Left _ -> error "parse failed"
        Right box -> case T.boxToUnverifiedWithMeta box of
          Ex.Exception e -> error $ e ++ " parsed: " ++ show box
          Ex.Success fam' ->
            if fam /= fam'
            then error "families not equal"
            else property True

test_parseRendered :: Test
test_parseRendered = testProperty s prop_parseRendered where
  s = "Parsing rendered Transaction yields same Transaction"

-- | Generates random metadata to accompany an unverified Posting.
toUnvWithMeta :: U.Posting -> Gen P.UnverifiedWithMeta
toUnvWithMeta (U.Posting pa nu fl ac ta en me) =
  P.UnverifiedWithMeta
  <$> pure fl
  <*> pure nu
  <*> pure pa
  <*> pure ac
  <*> pure ta
  <*> enWithMeta
  <*> pure me
  where
    enWithMeta = case en of
      Nothing -> return Nothing
      Just e -> Just <$> ((,) <$> pure e <*> arbitrary)

tests :: Test
tests = testGroup "Transaction"
        [ test_parseRendered ]
