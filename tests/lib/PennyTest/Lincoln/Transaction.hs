module PennyTest.Lincoln.Transaction where

import qualified Penny.Lincoln.Family.Family as Fam
import qualified Penny.Lincoln.Family.Siblings as Sib
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Bits as B
import qualified PennyTest.Lincoln.Bits as TB

import Control.Applicative (pure, (<$>), (<*>))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Text (pack)
import qualified Data.Traversable as Tr
import Test.QuickCheck as Q
import Test.QuickCheck (Arbitrary, arbitrary, Gen, suchThat)

-- | All the data needed to generate a single unverified transaction.
data TransInputs =
  TransInputs {
    dateTime :: Gen B.DateTime
    , tFlag :: Gen (Maybe B.Flag)
    , tNumber :: Gen (Maybe B.Number)
    , tPayee :: Gen (Maybe B.Payee)
    , tMemo :: Gen (Maybe B.Memo)
    , pPayee :: Gen (Maybe B.Payee)
    , pNumber :: Gen (Maybe B.Number)
    , pFlag :: Gen (Maybe B.Flag)
    , pAccount :: Gen B.Account
    , pTags :: Gen B.Tags
    , pMemo :: Gen (Maybe B.Memo)
    , pCmdty :: Gen B.Commodity
    , pEntries :: Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty) }

-- | Data needed to generate a group of unverified Postings. (A single
-- Transaction might have more than one group of unverified Postings.)
data PGroupInputs =
  PGroupInputs {
    tgPayee :: Gen (Maybe B.Payee)
    , tgNumber :: Gen (Maybe B.Number)
    , tgFlag :: Gen (Maybe B.Flag)
    , tgAccount :: Gen B.Account
    , tgTags :: Gen B.Tags
    , tgMemo :: Gen (Maybe B.Memo)
    , tgCommodity :: Gen B.Commodity
    , tgEntries :: Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty) }

-- | All Postings generated can have entirely random data for these elements.
data GenCommon = GenCommon {
  cPayee :: Gen (Maybe B.Payee)
  , cNumber :: Gen (Maybe B.Number)
  , cFlag :: Gen (Maybe B.Flag)
  , cTags :: Gen B.Tags
  , cMemo :: Gen (Maybe B.Memo) }

genCommon :: PGroupInputs -> GenCommon
genCommon t = GenCommon (tgPayee t) (tgNumber t) (tgFlag t) (tgTags t) (tgMemo t)

pGroupInputs :: TransInputs -> PGroupInputs
pGroupInputs t = PGroupInputs (pPayee t) (pNumber t) (pFlag t)
                 (pAccount t) (pTags t) (pMemo t) (pCmdty t)
                 (pEntries t)

mkTransTester :: TransInputs -> (T.Transaction -> Bool) -> Gen Bool
mkTransTester i f = do
  top <- U.TopLine <$> dateTime i <*> tFlag i <*> tNumber i
         <*> tPayee i <*> tMemo i
  undefined

-- | Generates a single unverified Posting.
makePosting ::
  GenCommon
  -> B.Account
  -> B.Commodity
  -> B.DrCr
  -> B.Qty
  -> Gen U.Posting
makePosting c ac cy dc q  = let
  am = B.Amount q cy
  en = Just $ B.Entry dc am
  in U.Posting <$> cPayee c <*> cNumber c <*> cFlag c
     <*> pure ac <*> cTags c <*> pure en <*> cMemo c

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced without inference.
pGroupNoInfer :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
pGroupNoInfer p = do
  ac <- tgAccount p
  cy <- tgCommodity p
  (origs, bals, _) <- tgEntries p
  let mkP = makePosting (genCommon p) ac cy
      mkPs (dc, qs) = Tr.traverse (mkP dc) qs
  originals <- mkPs origs
  balancers <- mkPs bals
  let (sib1:|rs1) = originals
      (sib2:|rs2) = balancers
      result = Sib.Siblings sib1 sib2 (rs1 ++ rs2)
  return result

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced, but the balance must be inferred.
pGroupInfer :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
pGroupInfer p = do
  ac <- tgAccount p
  cy <- tgCommodity p
  let getOrigsBals = do
        (o, _, mayBals) <- tgEntries p
        maybe getOrigsBals (\b -> return (o, b)) mayBals
  (origs, bals) <- getOrigsBals
  let mkP dc = makePosting (genCommon p) ac cy dc
      mkPs (dc, qs) = Tr.traverse (mkP dc) qs
  originals <- mkPs origs
  balancers <- mkPs bals
  inferred <- U.Posting <$> tgPayee p <*> tgNumber p <*> tgFlag p
              <*> pure ac <*> tgTags p <*> pure Nothing <*> tgMemo p
  let (sib1:|rs1) = originals
      (sib2:|rs2) = balancers
      result = Sib.Siblings sib1 sib2 (rs1 ++ rs2 ++ [inferred])
  return result

