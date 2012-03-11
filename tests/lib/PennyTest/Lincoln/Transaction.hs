module PennyTest.Lincoln.Transaction where

import Penny.Lincoln.Family (adopt)
import qualified Penny.Lincoln.Family.Family as Fam
import qualified Penny.Lincoln.Family.Siblings as Sib
import qualified Penny.Lincoln.Transaction as T
import qualified Penny.Lincoln.Transaction.Unverified as U
import qualified Penny.Lincoln.Bits as B
import qualified PennyTest.Lincoln.Bits as TB

import Control.Applicative (pure, (<$>), (<*>))
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Foldable as Foldable
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.Traversable as Tr
import qualified System.Random as R
import qualified System.Random.Shuffle as Shuf
import Test.QuickCheck (Arbitrary, arbitrary, Gen, listOf1,
                        resize)
import qualified Test.QuickCheck.Gen as G
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework (Test, testGroup)

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

{-
mkTransTester :: TransInputs -> (T.Transaction -> Bool) -> Gen Bool
mkTransTester i f = do
  top <- U.TopLine <$> dateTime i <*> tFlag i <*> tNumber i
         <*> tPayee i <*> tMemo i
  undefined
-}
-- | Generates a single unverified Posting.
makePosting ::
  GenCommon
  -> B.Account
  -> Maybe B.Entry
  -> Gen U.Posting
makePosting c ac en =
  U.Posting <$> cPayee c <*> cNumber c <*> cFlag c
  <*> pure ac <*> cTags c <*> pure en <*> cMemo c

-- | Generates a single unverified Posting with an Entry.
makePostingWithEntry ::
  GenCommon
  -> B.Account
  -> B.DrCr
  -> B.Commodity
  -> B.Qty
  -> Gen U.Posting
makePostingWithEntry c ac dc cy q = let
  am = B.Amount q cy
  en = Just $ B.Entry dc am
  in makePosting c ac en

-- | Generates a single unverified Posting with no Entry.
makePostingWithoutEntry ::
  GenCommon
  -> B.Account
  -> Gen U.Posting
makePostingWithoutEntry c ac = makePosting c ac Nothing

-- | Generates a single group of unverified Postings. Supply a
-- function to get the original and balancing quantities and DrCrs
-- from the generator. Whether the Postings are balanced or not
-- depends on the result of the function. Also returns the account
-- used to make the group of postings.
pGroup ::
  (Gen (TB.NEDrCrQty, TB.NEDrCrQty, Maybe TB.NEDrCrQty)
   -> Gen (TB.NEDrCrQty, TB.NEDrCrQty))
  -- ^ When applied to a generator, this function returns a pair p,
  -- where fst p are the original quantities and DrCrs, and snd p are
  -- the balancing quantities and DrCrs.
  -> PGroupInputs
  -> Gen (Sib.Siblings U.Posting, B.Account)
pGroup getQs p = do
  ac <- tgAccount p
  cy <- tgCommodity p
  (origs, bals) <- getQs . tgEntries $ p
  let mkP dc = makePostingWithEntry (genCommon p) ac dc cy
      mkPs (dc, qs) = Tr.traverse (mkP dc) qs
  originals <- mkPs origs
  balancers <- mkPs bals
  let (sib1:|rs1) = originals
      (sib2:|rs2) = balancers
      result = Sib.Siblings sib1 sib2 (rs1 ++ rs2)
  return (result, ac)

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced without inference.
pGroupNoInfer :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
pGroupNoInfer p = fst <$> pGroup (fmap (\(f, s, _) -> (f, s))) p

-- | Generates a single balanced group of unverified Postings. (A
-- single Transaction might consist of one or more groups). This group
-- is balanced, but the balance must be inferred.
pGroupInfer :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
pGroupInfer p = do 
  let getOrigsBals gen = do
        (o, _, mayBals) <- gen
        maybe (getOrigsBals gen) (\b -> return (o, b)) mayBals
  (sibs, ac) <- pGroup getOrigsBals p
  inferred <- makePostingWithoutEntry (genCommon p) ac
  let sibs' = Sib.Siblings (Sib.first sibs) (Sib.second sibs)
              (Sib.rest sibs ++ [inferred])
  return sibs'

-- | Makes unverified TopLines.
topLine :: TransInputs -> Gen U.TopLine
topLine t = U.TopLine
            <$> dateTime t
            <*> tFlag t
            <*> tNumber t
            <*> tPayee t
            <*> tMemo t

-- | Given a generator of unverified Postings, makes unverified
-- Transaction family.
unverifiedTransaction ::
  TransInputs
  -> (PGroupInputs -> Gen (Sib.Siblings U.Posting))
  -> Gen (Fam.Family U.TopLine U.Posting)
unverifiedTransaction t pf =
  adopt <$> topLine t <*> pf (pGroupInputs t)

-- | Makes a generator that makes multiple groups of balanced
-- unverified Postings. For a transaction to be balanced, it may
-- contain multiple groups of balanced unverified postings, but it can
-- contain only one group of inferrable postings.
multBalancedPostings :: PGroupInputs -> Gen (Sib.Siblings U.Posting)
multBalancedPostings p = toSibs <$> listOf1 (pGroupNoInfer p) where
  toSibs ls = let
    items = concat . fmap Foldable.toList $ ls
    in case items of
      (a:b:rs) -> Sib.Siblings a b rs
      _ -> error "multBalancedPostings error"

-- | Makes a generator that makes multiple groups of balanced
-- unverified Postings and that might also include inferrable
-- postings.
multBalancedPostingsWithInfer ::
  PGroupInputs
  -> Gen (Sib.Siblings U.Posting)
multBalancedPostingsWithInfer p = do
  balanced <- multBalancedPostings p
  doInferred <- arbitrary
  if doInferred
    then do
    inferred <- pGroupInfer p
    let result = Sib.Siblings (Sib.first balanced) (Sib.second balanced)
                 (Sib.rest balanced ++ Foldable.toList inferred)
    return result
    else return balanced

-- | Shuffles a bunch of Postings.
shufflePostings :: Sib.Siblings U.Posting
                   -> Gen (Sib.Siblings U.Posting)
shufflePostings s = do
  let ls = Foldable.toList s
  shuffled <- shuffle ls
  let result = case shuffled of
        a:b:rs -> Sib.Siblings a b rs
        _ -> error "shufflePostings error"
  return result

-- | Generates unverified Transactions. Some may require inference,
-- some may not. Some may have multiple groups of balanced
-- Postings. All postings within a transaction will be shuffled.
randomUnverifiedTransactions ::
  TransInputs
  -> Gen (Fam.Family U.TopLine U.Posting)
randomUnverifiedTransactions t = do
  let shuffled pg = do
        ss <- multBalancedPostingsWithInfer pg
        shufflePostings ss
  unverifiedTransaction t shuffled
  

-- | Makes unverified Transactions that will likely not be renderable.
randomUnrenderable :: Gen (Fam.Family U.TopLine U.Posting)
randomUnrenderable = randomUnverifiedTransactions t where
  t = TransInputs arbitrary arbitrary arbitrary arbitrary arbitrary 
      arbitrary arbitrary arbitrary arbitrary arbitrary arbitrary 
      arbitrary TB.randEntries

-- | Gets the StdGen out of a Gen.
qcStdGen :: Gen R.StdGen
qcStdGen = G.MkGen $ \g _ -> g

-- | Shuffles a list.
shuffle :: [a] -> Gen [a]
shuffle ls = do
  g <- qcStdGen
  return $ Shuf.shuffle' ls (length ls) g

--
-- Tests
--

-- | A random unrenderable unverified transaction makes a
-- Transaction. Resizes the generator; without doing that, stack
-- overflows will result from the ridiculous amount of data. This may
-- be a bug, as Penny should be able to handle any amount of data in
-- theory, but of course whether this is a bug depends on whether the
-- problem is with Penny or with QuickCheck.
prop_unrender :: Gen Bool
prop_unrender = resize 15 $ do
  f <- randomUnrenderable
  return $ case T.transaction f of
    Ex.Exception _ -> False
    Ex.Success _ -> True

test_unrender:: Test
test_unrender = testProperty s prop_unrender where
  s = "Random unrenderable unverified transactions make Transactions"

tests :: Test
tests = testGroup "Transaction"
        [ test_unrender ]
